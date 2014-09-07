/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *   Mupen64plus - LLVM JIT - LLVM IR emission from Nintendo 64 code       *
 *   Mupen64Plus homepage: http://code.google.com/p/mupen64plus/           *
 *   Copyright (C) 2014 Nebuleon                                           *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.          *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include <assert.h>
#include <stdint.h> /* to get [u]intN_t in the global namespace */
#ifdef LJ_SHOW_COMPILATION
#  include <stdio.h>
#endif

#include "llvm/IR/IRBuilder.h" /* IRBuilder<> */
#include "llvm/IR/Module.h" /* complete Function type */

#include "llvm_bridge.h"
#include "n64ops.h"

#include "../r4300.h" /* get declaration for PC, reg */

struct value_store {
	llvm::Value* n64_int[32]; // Last LLVM Value held by each N64 GPR.
	uint32_t n64_int_dirty; // Bit n (0 = LSB) is set if register n is dirty.
};

#define FAIL_IF(cond) do { if (cond) { return false; } } while (0)
#define NULL_IF(cond) do { if (cond) { return NULL; } } while (0)

bool set_pc(llvm::IRBuilder<>& builder, precomp_instr* new_pc)
{
	llvm::Value* PC_ptr = llvm::Constant::getIntegerValue(
		/* (1) Type: (native pointer-sized integer)* */
		llvm::PointerType::getUnqual(llvm::Type::getIntNTy(*context, sizeof(precomp_instr**) * 8)),
		/* (2) Value: (size: native pointer-sized integer, value: &PC) */
		llvm::APInt(sizeof(precomp_instr**) * 8, (uintptr_t) &PC)
	);
	FAIL_IF(!PC_ptr);
	llvm::Value* new_pc_constant = llvm::Constant::getIntegerValue(
		/* (1) Type: i32 */
		llvm::Type::getIntNTy(*context, sizeof(precomp_instr*) * 8),
		/* (2) Value: (size: 4, value: new_pc) */
		llvm::APInt(sizeof(precomp_instr*) * 8, (uintptr_t) new_pc)
	);
	FAIL_IF(!new_pc_constant);
	FAIL_IF(!builder.CreateStore(new_pc_constant, PC_ptr));
	return true;
}

llvm::Value* load_n64_int64(llvm::IRBuilder<>& builder, value_store& values, uint8_t src_reg)
{
	if (src_reg == 0)
		return llvm::Constant::getIntegerValue(
			llvm::Type::getInt64Ty(*context), llvm::APInt(64, 0)
		);

	if (values.n64_int[src_reg])
		return values.n64_int[src_reg];

	llvm::Value* regs_ptr = llvm::Constant::getIntegerValue(
		/* (1) Type: [32 x i64]* */
		llvm::PointerType::getUnqual(llvm::ArrayType::get(llvm::Type::getInt64Ty(*context), 32)),
		/* (2) Value: (size: native pointer-sized integer, value: &reg[0]) */
		llvm::APInt(sizeof(uint64_t*) * 8, (uintptr_t) reg)
	);
	NULL_IF(!regs_ptr);
	/* Generate the address of &reg[src_ptr], type i64. */
	llvm::Value* src_reg_ptr = builder.CreateConstInBoundsGEP2_32(regs_ptr, 0, src_reg);
	NULL_IF(!src_reg_ptr);
	/* Load the 64-bit register. */
	llvm::Value* result = builder.CreateLoad(src_reg_ptr);
	NULL_IF(!result);

	values.n64_int[src_reg] = result;
	return result;
}

llvm::Value* load_n64_int32(llvm::IRBuilder<>& builder, value_store& values, uint8_t src_reg)
{
	if (src_reg == 0)
		return llvm::Constant::getIntegerValue(
			llvm::Type::getInt32Ty(*context), llvm::APInt(32, 0)
		);

	llvm::Value* result64 = load_n64_int64(builder, values, src_reg);
	NULL_IF(!result64);
	return builder.CreateTrunc(result64, llvm::Type::getInt32Ty(*context));
}

void queue_n64_int64(value_store& values, uint8_t dst_reg, llvm::Value* value)
{
	values.n64_int[dst_reg] = value;
	values.n64_int_dirty |= (UINT32_C(1) << dst_reg);
}

bool queue_n64_int32(llvm::IRBuilder<>& builder, value_store& values, uint8_t dst_reg, llvm::Value* value)
{
	llvm::Value* value64 = builder.CreateSExt(value, llvm::Type::getInt64Ty(*context));
	FAIL_IF(!value64);
	values.n64_int[dst_reg] = value64;
	values.n64_int_dirty |= (UINT32_C(1) << dst_reg);
	return true;
}

bool store_queued_regs(llvm::IRBuilder<>& builder, value_store& values)
{
	for (int i = 1; i < 32; i++) {
		if (values.n64_int_dirty & (UINT32_C(1) << i)) {
			llvm::Value* regs_ptr = llvm::Constant::getIntegerValue(
				/* (1) Type: [32 x i64]* */
				llvm::PointerType::getUnqual(llvm::ArrayType::get(llvm::Type::getInt64Ty(*context), 32)),
				/* (2) Value: (size: native pointer-sized integer, value: &reg[0]) */
				llvm::APInt(sizeof(uint64_t*) * 8, (uintptr_t) reg)
			);
			FAIL_IF(!regs_ptr);
			/* Generate the address of &reg[i], type i64. */
			llvm::Value* dst_reg_ptr = builder.CreateConstInBoundsGEP2_32(regs_ptr, 0, i);
			FAIL_IF(!dst_reg_ptr);
			/* Store the 64-bit register. */
			FAIL_IF(!builder.CreateStore(values.n64_int[i], dst_reg_ptr));
		}
		values.n64_int[i] = NULL;
	}
	values.n64_int_dirty = 0;
	return true;
}

bool llvm_ir_for_insn(llvm::IRBuilder<>& builder, value_store& values, const n64_insn_t* n64_insn)
{
	switch (n64_insn->opcode) {
		case N64_OP_NOP:
		case N64_OP_CACHE:
		case N64_OP_SYNC:
			break;

		case N64_OP_SLL:
		{
			llvm::Value* a = load_n64_int32(builder, values, n64_insn->rt);
			llvm::Value* sa = llvm::Constant::getIntegerValue(
				llvm::Type::getInt32Ty(*context), llvm::APInt(32, n64_insn->imm)
			);
			FAIL_IF(!a || !sa);
			llvm::Value* result32 = builder.CreateShl(a, sa);
			FAIL_IF(!result32);
			FAIL_IF(!queue_n64_int32(builder, values, n64_insn->rd, result32));
			break;
		}

		case N64_OP_SRL:
		{
			llvm::Value* a = load_n64_int32(builder, values, n64_insn->rt);
			llvm::Value* sa = llvm::Constant::getIntegerValue(
				llvm::Type::getInt32Ty(*context), llvm::APInt(32, n64_insn->imm)
			);
			FAIL_IF(!a || !sa);
			llvm::Value* result32 = builder.CreateLShr(a, sa);
			FAIL_IF(!result32);
			FAIL_IF(!queue_n64_int32(builder, values, n64_insn->rd, result32));
			break;
		}

		case N64_OP_SRA:
		{
			llvm::Value* a = load_n64_int32(builder, values, n64_insn->rt);
			llvm::Value* sa = llvm::Constant::getIntegerValue(
				llvm::Type::getInt32Ty(*context), llvm::APInt(32, n64_insn->imm)
			);
			FAIL_IF(!a || !sa);
			llvm::Value* result32 = builder.CreateAShr(a, sa);
			FAIL_IF(!result32);
			FAIL_IF(!queue_n64_int32(builder, values, n64_insn->rd, result32));
			break;
		}

		case N64_OP_DSLL:
		{
			llvm::Value* a = load_n64_int64(builder, values, n64_insn->rt);
			llvm::Value* sa = llvm::Constant::getIntegerValue(
				llvm::Type::getInt32Ty(*context), llvm::APInt(64, n64_insn->imm)
			);
			FAIL_IF(!a || !sa);
			llvm::Value* result64 = builder.CreateShl(a, sa);
			FAIL_IF(!result64);
			queue_n64_int64(values, n64_insn->rd, result64);
			break;
		}

		case N64_OP_DSRL:
		{
			llvm::Value* a = load_n64_int64(builder, values, n64_insn->rt);
			llvm::Value* sa = llvm::Constant::getIntegerValue(
				llvm::Type::getInt32Ty(*context), llvm::APInt(64, n64_insn->imm)
			);
			FAIL_IF(!a || !sa);
			llvm::Value* result64 = builder.CreateLShr(a, sa);
			FAIL_IF(!result64);
			queue_n64_int64(values, n64_insn->rd, result64);
			break;
		}

		case N64_OP_DSRA:
		{
			llvm::Value* a = load_n64_int64(builder, values, n64_insn->rt);
			llvm::Value* sa = llvm::Constant::getIntegerValue(
				llvm::Type::getInt32Ty(*context), llvm::APInt(64, n64_insn->imm)
			);
			FAIL_IF(!a || !sa);
			llvm::Value* result64 = builder.CreateAShr(a, sa);
			FAIL_IF(!result64);
			queue_n64_int64(values, n64_insn->rd, result64);
			break;
		}

		case N64_OP_DSLL32:
		{
			llvm::Value* a = load_n64_int64(builder, values, n64_insn->rt);
			llvm::Value* sa = llvm::Constant::getIntegerValue(
				llvm::Type::getInt32Ty(*context), llvm::APInt(64, 32 + n64_insn->imm)
			);
			FAIL_IF(!a || !sa);
			llvm::Value* result64 = builder.CreateShl(a, sa);
			FAIL_IF(!result64);
			queue_n64_int64(values, n64_insn->rd, result64);
			break;
		}

		case N64_OP_DSRL32:
		{
			llvm::Value* a = load_n64_int64(builder, values, n64_insn->rt);
			llvm::Value* sa = llvm::Constant::getIntegerValue(
				llvm::Type::getInt32Ty(*context), llvm::APInt(64, 32 + n64_insn->imm)
			);
			FAIL_IF(!a || !sa);
			llvm::Value* result64 = builder.CreateLShr(a, sa);
			FAIL_IF(!result64);
			queue_n64_int64(values, n64_insn->rd, result64);
			break;
		}

		case N64_OP_DSRA32:
		{
			llvm::Value* a = load_n64_int64(builder, values, n64_insn->rt);
			llvm::Value* sa = llvm::Constant::getIntegerValue(
				llvm::Type::getInt32Ty(*context), llvm::APInt(64, 32 + n64_insn->imm)
			);
			FAIL_IF(!a || !sa);
			llvm::Value* result64 = builder.CreateAShr(a, sa);
			FAIL_IF(!result64);
			queue_n64_int64(values, n64_insn->rd, result64);
			break;
		}

		default:
			assert(false && "Opcode without an implementation requested in llvm_ir_for_insn");
			break;
	}
	return true;
}

bool llvm_ir_for_n64(void* fn_ptr, const n64_insn_t* n64_insns, uint32_t n64_insn_count)
{
	llvm::Function* fn = (llvm::Function*) fn_ptr;
	llvm::IRBuilder<> builder(*context);
	value_store values = { };
	builder.SetInsertPoint(&fn->getEntryBlock());

	for (unsigned int i = 0; i < n64_insn_count; i++) {
#ifdef LJ_SHOW_COMPILATION
		printf(" %s", get_n64_op_name(n64_insns[i].opcode));
#endif
		FAIL_IF(!llvm_ir_for_insn(builder, values, &n64_insns[i]));
	}
#ifdef LJ_SHOW_COMPILATION
	printf("\n");
#endif

	FAIL_IF(!store_queued_regs(builder, values));
	FAIL_IF(!set_pc(builder, n64_insns[n64_insn_count - 1].runtime + 1));

	FAIL_IF(!builder.CreateRetVoid());
	return true;
}
