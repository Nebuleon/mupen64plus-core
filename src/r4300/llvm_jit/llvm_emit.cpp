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

struct value_store {
	llvm::Value* n64_int[32]; // Last LLVM Value held by each N64 GPR.
	uint32_t n64_int_dirty; // Bit n (0 = LSB) is set if register n is dirty.
	llvm::Value* n64_hi; // Last LLVM Value held by the multiplier unit's HI.
	bool n64_hi_dirty; // true if HI is dirty.
	llvm::Value* n64_lo; // Last LLVM Value held by the multiplier unit's LO.
	bool n64_lo_dirty; // true if LO is dirty.
};

#define FAIL_IF(cond) do { if (cond) { return false; } } while (0)
#define NULL_IF(cond) do { if (cond) { return NULL; } } while (0)

bool set_pc(llvm::IRBuilder<>& builder, precomp_instr* new_pc)
{
	llvm::Value* new_pc_constant = llvm::Constant::getIntegerValue(
		/* (1) Type: i8* */
		llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(*context)),
		/* (2) Value: (size: native pointer-sized integer, value: new_pc) */
		llvm::APInt(sizeof(precomp_instr*) * 8, (uintptr_t) new_pc)
	);
	FAIL_IF(!new_pc_constant);
	FAIL_IF(!builder.CreateStore(new_pc_constant, llvm_PC));
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

	/* Generate the address of &reg[src_ptr], type i64. */
	llvm::Value* src_reg_ptr = builder.CreateConstInBoundsGEP2_32(llvm_reg, 0, src_reg);
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

llvm::Value* load_n64_hi(llvm::IRBuilder<>& builder, value_store& values)
{
	if (values.n64_hi)
		return values.n64_hi;

	/* Load the 64-bit register. */
	llvm::Value* result = builder.CreateLoad(llvm_hi);
	NULL_IF(!result);

	values.n64_hi = result;
	return result;
}

llvm::Value* load_n64_lo(llvm::IRBuilder<>& builder, value_store& values)
{
	if (values.n64_lo)
		return values.n64_lo;

	/* Load the 64-bit register. */
	llvm::Value* result = builder.CreateLoad(llvm_lo);
	NULL_IF(!result);

	values.n64_lo = result;
	return result;
}

void queue_n64_hi(value_store& values, llvm::Value* value)
{
	values.n64_hi = value;
	values.n64_hi_dirty = true;
}

void queue_n64_lo(value_store& values, llvm::Value* value)
{
	values.n64_lo = value;
	values.n64_lo_dirty = true;
}

bool store_queued_regs(llvm::IRBuilder<>& builder, value_store& values)
{
	for (int i = 1; i < 32; i++) {
		if (values.n64_int_dirty & (UINT32_C(1) << i)) {
			/* Generate the address of &reg[i], type i64. */
			llvm::Value* dst_reg_ptr = builder.CreateConstInBoundsGEP2_32(llvm_reg, 0, i);
			FAIL_IF(!dst_reg_ptr);
			/* Store the 64-bit register. */
			FAIL_IF(!builder.CreateStore(values.n64_int[i], dst_reg_ptr));
		}
		values.n64_int[i] = NULL;
	}
	values.n64_int_dirty = 0;

	if (values.n64_hi_dirty) {
		/* Store the 64-bit register. */
		FAIL_IF(!builder.CreateStore(values.n64_hi, llvm_hi));
	}

	if (values.n64_lo_dirty) {
		FAIL_IF(!builder.CreateStore(values.n64_lo, llvm_lo));
	}
	values.n64_hi = values.n64_lo = NULL;
	values.n64_hi_dirty = values.n64_lo_dirty = false;

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

		case N64_OP_SLLV:
		{
			llvm::Value* a = load_n64_int32(builder, values, n64_insn->rt);
			llvm::Value* b = load_n64_int32(builder, values, n64_insn->rs);
			// The shift amount must not be greater than 31 in LLVM.
			// The MIPS specification states that only the 5 least significant
			// bits contribute to the shift amount (so the mask is 31).
			llvm::Value* const31 = llvm::Constant::getIntegerValue(
				llvm::Type::getInt32Ty(*context), llvm::APInt(32, 31)
			);
			FAIL_IF(!a || !b || !const31);
			llvm::Value* sa = builder.CreateAnd(b, const31);
			llvm::Value* result32 = builder.CreateShl(a, sa);
			FAIL_IF(!result32);
			FAIL_IF(!queue_n64_int32(builder, values, n64_insn->rd, result32));
			break;
		}

		case N64_OP_SRLV:
		{
			llvm::Value* a = load_n64_int32(builder, values, n64_insn->rt);
			llvm::Value* b = load_n64_int32(builder, values, n64_insn->rs);
			// The shift amount must not be greater than 31 in LLVM.
			// The MIPS specification states that only the 5 least significant
			// bits contribute to the shift amount (so the mask is 31).
			llvm::Value* const31 = llvm::Constant::getIntegerValue(
				llvm::Type::getInt32Ty(*context), llvm::APInt(32, 31)
			);
			FAIL_IF(!a || !b || !const31);
			llvm::Value* sa = builder.CreateAnd(b, const31);
			llvm::Value* result32 = builder.CreateLShr(a, sa);
			FAIL_IF(!result32);
			FAIL_IF(!queue_n64_int32(builder, values, n64_insn->rd, result32));
			break;
		}

		case N64_OP_SRAV:
		{
			llvm::Value* a = load_n64_int32(builder, values, n64_insn->rt);
			llvm::Value* b = load_n64_int32(builder, values, n64_insn->rs);
			// The shift amount must not be greater than 31 in LLVM.
			// The MIPS specification states that only the 5 least significant
			// bits contribute to the shift amount (so the mask is 31).
			llvm::Value* const31 = llvm::Constant::getIntegerValue(
				llvm::Type::getInt32Ty(*context), llvm::APInt(32, 31)
			);
			FAIL_IF(!a || !b || !const31);
			llvm::Value* sa = builder.CreateAnd(b, const31);
			llvm::Value* result32 = builder.CreateAShr(a, sa);
			FAIL_IF(!result32);
			FAIL_IF(!queue_n64_int32(builder, values, n64_insn->rd, result32));
			break;
		}

		case N64_OP_DSLLV:
		{
			llvm::Value* a = load_n64_int64(builder, values, n64_insn->rt);
			llvm::Value* b = load_n64_int64(builder, values, n64_insn->rs);
			// The shift amount must not be greater than 63 in LLVM.
			// The MIPS specification states that only the 6 least significant
			// bits contribute to the shift amount (so the mask is 63).
			llvm::Value* const63 = llvm::Constant::getIntegerValue(
				llvm::Type::getInt64Ty(*context), llvm::APInt(64, 63)
			);
			FAIL_IF(!a || !b || !const63);
			llvm::Value* sa = builder.CreateAnd(b, const63);
			llvm::Value* result64 = builder.CreateShl(a, sa);
			FAIL_IF(!result64);
			queue_n64_int64(values, n64_insn->rd, result64);
			break;
		}

		case N64_OP_DSRLV:
		{
			llvm::Value* a = load_n64_int64(builder, values, n64_insn->rt);
			llvm::Value* b = load_n64_int64(builder, values, n64_insn->rs);
			// The shift amount must not be greater than 63 in LLVM.
			// The MIPS specification states that only the 6 least significant
			// bits contribute to the shift amount (so the mask is 63).
			llvm::Value* const63 = llvm::Constant::getIntegerValue(
				llvm::Type::getInt64Ty(*context), llvm::APInt(64, 63)
			);
			FAIL_IF(!a || !b || !const63);
			llvm::Value* sa = builder.CreateAnd(b, const63);
			llvm::Value* result64 = builder.CreateLShr(a, sa);
			FAIL_IF(!result64);
			queue_n64_int64(values, n64_insn->rd, result64);
			break;
		}

		case N64_OP_DSRAV:
		{
			llvm::Value* a = load_n64_int64(builder, values, n64_insn->rt);
			llvm::Value* b = load_n64_int64(builder, values, n64_insn->rs);
			// The shift amount must not be greater than 63 in LLVM.
			// The MIPS specification states that only the 6 least significant
			// bits contribute to the shift amount (so the mask is 63).
			llvm::Value* const63 = llvm::Constant::getIntegerValue(
				llvm::Type::getInt64Ty(*context), llvm::APInt(64, 63)
			);
			FAIL_IF(!a || !b || !const63);
			llvm::Value* sa = builder.CreateAnd(b, const63);
			llvm::Value* result64 = builder.CreateAShr(a, sa);
			FAIL_IF(!result64);
			queue_n64_int64(values, n64_insn->rd, result64);
			break;
		}

		case N64_OP_ADD:
		{
			llvm::Value* a = load_n64_int32(builder, values, n64_insn->rs);
			llvm::Value* b = load_n64_int32(builder, values, n64_insn->rt);
			FAIL_IF(!a || !b);
			llvm::Value* result32 = builder.CreateAdd(a, b);
			FAIL_IF(!result32);
			FAIL_IF(!queue_n64_int32(builder, values, n64_insn->rd, result32));
			break;
		}

		case N64_OP_ADDU:
		{
			if (n64_insn->rt == 0) {
				// ADDU Rd, Rs, $0 moves a 32-bit value with sign-extension.
				llvm::Value* a = load_n64_int32(builder, values, n64_insn->rs);
				FAIL_IF(!a);
				FAIL_IF(!queue_n64_int32(builder, values, n64_insn->rd, a));
			} else {
				llvm::Value* a = load_n64_int32(builder, values, n64_insn->rs);
				llvm::Value* b = load_n64_int32(builder, values, n64_insn->rt);
				FAIL_IF(!a || !b);
				llvm::Value* result32 = builder.CreateAdd(a, b);
				FAIL_IF(!result32);
				FAIL_IF(!queue_n64_int32(builder, values, n64_insn->rd, result32));
			}
			break;
		}

		case N64_OP_SUB:
		case N64_OP_SUBU:
		{
			llvm::Value* a = load_n64_int32(builder, values, n64_insn->rs);
			llvm::Value* b = load_n64_int32(builder, values, n64_insn->rt);
			FAIL_IF(!a || !b);
			llvm::Value* result32 = builder.CreateSub(a, b);
			FAIL_IF(!result32);
			FAIL_IF(!queue_n64_int32(builder, values, n64_insn->rd, result32));
			break;
		}

		case N64_OP_AND:
		{
			llvm::Value* a = load_n64_int64(builder, values, n64_insn->rs);
			llvm::Value* b = load_n64_int64(builder, values, n64_insn->rt);
			FAIL_IF(!a || !b);
			llvm::Value* result64 = builder.CreateAnd(a, b);
			FAIL_IF(!result64);
			queue_n64_int64(values, n64_insn->rd, result64);
			break;
		}

		case N64_OP_OR:
		{
			if (n64_insn->rt == 0) {
				// OR Rd, Rs, $0 moves a 64-bit value. (Rs == $0 stores 0.)
				llvm::Value* a = load_n64_int64(builder, values, n64_insn->rs);
				FAIL_IF(!a);
				queue_n64_int64(values, n64_insn->rd, a);
			} else {
				llvm::Value* a = load_n64_int64(builder, values, n64_insn->rs);
				llvm::Value* b = load_n64_int64(builder, values, n64_insn->rt);
				FAIL_IF(!a || !b);
				llvm::Value* result64 = builder.CreateOr(a, b);
				FAIL_IF(!result64);
				queue_n64_int64(values, n64_insn->rd, result64);
			}
			break;
		}

		case N64_OP_XOR:
		{
			llvm::Value* a = load_n64_int64(builder, values, n64_insn->rs);
			llvm::Value* b = load_n64_int64(builder, values, n64_insn->rt);
			FAIL_IF(!a || !b);
			llvm::Value* result64 = builder.CreateXor(a, b);
			FAIL_IF(!result64);
			queue_n64_int64(values, n64_insn->rd, result64);
			break;
		}

		case N64_OP_NOR:
		{
			if (n64_insn->rs == 0 && n64_insn->rt == 0) {
				// NOR Rd, $0, $0 loads a register with -1.
				llvm::Value* allOnes =
					llvm::Constant::getAllOnesValue(llvm::Type::getInt64Ty(*context));
				queue_n64_int64(values, n64_insn->rd, allOnes);
			} else if (n64_insn->rt == 0) {
				// NOR Rd, Rs, $0 is equivalent to "NOT" Rd, Rs.
				llvm::Value* a = load_n64_int64(builder, values, n64_insn->rs);
				FAIL_IF(!a);
				// NOT is implemented in LLVM as (a ^ all-ones).
				llvm::Value* result64 = builder.CreateXor(a,
					llvm::Constant::getAllOnesValue(llvm::Type::getInt64Ty(*context)));
				FAIL_IF(!result64);
				queue_n64_int64(values, n64_insn->rd, result64);
			} else {
				llvm::Value* a = load_n64_int64(builder, values, n64_insn->rs);
				llvm::Value* b = load_n64_int64(builder, values, n64_insn->rt);
				FAIL_IF(!a || !b);
				// NOR is implemented in LLVM as (~(a | b)),
				// which in turn is ((a | b) ^ all-ones).
				llvm::Value* or64 = builder.CreateOr(a, b);
				FAIL_IF(!or64);
				llvm::Value* result64 = builder.CreateXor(or64,
					llvm::Constant::getAllOnesValue(llvm::Type::getInt64Ty(*context)));
				FAIL_IF(!result64);
				queue_n64_int64(values, n64_insn->rd, result64);
			}
			break;
		}

		case N64_OP_SLT:
		{
			llvm::Value* a = load_n64_int64(builder, values, n64_insn->rs);
			llvm::Value* b = load_n64_int64(builder, values, n64_insn->rt);
			FAIL_IF(!a || !b);
			llvm::Value* cmp_bit = builder.CreateICmpSLT(a, b);
			FAIL_IF(!cmp_bit);
			llvm::Value* result64 = builder.CreateZExt(cmp_bit,
				llvm::Type::getInt64Ty(*context));
			FAIL_IF(!result64);
			queue_n64_int64(values, n64_insn->rd, result64);
			break;
		}

		case N64_OP_SLTU:
		{
			llvm::Value* a = load_n64_int64(builder, values, n64_insn->rs);
			llvm::Value* b = load_n64_int64(builder, values, n64_insn->rt);
			FAIL_IF(!a || !b);
			llvm::Value* cmp_bit = builder.CreateICmpULT(a, b);
			FAIL_IF(!cmp_bit);
			llvm::Value* result64 = builder.CreateZExt(cmp_bit,
				llvm::Type::getInt64Ty(*context));
			FAIL_IF(!result64);
			queue_n64_int64(values, n64_insn->rd, result64);
			break;
		}

		case N64_OP_DADD:
		case N64_OP_DADDU:
		{
			llvm::Value* a = load_n64_int64(builder, values, n64_insn->rs);
			llvm::Value* b = load_n64_int64(builder, values, n64_insn->rt);
			FAIL_IF(!a || !b);
			llvm::Value* result64 = builder.CreateAdd(a, b);
			FAIL_IF(!result64);
			queue_n64_int64(values, n64_insn->rd, result64);
			break;
		}

		case N64_OP_DSUB:
		case N64_OP_DSUBU:
		{
			llvm::Value* a = load_n64_int64(builder, values, n64_insn->rs);
			llvm::Value* b = load_n64_int64(builder, values, n64_insn->rt);
			FAIL_IF(!a || !b);
			llvm::Value* result64 = builder.CreateSub(a, b);
			FAIL_IF(!result64);
			queue_n64_int64(values, n64_insn->rd, result64);
			break;
		}

		case N64_OP_ADDI:
		case N64_OP_ADDIU:
		{
			llvm::Value* a = load_n64_int32(builder, values, n64_insn->rs);
			llvm::Value* b = llvm::Constant::getIntegerValue(
				llvm::Type::getInt32Ty(*context), llvm::APInt(32, n64_insn->imm)
			);
			FAIL_IF(!a || !b);
			llvm::Value* result32 = builder.CreateAdd(a, b);
			FAIL_IF(!result32);
			FAIL_IF(!queue_n64_int32(builder, values, n64_insn->rt, result32));
			break;
		}

		case N64_OP_SLTI:
		{
			llvm::Value* a = load_n64_int64(builder, values, n64_insn->rs);
			llvm::Value* b = llvm::Constant::getIntegerValue(
				llvm::Type::getInt64Ty(*context), llvm::APInt(64, n64_insn->imm)
			);
			FAIL_IF(!a || !b);
			llvm::Value* cmp_bit = builder.CreateICmpSLT(a, b);
			FAIL_IF(!cmp_bit);
			llvm::Value* result64 = builder.CreateZExt(cmp_bit,
				llvm::Type::getInt64Ty(*context));
			FAIL_IF(!result64);
			queue_n64_int64(values, n64_insn->rt, result64);
			break;
		}

		case N64_OP_SLTIU:
		{
			llvm::Value* a = load_n64_int64(builder, values, n64_insn->rs);
			llvm::Value* b = llvm::Constant::getIntegerValue(
				llvm::Type::getInt64Ty(*context), llvm::APInt(64, n64_insn->imm)
			);
			FAIL_IF(!a || !b);
			llvm::Value* cmp_bit = builder.CreateICmpULT(a, b);
			FAIL_IF(!cmp_bit);
			llvm::Value* result64 = builder.CreateZExt(cmp_bit,
				llvm::Type::getInt64Ty(*context));
			FAIL_IF(!result64);
			queue_n64_int64(values, n64_insn->rt, result64);
			break;
		}

		case N64_OP_ANDI:
		{
			llvm::Value* a64 = load_n64_int64(builder, values, n64_insn->rs);
			FAIL_IF(!a64);
			llvm::Value* a = builder.CreateTrunc(a64, llvm::Type::getInt16Ty(*context));
			llvm::Value* b = llvm::Constant::getIntegerValue(
				llvm::Type::getInt16Ty(*context), llvm::APInt(16, n64_insn->imm)
			);
			FAIL_IF(!a || !b);
			llvm::Value* result16 = builder.CreateAnd(a, b);
			FAIL_IF(!result16);
			llvm::Value* result64 = builder.CreateZExt(result16,
				llvm::Type::getInt64Ty(*context));
			FAIL_IF(!result64);
			queue_n64_int64(values, n64_insn->rt, result64);
			break;
		}

		case N64_OP_ORI:
		{
			llvm::Value* a = load_n64_int64(builder, values, n64_insn->rs);
			llvm::Value* b = llvm::Constant::getIntegerValue(
				llvm::Type::getInt64Ty(*context), llvm::APInt(64, n64_insn->imm)
			);
			FAIL_IF(!a || !b);
			llvm::Value* result64 = builder.CreateOr(a, b);
			FAIL_IF(!result64);
			queue_n64_int64(values, n64_insn->rt, result64);
			break;
		}

		case N64_OP_XORI:
		{
			llvm::Value* a = load_n64_int64(builder, values, n64_insn->rs);
			llvm::Value* b = llvm::Constant::getIntegerValue(
				llvm::Type::getInt64Ty(*context), llvm::APInt(64, n64_insn->imm)
			);
			FAIL_IF(!a || !b);
			llvm::Value* result64 = builder.CreateXor(a, b);
			FAIL_IF(!result64);
			queue_n64_int64(values, n64_insn->rt, result64);
			break;
		}

		case N64_OP_LUI:
		{
			llvm::Value* a = llvm::Constant::getIntegerValue(
				llvm::Type::getInt64Ty(*context), llvm::APInt(64, (int64_t) (n64_insn->imm << 16))
			);
			FAIL_IF(!a);
			queue_n64_int64(values, n64_insn->rt, a);
			break;
		}

		case N64_OP_DADDI:
		case N64_OP_DADDIU:
		{
			llvm::Value* a = load_n64_int64(builder, values, n64_insn->rs);
			llvm::Value* b = llvm::Constant::getIntegerValue(
				llvm::Type::getInt64Ty(*context), llvm::APInt(64, n64_insn->imm)
			);
			FAIL_IF(!a || !b);
			llvm::Value* result64 = builder.CreateAdd(a, b);
			FAIL_IF(!result64);
			queue_n64_int64(values, n64_insn->rt, result64);
			break;
		}

		/* ... */

		case N64_OP_MFHI:
		{
			llvm::Value* hi = load_n64_hi(builder, values);
			FAIL_IF(!hi);
			queue_n64_int64(values, n64_insn->rd, hi);
			break;
		}

		case N64_OP_MFLO:
		{
			llvm::Value* lo = load_n64_lo(builder, values);
			FAIL_IF(!lo);
			queue_n64_int64(values, n64_insn->rd, lo);
			break;
		}

		case N64_OP_MTHI:
		{
			llvm::Value* a = load_n64_int64(builder, values, n64_insn->rs);
			FAIL_IF(!a);
			queue_n64_hi(values, a);
			break;
		}

		case N64_OP_MTLO:
		{
			llvm::Value* a = load_n64_int64(builder, values, n64_insn->rs);
			FAIL_IF(!a);
			queue_n64_lo(values, a);
			break;
		}

		case N64_OP_MULT:
		{
			// This is a 32-bit by 32-bit signed multiplication with 64-bit
			// result. The upper and lower 32 bits go into HI and LO, both
			// sign-extended to 64 bits independently from each other.
			llvm::Value* a32 = load_n64_int32(builder, values, n64_insn->rs);
			llvm::Value* b32 = load_n64_int32(builder, values, n64_insn->rt);
			FAIL_IF(!a32 || !b32);
			// In LLVM, 64-bit operands are required in order to get a 64-bit
			// result. So we sign-extend the 32-bit truncated operands back to
			// 64-bit.
			llvm::Value* a64 = builder.CreateSExt(a32,
				llvm::Type::getInt64Ty(*context));
			llvm::Value* b64 = builder.CreateSExt(b32,
				llvm::Type::getInt64Ty(*context));
			FAIL_IF(!a64 || !b64);
			// We know the result will not overflow in any way.
			llvm::Value* result64 = builder.CreateMul(a64, b64, "",
				true /* nuw (no unsigned wrap) */, true /* nsw (signed) */);
			// Split the result.
			FAIL_IF(!result64);
			llvm::Value* const32 = llvm::Constant::getIntegerValue(
				llvm::Type::getInt64Ty(*context), llvm::APInt(64, 32)
			);
			FAIL_IF(!const32);
			// HI: Arithmetic shift right by 32 performs sign-extension.
			llvm::Value* hi64 = builder.CreateAShr(result64, const32);
			// LO: Truncate to 32 bits then sign-extend the result.
			llvm::Value* lo32 = builder.CreateTrunc(result64,
				llvm::Type::getInt32Ty(*context));
			FAIL_IF(!lo32);
			llvm::Value* lo64 = builder.CreateSExt(lo32,
				llvm::Type::getInt64Ty(*context));
			FAIL_IF(!hi64 || !lo64);
			queue_n64_hi(values, hi64);
			queue_n64_lo(values, lo64);
			break;
		}

		case N64_OP_MULTU:
		{
			// This is a 32-bit by 32-bit unsigned multiplication with 64-bit
			// result. The upper and lower 32 bits go into HI and LO, both
			// sign-extended to 64 bits independently from each other.
			llvm::Value* a32 = load_n64_int32(builder, values, n64_insn->rs);
			llvm::Value* b32 = load_n64_int32(builder, values, n64_insn->rt);
			FAIL_IF(!a32 || !b32);
			// In LLVM, 64-bit operands are required in order to get a 64-bit
			// result. So we zero-extend the 32-bit truncated operands back to
			// 64-bit.
			llvm::Value* a64 = builder.CreateZExt(a32,
				llvm::Type::getInt64Ty(*context));
			llvm::Value* b64 = builder.CreateZExt(b32,
				llvm::Type::getInt64Ty(*context));
			FAIL_IF(!a64 || !b64);
			// We know the result will not overflow in any way.
			llvm::Value* result64 = builder.CreateMul(a64, b64, "",
				true /* nuw (no unsigned wrap) */, true /* nsw (signed) */);
			// Split the result.
			FAIL_IF(!result64);
			llvm::Value* const32 = llvm::Constant::getIntegerValue(
				llvm::Type::getInt64Ty(*context), llvm::APInt(64, 32)
			);
			FAIL_IF(!const32);
			// HI: Arithmetic shift right by 32 performs sign-extension.
			llvm::Value* hi64 = builder.CreateAShr(result64, const32);
			// LO: Truncate to 32 bits then sign-extend the result.
			llvm::Value* lo32 = builder.CreateTrunc(result64,
				llvm::Type::getInt32Ty(*context));
			FAIL_IF(!lo32);
			llvm::Value* lo64 = builder.CreateSExt(lo32,
				llvm::Type::getInt64Ty(*context));
			FAIL_IF(!hi64 || !lo64);
			queue_n64_hi(values, hi64);
			queue_n64_lo(values, lo64);
			break;
		}

		default:
			assert(false && "Opcode without an implementation requested in llvm_ir_for_insn");
			return false;
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
