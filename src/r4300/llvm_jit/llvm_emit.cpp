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
		return builder.getInt64(0);

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
		return builder.getInt32(0);

	llvm::Value* result64 = load_n64_int64(builder, values, src_reg);
	NULL_IF(!result64);
	return builder.CreateTrunc(result64, builder.getInt32Ty());
}

void queue_n64_int64(value_store& values, uint8_t dst_reg, llvm::Value* value)
{
	values.n64_int[dst_reg] = value;
	values.n64_int_dirty |= (UINT32_C(1) << dst_reg);
}

bool queue_n64_int32(llvm::IRBuilder<>& builder, value_store& values, uint8_t dst_reg, llvm::Value* value)
{
	llvm::Value* value64 = builder.CreateSExt(value, builder.getInt64Ty());
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
		case N64_OP_SRL:
		case N64_OP_SRA:
		{
			llvm::Value* a = load_n64_int32(builder, values, n64_insn->rt);
			FAIL_IF(!a);
			llvm::Value* result32 = NULL;
			switch (n64_insn->opcode) {
				case N64_OP_SLL:
					result32 = builder.CreateShl(a, n64_insn->imm);
					break;
				case N64_OP_SRL:
					result32 = builder.CreateLShr(a, n64_insn->imm);
					break;
				case N64_OP_SRA:
					result32 = builder.CreateAShr(a, n64_insn->imm);
					break;
				default: break;
			}
			FAIL_IF(!result32);
			FAIL_IF(!queue_n64_int32(builder, values, n64_insn->rd, result32));
			break;
		}

		case N64_OP_DSLL:
		case N64_OP_DSRL:
		case N64_OP_DSRA:
		case N64_OP_DSLL32:
		case N64_OP_DSRL32:
		case N64_OP_DSRA32:
		{
			llvm::Value* a = load_n64_int64(builder, values, n64_insn->rt);
			FAIL_IF(!a);
			llvm::Value* result64 = NULL;
			switch (n64_insn->opcode) {
				case N64_OP_DSLL:
					result64 = builder.CreateShl(a, n64_insn->imm);
					break;
				case N64_OP_DSRL:
					result64 = builder.CreateLShr(a, n64_insn->imm);
					break;
				case N64_OP_DSRA:
					result64 = builder.CreateAShr(a, n64_insn->imm);
					break;
				case N64_OP_DSLL32:
					result64 = builder.CreateShl(a, 32 + n64_insn->imm);
					break;
				case N64_OP_DSRL32:
					result64 = builder.CreateLShr(a, 32 + n64_insn->imm);
					break;
				case N64_OP_DSRA32:
					result64 = builder.CreateAShr(a, 32 + n64_insn->imm);
					break;
				default: break;
			}
			FAIL_IF(!result64);
			queue_n64_int64(values, n64_insn->rd, result64);
			break;
		}

		case N64_OP_SLLV:
		case N64_OP_SRLV:
		case N64_OP_SRAV:
		{
			llvm::Value* a = load_n64_int32(builder, values, n64_insn->rt);
			llvm::Value* b = load_n64_int32(builder, values, n64_insn->rs);
			// The shift amount must not be greater than 31 in LLVM.
			// The MIPS specification states that only the 5 least significant
			// bits contribute to the shift amount (so the mask is 31).
			FAIL_IF(!a || !b);
			llvm::Value* sa = builder.CreateAnd(b, 31);
			llvm::Value* result32 = NULL;
			switch (n64_insn->opcode) {
				case N64_OP_SLLV:
					result32 = builder.CreateShl(a, sa);
					break;
				case N64_OP_SRLV:
					result32 = builder.CreateLShr(a, sa);
					break;
				case N64_OP_SRAV:
					result32 = builder.CreateAShr(a, sa);
					break;
				default: break;
			}
			FAIL_IF(!result32);
			FAIL_IF(!queue_n64_int32(builder, values, n64_insn->rd, result32));
			break;
		}

		case N64_OP_DSLLV:
		case N64_OP_DSRLV:
		case N64_OP_DSRAV:
		{
			llvm::Value* a = load_n64_int64(builder, values, n64_insn->rt);
			llvm::Value* b = load_n64_int64(builder, values, n64_insn->rs);
			// The shift amount must not be greater than 63 in LLVM.
			// The MIPS specification states that only the 6 least significant
			// bits contribute to the shift amount (so the mask is 63).
			FAIL_IF(!a || !b);
			llvm::Value* sa = builder.CreateAnd(b, 63);
			llvm::Value* result64 = NULL;
			switch (n64_insn->opcode) {
				case N64_OP_DSLLV:
					result64 = builder.CreateShl(a, sa);
					break;
				case N64_OP_DSRLV:
					result64 = builder.CreateLShr(a, sa);
					break;
				case N64_OP_DSRAV:
					result64 = builder.CreateAShr(a, sa);
					break;
				default: break;
			}
			FAIL_IF(!result64);
			queue_n64_int64(values, n64_insn->rd, result64);
			break;
		}

		case N64_OP_ADD:
		case N64_OP_ADDU:
		{
			llvm::Value* a = load_n64_int32(builder, values, n64_insn->rs);
			FAIL_IF(!a);
			if (n64_insn->rt == 0) {
				// ADDU Rd, Rs, $0 moves a 32-bit value with sign-extension.
				FAIL_IF(!queue_n64_int32(builder, values, n64_insn->rd, a));
			} else {
				llvm::Value* b = load_n64_int32(builder, values, n64_insn->rt);
				FAIL_IF(!b);
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
			llvm::Value* a = load_n64_int64(builder, values, n64_insn->rs);
			FAIL_IF(!a);
			if (n64_insn->rt == 0) {
				// OR Rd, Rs, $0 moves a 64-bit value. (Rs == $0 stores 0.)
				queue_n64_int64(values, n64_insn->rd, a);
			} else {
				llvm::Value* b = load_n64_int64(builder, values, n64_insn->rt);
				FAIL_IF(!b);
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
			llvm::Value* allOnes = llvm::Constant::getAllOnesValue(builder.getInt64Ty());
			FAIL_IF(!allOnes);
			if (n64_insn->rs == 0 && n64_insn->rt == 0) {
				// NOR Rd, $0, $0 loads a register with -1.
				queue_n64_int64(values, n64_insn->rd, allOnes);
			} else if (n64_insn->rt == 0) {
				// NOR Rd, Rs, $0 is equivalent to "NOT" Rd, Rs.
				llvm::Value* a = load_n64_int64(builder, values, n64_insn->rs);
				FAIL_IF(!a);
				// NOT is implemented in LLVM as (a ^ all-ones).
				llvm::Value* result64 = builder.CreateXor(a, allOnes);
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
				llvm::Value* result64 = builder.CreateXor(or64, allOnes);
				FAIL_IF(!result64);
				queue_n64_int64(values, n64_insn->rd, result64);
			}
			break;
		}

		case N64_OP_SLT:
		case N64_OP_SLTU:
		{
			llvm::Value* a = load_n64_int64(builder, values, n64_insn->rs);
			llvm::Value* b = load_n64_int64(builder, values, n64_insn->rt);
			FAIL_IF(!a || !b);
			llvm::Value* cmp_bit = n64_insn->opcode == N64_OP_SLT
				? builder.CreateICmpSLT(a, b) /* signed */
				: builder.CreateICmpULT(a, b) /* unsigned */;
			FAIL_IF(!cmp_bit);
			llvm::Value* result64 = builder.CreateZExt(cmp_bit, builder.getInt64Ty());
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
			FAIL_IF(!a);
			llvm::Value* result32 = builder.CreateAdd(a, builder.getInt32(n64_insn->imm));
			FAIL_IF(!result32);
			FAIL_IF(!queue_n64_int32(builder, values, n64_insn->rt, result32));
			break;
		}

		case N64_OP_SLTI:
		{
			llvm::Value* a = load_n64_int64(builder, values, n64_insn->rs);
			FAIL_IF(!a);
			llvm::Value* cmp_bit = builder.CreateICmpSLT(
				a, builder.getInt64((int64_t) n64_insn->imm));
			FAIL_IF(!cmp_bit);
			llvm::Value* result64 = builder.CreateZExt(cmp_bit, builder.getInt64Ty());
			FAIL_IF(!result64);
			queue_n64_int64(values, n64_insn->rt, result64);
			break;
		}

		case N64_OP_SLTIU:
		{
			llvm::Value* a = load_n64_int64(builder, values, n64_insn->rs);
			FAIL_IF(!a);
			llvm::Value* cmp_bit = builder.CreateICmpULT(
				a, builder.getInt64((int64_t) n64_insn->imm));
			FAIL_IF(!cmp_bit);
			llvm::Value* result64 = builder.CreateZExt(cmp_bit, builder.getInt64Ty());
			FAIL_IF(!result64);
			queue_n64_int64(values, n64_insn->rt, result64);
			break;
		}

		case N64_OP_ANDI:
		{
			llvm::Value* a64 = load_n64_int64(builder, values, n64_insn->rs);
			FAIL_IF(!a64);
			llvm::Value* a = builder.CreateTrunc(a64, builder.getInt16Ty());
			FAIL_IF(!a);
			llvm::Value* result16 = builder.CreateAnd(a, n64_insn->imm);
			FAIL_IF(!result16);
			llvm::Value* result64 = builder.CreateZExt(result16, builder.getInt64Ty());
			FAIL_IF(!result64);
			queue_n64_int64(values, n64_insn->rt, result64);
			break;
		}

		case N64_OP_ORI:
		case N64_OP_XORI:
		{
			llvm::Value* a = load_n64_int64(builder, values, n64_insn->rs);
			FAIL_IF(!a);
			llvm::Value* result64 = n64_insn->opcode == N64_OP_ORI
				? builder.CreateOr(a, n64_insn->imm)
				: builder.CreateXor(a, n64_insn->imm);
			FAIL_IF(!result64);
			queue_n64_int64(values, n64_insn->rt, result64);
			break;
		}

		case N64_OP_LUI:
		{
			llvm::Value* a = builder.getInt64((int64_t) (n64_insn->imm << 16));
			FAIL_IF(!a);
			queue_n64_int64(values, n64_insn->rt, a);
			break;
		}

		case N64_OP_DADDI:
		case N64_OP_DADDIU:
		{
			llvm::Value* a = load_n64_int64(builder, values, n64_insn->rs);
			FAIL_IF(!a);
			llvm::Value* result64 = builder.CreateAdd(
				a, builder.getInt64((int64_t) n64_insn->imm));
			FAIL_IF(!result64);
			queue_n64_int64(values, n64_insn->rt, result64);
			break;
		}

		/* ... */

		case N64_OP_LB:
		case N64_OP_LBU:
		case N64_OP_LH:
		case N64_OP_LHU:
		case N64_OP_LW:
		case N64_OP_LWU:
		case N64_OP_LD:
		{
			// Calculate the (N64) address to be loaded from.
			llvm::Value* rs32 = load_n64_int32(builder, values, n64_insn->rs);
			FAIL_IF(!rs32);
			llvm::Value* address = rs32;
			if (n64_insn->imm != 0) {
				address = builder.CreateAdd(rs32, builder.getInt32(n64_insn->imm));
				FAIL_IF(!address);
			}
			// Prepare for the call to C.
			FAIL_IF(!store_queued_regs(builder, values));
			// Set the global variable 'address' to the address to be loaded.
			FAIL_IF(!builder.CreateStore(address, llvm_address));
			// Set the global variable 'rdword' to point to the register to be
			// written to.
			llvm::Value* dst_reg_ptr = builder.CreateConstInBoundsGEP2_32(llvm_reg, 0, n64_insn->rt);
			FAIL_IF(!dst_reg_ptr);
			FAIL_IF(!builder.CreateStore(dst_reg_ptr, llvm_rdword));
			// Grab the memory accessor function to be used.
			llvm::Value* address_hi16 = builder.CreateLShr(address, 16);
			FAIL_IF(!address_hi16);
			std::vector<llvm::Value*> gepArgs;
			gepArgs.push_back(builder.getInt32(0));
			gepArgs.push_back(address_hi16);

			// Calculate where the function pointer is.
			llvm::Value* accessor_addr = NULL;
			switch (n64_insn->opcode) {
				case N64_OP_LB:
				case N64_OP_LBU:
					accessor_addr = builder.CreateInBoundsGEP(llvm_readmemb, gepArgs);
					break;
				case N64_OP_LH:
				case N64_OP_LHU:
					accessor_addr = builder.CreateInBoundsGEP(llvm_readmemh, gepArgs);
					break;
				case N64_OP_LW:
				case N64_OP_LWU:
					accessor_addr = builder.CreateInBoundsGEP(llvm_readmem, gepArgs);
					break;
				case N64_OP_LD:
					accessor_addr = builder.CreateInBoundsGEP(llvm_readmemd, gepArgs);
					break;
				default: break;
			}
			FAIL_IF(!accessor_addr);

			// Then load it.
			llvm::Value* accessor = builder.CreateLoad(accessor_addr);
			// If an N64 exception occurs, the Program Counter needs to have
			// been updated to point past the load.
			FAIL_IF(!set_pc(builder, n64_insn->runtime + 1));
			// Call C.
			FAIL_IF(!builder.CreateCall(accessor));

			// C will have modified the value of 'address' to be 0 if an N64
			// exception occurred. Reload it and compare.
			llvm::Value* new_address = builder.CreateLoad(llvm_address);
			FAIL_IF(!new_address);
			llvm::Value* new_address_is_0 = builder.CreateICmpEQ(
				new_address, builder.getInt32(0));
			llvm::BasicBlock* if_zero = llvm::BasicBlock::Create(
				*context, "load_exception", builder.GetInsertBlock()->getParent());
			llvm::BasicBlock* if_nonzero = llvm::BasicBlock::Create(
				*context, "load_normal", builder.GetInsertBlock()->getParent());
			FAIL_IF(!new_address_is_0 || !if_zero || !if_nonzero);
			// Create a conditional branch that will go to our exiting block
			// (if_zero) if an N64 exception occurred, otherwise if_nonzero.
			FAIL_IF(!builder.CreateCondBr(new_address_is_0, if_zero, if_nonzero));
			builder.SetInsertPoint(if_zero);

			// if_zero: skip_jump = 0; (in case the load is in a delay slot)
			// return;
			FAIL_IF(!builder.CreateStore(builder.getInt32(0), llvm_skip_jump));
			FAIL_IF(!builder.CreateRetVoid());

			// if_nonzero:
			builder.SetInsertPoint(if_nonzero);
			switch (n64_insn->opcode) {
				// Fixup: Load the register and sign-extend it, only if signed
				case N64_OP_LB:
				{
					llvm::Value* rt64 = load_n64_int64(builder, values, n64_insn->rt);
					FAIL_IF(!rt64);
					llvm::Value* rt8 = builder.CreateTrunc(rt64, builder.getInt8Ty());
					FAIL_IF(!rt8);
					rt64 = builder.CreateSExt(rt8, builder.getInt64Ty());
					queue_n64_int64(values, n64_insn->rt, rt64);
					break;
				}
				case N64_OP_LH:
				{
					llvm::Value* rt64 = load_n64_int64(builder, values, n64_insn->rt);
					FAIL_IF(!rt64);
					llvm::Value* rt16 = builder.CreateTrunc(rt64, builder.getInt16Ty());
					FAIL_IF(!rt16);
					rt64 = builder.CreateSExt(rt16, builder.getInt64Ty());
					queue_n64_int64(values, n64_insn->rt, rt64);
					break;
				}
				case N64_OP_LW:
				{
					llvm::Value* rt32 = load_n64_int32(builder, values, n64_insn->rt);
					FAIL_IF(!rt32);
					FAIL_IF(!queue_n64_int32(builder, values, n64_insn->rt, rt32));
					break;
				}
				default: break;
			}
			break;
		}

		case N64_OP_SB:
		case N64_OP_SH:
		case N64_OP_SW:
		case N64_OP_SD:
		{
			// Calculate the (N64) address to be stored to.
			llvm::Value* rs32 = load_n64_int32(builder, values, n64_insn->rs);
			FAIL_IF(!rs32);
			llvm::Value* address = rs32;
			if (n64_insn->imm != 0) {
				address = builder.CreateAdd(rs32, builder.getInt32(n64_insn->imm));
				FAIL_IF(!address);
			}

			// Mark the page containing the address as having been rewritten.
			// If it contained N64 code, that will invalidate the native code
			// or cached interpretation done for thae N64 code the next time
			// something jumps to it.
			llvm::Value* address_hi20 = builder.CreateLShr(address, 12);
			FAIL_IF(!address_hi20);
			std::vector<llvm::Value*> icGepArgs;
			icGepArgs.push_back(builder.getInt32(0));
			icGepArgs.push_back(address_hi20);
			llvm::Value* invalid_code_addr = builder.CreateInBoundsGEP(
				llvm_invalid_code, icGepArgs);
			FAIL_IF(!invalid_code_addr);
			FAIL_IF(!builder.CreateStore(builder.getInt8(1), invalid_code_addr));

			// Prepare for the call to C.
			// Set a global variable to the value to be written.
			switch (n64_insn->opcode) {
				case N64_OP_SB:
				{
					// Here the value of 'cpu_byte' will be stored.
					llvm::Value* rt64 = load_n64_int64(builder, values, n64_insn->rt);
					FAIL_IF(!rt64);
					llvm::Value* rt8 = builder.CreateTrunc(rt64, builder.getInt8Ty());
					FAIL_IF(!builder.CreateStore(rt8, llvm_cpu_byte));
					break;
				}
				case N64_OP_SH:
				{
					// Here the value of 'hword' will be stored.
					llvm::Value* rt64 = load_n64_int64(builder, values, n64_insn->rt);
					FAIL_IF(!rt64);
					llvm::Value* rt16 = builder.CreateTrunc(rt64, builder.getInt16Ty());
					FAIL_IF(!builder.CreateStore(rt16, llvm_hword));
					break;
				}
				case N64_OP_SW:
				{
					// Here the value of 'word' will be stored.
					llvm::Value* rt32 = load_n64_int32(builder, values, n64_insn->rt);
					FAIL_IF(!rt32);
					FAIL_IF(!builder.CreateStore(rt32, llvm_word));
					break;
				}
				case N64_OP_SD:
				{
					// Here the value of 'dword' will be stored.
					llvm::Value* rt64 = load_n64_int64(builder, values, n64_insn->rt);
					FAIL_IF(!rt64);
					FAIL_IF(!builder.CreateStore(rt64, llvm_dword));
					break;
				}
				default: break;
			}
			FAIL_IF(!store_queued_regs(builder, values));
			// Set the global variable 'address' to the address to be stored.
			FAIL_IF(!builder.CreateStore(address, llvm_address));
			// Grab the memory accessor function to be used.
			llvm::Value* address_hi16 = builder.CreateLShr(address, 16);
			FAIL_IF(!address_hi16);
			std::vector<llvm::Value*> gepArgs;
			gepArgs.push_back(builder.getInt32(0));
			gepArgs.push_back(address_hi16);

			// Calculate where the function pointer is.
			llvm::Value* accessor_addr = NULL;
			switch (n64_insn->opcode) {
				case N64_OP_SB:
					accessor_addr = builder.CreateInBoundsGEP(llvm_writememb, gepArgs);
					break;
				case N64_OP_SH:
					accessor_addr = builder.CreateInBoundsGEP(llvm_writememh, gepArgs);
					break;
				case N64_OP_SW:
					accessor_addr = builder.CreateInBoundsGEP(llvm_writemem, gepArgs);
					break;
				case N64_OP_SD:
					accessor_addr = builder.CreateInBoundsGEP(llvm_writememd, gepArgs);
					break;
				default: break;
			}
			FAIL_IF(!accessor_addr);

			// Then load it.
			llvm::Value* accessor = builder.CreateLoad(accessor_addr);
			// If an N64 exception occurs, the Program Counter needs to have
			// been updated to point past the store.
			FAIL_IF(!set_pc(builder, n64_insn->runtime + 1));
			// Call C.
			FAIL_IF(!builder.CreateCall(accessor));

			// C will have modified the value of 'address' to be 0 if an N64
			// exception occurred. Reload it and compare.
			llvm::Value* new_address = builder.CreateLoad(llvm_address);
			FAIL_IF(!new_address);
			llvm::Value* new_address_is_0 = builder.CreateICmpEQ(
				new_address, builder.getInt32(0));
			llvm::BasicBlock* if_zero = llvm::BasicBlock::Create(
				*context, "store_exception", builder.GetInsertBlock()->getParent());
			llvm::BasicBlock* if_nonzero = llvm::BasicBlock::Create(
				*context, "store_normal", builder.GetInsertBlock()->getParent());
			FAIL_IF(!new_address_is_0 || !if_zero || !if_nonzero);
			// Create a conditional branch that will go to our exiting block
			// (if_zero) if an N64 exception occurred, otherwise if_nonzero.
			FAIL_IF(!builder.CreateCondBr(new_address_is_0, if_zero, if_nonzero));
			builder.SetInsertPoint(if_zero);

			// if_zero: skip_jump = 0; (in case the store is in a delay slot)
			// return;
			FAIL_IF(!builder.CreateStore(builder.getInt32(0), llvm_skip_jump));
			FAIL_IF(!builder.CreateRetVoid());

			// if_nonzero:
			builder.SetInsertPoint(if_nonzero);
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
		case N64_OP_MULTU:
		{
			// This is a 32-bit by 32-bit (un)signed multiplication with
			// 64-bit result. The upper and lower 32 bits go into HI and LO,
			// both sign-extended to 64 bits independently from each other.
			llvm::Value* a32 = load_n64_int32(builder, values, n64_insn->rs);
			llvm::Value* b32 = load_n64_int32(builder, values, n64_insn->rt);
			FAIL_IF(!a32 || !b32);
			// In LLVM, 64-bit operands are required in order to get a 64-bit
			// result. So we sign-extend, or zero-extend, the 32-bit truncated
			// operands back to 64-bit.
			llvm::Value* a64 = n64_insn->opcode == N64_OP_MULT
				? builder.CreateSExt(a32, builder.getInt64Ty())
				: builder.CreateZExt(a32, builder.getInt64Ty());
			llvm::Value* b64 = n64_insn->opcode == N64_OP_MULT
				? builder.CreateSExt(b32, builder.getInt64Ty())
				: builder.CreateZExt(b32, builder.getInt64Ty());
			FAIL_IF(!a64 || !b64);
			// We know the result will not overflow in any way.
			llvm::Value* result64 = builder.CreateMul(a64, b64, "",
				true /* nuw (no unsigned wrap) */, true /* nsw (signed) */);
			// Split the result.
			FAIL_IF(!result64);
			// HI: Arithmetic shift right by 32 performs sign-extension.
			llvm::Value* hi64 = builder.CreateAShr(result64, 32);
			// LO: Truncate to 32 bits then sign-extend the result.
			llvm::Value* lo32 = builder.CreateTrunc(result64, builder.getInt32Ty());
			FAIL_IF(!lo32);
			llvm::Value* lo64 = builder.CreateSExt(lo32, builder.getInt64Ty());
			FAIL_IF(!hi64 || !lo64);
			queue_n64_hi(values, hi64);
			queue_n64_lo(values, lo64);
			break;
		}

		/* ... */

		case N64_OP_DMULT:
		case N64_OP_DMULTU:
		{
			// This is a 64-bit by 64-bit (un)signed multiplication with
			// 128-bit result. The upper and lower 64 bits go into HI and LO.
			llvm::Value* a64 = load_n64_int64(builder, values, n64_insn->rs);
			llvm::Value* b64 = load_n64_int64(builder, values, n64_insn->rt);
			FAIL_IF(!a64 || !b64);
			// In LLVM, 128-bit operands are required in order to get a
			// 128-bit result. So we sign-extend, or zero-extend, the 64-bit
			// truncated operands back to 128-bit.
			llvm::Value* a128 = n64_insn->opcode == N64_OP_DMULT
				? builder.CreateSExt(a64, llvm::Type::getIntNTy(*context, 128))
				: builder.CreateZExt(a64, llvm::Type::getIntNTy(*context, 128));
			llvm::Value* b128 = n64_insn->opcode == N64_OP_DMULT
				? builder.CreateSExt(b64, llvm::Type::getIntNTy(*context, 128))
				: builder.CreateZExt(b64, llvm::Type::getIntNTy(*context, 128));
			FAIL_IF(!a128 || !b128);
			// We know the result will not overflow in any way.
			llvm::Value* result128 = builder.CreateMul(a128, b128, "",
				true /* nuw (no unsigned wrap) */, true /* nsw (signed) */);
			// Split the result.
			FAIL_IF(!result128);
			llvm::Value* hi64 = builder.CreateAShr(result128, 64);
			llvm::Value* lo64 = builder.CreateTrunc(result128, builder.getInt64Ty());
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
