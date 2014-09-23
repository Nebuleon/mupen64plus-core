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
#include "llvm/IR/MDBuilder.h" /* MDBuilder */
#include "llvm/IR/Module.h" /* complete Function type */

#include "branches.h"
#include "emitflags.h"
#include "llvm_bridge.h"
#include "llvm_aux.hpp"
#include "n64ops.h"
#include "../cp0.h" /* get definitions for CP0_*_REG */
#include "../r4300.h" /* get declaration for count_per_op */

#define FAIL_IF(cond) do { if (cond) { return false; } } while (0)
#define NULL_IF(cond) do { if (cond) { return NULL; } } while (0)

bool llvm_ir_for_insn(llvm::IRBuilder<>& builder, FunctionData& fnData, const n64_insn_t* n64_insn, bool delay_slot);

bool llvm_ir_for_delay_slot(llvm::IRBuilder<>& builder, FunctionData& fnData, const n64_insn_t* n64_insn)
{
	if (!(n64_insn->emit_flags & INSTRUCTION_IGNORES_DELAY_SLOT)) {
		FAIL_IF(!builder.CreateStore(builder.getInt32(1), llvm_delay_slot));
	}
	FAIL_IF(!llvm_ir_for_insn(builder, fnData, n64_insn, true /* delay slot */));
	if (!(n64_insn->emit_flags & INSTRUCTION_IGNORES_DELAY_SLOT)) {
		FAIL_IF(!builder.CreateStore(builder.getInt32(0), llvm_delay_slot));
	}
	return true;
}

bool llvm_ir_for_update_count(llvm::IRBuilder<>& builder, FunctionData& fnData, uint32_t range_end)
{
	// a) Count
	llvm::Value* count_val = fnData.loadN64Cop0(builder, CP0_COUNT_REG);
	// b) last_addr
	llvm::Value* last_addr_val = builder.CreateLoad(llvm_last_addr);
	FAIL_IF(!count_val || !last_addr_val);
	// c) range_end - last_addr ['sub nuw': end - start will not be negative]
	llvm::Value* range_bytes = builder.CreateNUWSub(builder.getInt32(range_end), last_addr_val);
	FAIL_IF(!range_bytes);
	// d) (range_end - last_addr) / 4 ['lshr exact': 4-byte aligned MIPS code]
	llvm::Value* range_insns = builder.CreateLShr(range_bytes, 2, "",
		true /* exact */);
	FAIL_IF(!range_insns);
	// e) ((range_end - last_addr) / 4) * count_per_op
	// count_per_op is read at compilation time, because all writes to it are
	// executed strictly before the JIT is started. If this changes, please
	// edit this to perform a load of count_per_op at runtime.
	llvm::Value* count_add = builder.CreateMul(range_insns, builder.getInt32(count_per_op));
	FAIL_IF(!count_add);
	// f) Count + ((range_end - last_addr) / 4) * count_per_op
	llvm::Value* count_new = builder.CreateAdd(count_val, count_add);
	FAIL_IF(!count_new);
	// g) Count = Count + ((range_end - last_addr) / 4) * count_per_op;
	return fnData.storeN64Cop0(builder, CP0_COUNT_REG, count_new);
}

bool llvm_ir_for_update_last_addr(llvm::IRBuilder<>& builder, FunctionData& fnData, uint32_t new_range_start)
{
	return builder.CreateStore(builder.getInt32(new_range_start), llvm_last_addr) != NULL;
}

bool llvm_ir_for_interrupt_check(llvm::IRBuilder<>& builder, FunctionData& fnData, const n64_insn_t* n64_insn, precomp_instr* restart_pc)
{
	// a) next_interupt
	llvm::Value* next_interupt_val = builder.CreateLoad(llvm_next_interupt);
	// b) Count
	llvm::Value* count_val = fnData.loadN64Cop0(builder, CP0_COUNT_REG);
	FAIL_IF(!next_interupt_val || !count_val);
	// c) next_interupt <= Count
	llvm::Value* cmp = builder.CreateICmpULE(next_interupt_val, count_val);

	llvm::BasicBlock* if_interrupt = llvm::BasicBlock::Create(
		*context, llvm::Twine(nameForAddr(n64_insn->addr)).concat("_Interrupt"),
		builder.GetInsertBlock()->getParent());
	llvm::BasicBlock* if_no_interrupt = llvm::BasicBlock::Create(
		*context, llvm::Twine(nameForAddr(n64_insn->addr)).concat("_NoInterrupt"),
		builder.GetInsertBlock()->getParent());
	FAIL_IF(!cmp || !if_interrupt || !if_no_interrupt);
	// Create a conditional branch that will go to our interrupt-triggering
	// block (if_interrupt) if an N64 interrupt needs to occur, otherwise to
	// if_no_interrupt.
	// Having no interrupt is far more likely than having one.
	FAIL_IF(!builder.CreateCondBr(cmp, if_interrupt, if_no_interrupt,
		llvm::MDBuilder(*context).createBranchWeights(1, 256)));

	// if_interrupt:
	builder.SetInsertPoint(if_interrupt);
	FAIL_IF(!fnData.setPC(builder, restart_pc));
	FAIL_IF(!fnData.setInterrupt(builder));
	FAIL_IF(!fnData.branchToStore(builder));

	// if_no_interrupt:
	builder.SetInsertPoint(if_no_interrupt);

	return true;
}

bool llvm_ir_for_insn(llvm::IRBuilder<>& builder, FunctionData& fnData, const n64_insn_t* n64_insn, bool delay_slot)
{
	switch (n64_insn->opcode) {
		case N64_OP_NOP:
		case N64_OP_CACHE:
		case N64_OP_SYNC:
			if (!delay_slot)
				FAIL_IF(!fnData.branchN64(builder, n64_insn->addr + 4));
			break;

		case N64_OP_SLL:
		case N64_OP_SRL:
		case N64_OP_SRA:
		{
			llvm::Value* a = fnData.loadN64Int(builder, n64_insn->rt, 32);
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
			FAIL_IF(!fnData.storeN64Int(builder, n64_insn->rd, result32, 32));
			if (!delay_slot)
				FAIL_IF(!fnData.branchN64(builder, n64_insn->addr + 4));
			break;
		}

		case N64_OP_DSLL:
		case N64_OP_DSRL:
		case N64_OP_DSRA:
		case N64_OP_DSLL32:
		case N64_OP_DSRL32:
		case N64_OP_DSRA32:
		{
			llvm::Value* a = fnData.loadN64Int(builder, n64_insn->rt);
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
			FAIL_IF(!fnData.storeN64Int(builder, n64_insn->rd, result64));
			if (!delay_slot)
				FAIL_IF(!fnData.branchN64(builder, n64_insn->addr + 4));
			break;
		}

		case N64_OP_SLLV:
		case N64_OP_SRLV:
		case N64_OP_SRAV:
		{
			llvm::Value* a = fnData.loadN64Int(builder, n64_insn->rt, 32);
			llvm::Value* b = fnData.loadN64Int(builder, n64_insn->rs, 32);
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
			FAIL_IF(!fnData.storeN64Int(builder, n64_insn->rd, result32, 32));
			if (!delay_slot)
				FAIL_IF(!fnData.branchN64(builder, n64_insn->addr + 4));
			break;
		}

		case N64_OP_DSLLV:
		case N64_OP_DSRLV:
		case N64_OP_DSRAV:
		{
			llvm::Value* a = fnData.loadN64Int(builder, n64_insn->rt);
			llvm::Value* b = fnData.loadN64Int(builder, n64_insn->rs);
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
			FAIL_IF(!fnData.storeN64Int(builder, n64_insn->rd, result64));
			if (!delay_slot)
				FAIL_IF(!fnData.branchN64(builder, n64_insn->addr + 4));
			break;
		}

		case N64_OP_ADD:
		case N64_OP_ADDU:
		{
			llvm::Value* a = fnData.loadN64Int(builder, n64_insn->rs, 32);
			FAIL_IF(!a);
			if (n64_insn->rt == 0) {
				// ADDU Rd, Rs, $0 moves a 32-bit value with sign-extension.
				FAIL_IF(!fnData.storeN64Int(builder, n64_insn->rd, a, 32));
			} else {
				llvm::Value* b = fnData.loadN64Int(builder, n64_insn->rt, 32);
				FAIL_IF(!b);
				llvm::Value* result32 = builder.CreateAdd(a, b);
				FAIL_IF(!result32);
				FAIL_IF(!fnData.storeN64Int(builder, n64_insn->rd, result32, 32));
			}
			if (!delay_slot)
				FAIL_IF(!fnData.branchN64(builder, n64_insn->addr + 4));
			break;
		}

		case N64_OP_SUB:
		case N64_OP_SUBU:
		{
			llvm::Value* a = fnData.loadN64Int(builder, n64_insn->rs, 32);
			llvm::Value* b = fnData.loadN64Int(builder, n64_insn->rt, 32);
			FAIL_IF(!a || !b);
			llvm::Value* result32 = builder.CreateSub(a, b);
			FAIL_IF(!result32);
			FAIL_IF(!fnData.storeN64Int(builder, n64_insn->rd, result32, 32));
			if (!delay_slot)
				FAIL_IF(!fnData.branchN64(builder, n64_insn->addr + 4));
			break;
		}

		case N64_OP_AND:
		{
			llvm::Value* a = fnData.loadN64Int(builder, n64_insn->rs);
			llvm::Value* b = fnData.loadN64Int(builder, n64_insn->rt);
			FAIL_IF(!a || !b);
			llvm::Value* result64 = builder.CreateAnd(a, b);
			FAIL_IF(!result64);
			FAIL_IF(!fnData.storeN64Int(builder, n64_insn->rd, result64));
			if (!delay_slot)
				FAIL_IF(!fnData.branchN64(builder, n64_insn->addr + 4));
			break;
		}

		case N64_OP_OR:
		{
			llvm::Value* a = fnData.loadN64Int(builder, n64_insn->rs);
			FAIL_IF(!a);
			if (n64_insn->rt == 0) {
				// OR Rd, Rs, $0 moves a 64-bit value. (Rs == $0 stores 0.)
				FAIL_IF(!fnData.storeN64Int(builder, n64_insn->rd, a));
			} else {
				llvm::Value* b = fnData.loadN64Int(builder, n64_insn->rt);
				FAIL_IF(!b);
				llvm::Value* result64 = builder.CreateOr(a, b);
				FAIL_IF(!result64);
				FAIL_IF(!fnData.storeN64Int(builder, n64_insn->rd, result64));
			}
			if (!delay_slot)
				FAIL_IF(!fnData.branchN64(builder, n64_insn->addr + 4));
			break;
		}

		case N64_OP_XOR:
		{
			llvm::Value* a = fnData.loadN64Int(builder, n64_insn->rs);
			llvm::Value* b = fnData.loadN64Int(builder, n64_insn->rt);
			FAIL_IF(!a || !b);
			llvm::Value* result64 = builder.CreateXor(a, b);
			FAIL_IF(!result64);
			FAIL_IF(!fnData.storeN64Int(builder, n64_insn->rd, result64));
			if (!delay_slot)
				FAIL_IF(!fnData.branchN64(builder, n64_insn->addr + 4));
			break;
		}

		case N64_OP_NOR:
		{
			llvm::Value* allOnes = llvm::Constant::getAllOnesValue(builder.getInt64Ty());
			FAIL_IF(!allOnes);
			if (n64_insn->rs == 0 && n64_insn->rt == 0) {
				// NOR Rd, $0, $0 loads a register with -1.
				FAIL_IF(!fnData.storeN64Int(builder, n64_insn->rd, allOnes));
			} else if (n64_insn->rt == 0) {
				// NOR Rd, Rs, $0 is equivalent to "NOT" Rd, Rs.
				llvm::Value* a = fnData.loadN64Int(builder, n64_insn->rs);
				FAIL_IF(!a);
				// NOT is implemented in LLVM as (a ^ all-ones).
				llvm::Value* result64 = builder.CreateXor(a, allOnes);
				FAIL_IF(!result64);
				FAIL_IF(!fnData.storeN64Int(builder, n64_insn->rd, result64));
			} else {
				llvm::Value* a = fnData.loadN64Int(builder, n64_insn->rs);
				llvm::Value* b = fnData.loadN64Int(builder, n64_insn->rt);
				FAIL_IF(!a || !b);
				// NOR is implemented in LLVM as (~(a | b)),
				// which in turn is ((a | b) ^ all-ones).
				llvm::Value* or64 = builder.CreateOr(a, b);
				FAIL_IF(!or64);
				llvm::Value* result64 = builder.CreateXor(or64, allOnes);
				FAIL_IF(!result64);
				FAIL_IF(!fnData.storeN64Int(builder, n64_insn->rd, result64));
			}
			if (!delay_slot)
				FAIL_IF(!fnData.branchN64(builder, n64_insn->addr + 4));
			break;
		}

		case N64_OP_SLT:
		case N64_OP_SLTU:
		{
			llvm::Value* a = fnData.loadN64Int(builder, n64_insn->rs);
			llvm::Value* b = fnData.loadN64Int(builder, n64_insn->rt);
			FAIL_IF(!a || !b);
			llvm::Value* cmp_bit = n64_insn->opcode == N64_OP_SLT
				? builder.CreateICmpSLT(a, b) /* signed */
				: builder.CreateICmpULT(a, b) /* unsigned */;
			FAIL_IF(!cmp_bit);
			llvm::Value* result64 = builder.CreateZExt(cmp_bit, builder.getInt64Ty());
			FAIL_IF(!result64);
			FAIL_IF(!fnData.storeN64Int(builder, n64_insn->rd, result64));
			if (!delay_slot)
				FAIL_IF(!fnData.branchN64(builder, n64_insn->addr + 4));
			break;
		}

		case N64_OP_DADD:
		case N64_OP_DADDU:
		{
			llvm::Value* a = fnData.loadN64Int(builder, n64_insn->rs);
			llvm::Value* b = fnData.loadN64Int(builder, n64_insn->rt);
			FAIL_IF(!a || !b);
			llvm::Value* result64 = builder.CreateAdd(a, b);
			FAIL_IF(!result64);
			FAIL_IF(!fnData.storeN64Int(builder, n64_insn->rd, result64));
			if (!delay_slot)
				FAIL_IF(!fnData.branchN64(builder, n64_insn->addr + 4));
			break;
		}

		case N64_OP_DSUB:
		case N64_OP_DSUBU:
		{
			llvm::Value* a = fnData.loadN64Int(builder, n64_insn->rs);
			llvm::Value* b = fnData.loadN64Int(builder, n64_insn->rt);
			FAIL_IF(!a || !b);
			llvm::Value* result64 = builder.CreateSub(a, b);
			FAIL_IF(!result64);
			FAIL_IF(!fnData.storeN64Int(builder, n64_insn->rd, result64));
			if (!delay_slot)
				FAIL_IF(!fnData.branchN64(builder, n64_insn->addr + 4));
			break;
		}

		case N64_OP_ADDI:
		case N64_OP_ADDIU:
		{
			llvm::Value* a = fnData.loadN64Int(builder, n64_insn->rs, 32);
			FAIL_IF(!a);
			llvm::Value* result32 = builder.CreateAdd(a, builder.getInt32(n64_insn->imm));
			FAIL_IF(!result32);
			FAIL_IF(!fnData.storeN64Int(builder, n64_insn->rt, result32, 32));
			if (!delay_slot)
				FAIL_IF(!fnData.branchN64(builder, n64_insn->addr + 4));
			break;
		}

		case N64_OP_SLTI:
		{
			llvm::Value* a = fnData.loadN64Int(builder, n64_insn->rs);
			FAIL_IF(!a);
			llvm::Value* cmp_bit = builder.CreateICmpSLT(
				a, builder.getInt64((int64_t) n64_insn->imm));
			FAIL_IF(!cmp_bit);
			llvm::Value* result64 = builder.CreateZExt(cmp_bit, builder.getInt64Ty());
			FAIL_IF(!result64);
			FAIL_IF(!fnData.storeN64Int(builder, n64_insn->rt, result64));
			if (!delay_slot)
				FAIL_IF(!fnData.branchN64(builder, n64_insn->addr + 4));
			break;
		}

		case N64_OP_SLTIU:
		{
			llvm::Value* a = fnData.loadN64Int(builder, n64_insn->rs);
			FAIL_IF(!a);
			llvm::Value* cmp_bit = builder.CreateICmpULT(
				a, builder.getInt64((int64_t) n64_insn->imm));
			FAIL_IF(!cmp_bit);
			llvm::Value* result64 = builder.CreateZExt(cmp_bit, builder.getInt64Ty());
			FAIL_IF(!result64);
			FAIL_IF(!fnData.storeN64Int(builder, n64_insn->rt, result64));
			if (!delay_slot)
				FAIL_IF(!fnData.branchN64(builder, n64_insn->addr + 4));
			break;
		}

		case N64_OP_ANDI:
		{
			llvm::Value* a = fnData.loadN64Int(builder, n64_insn->rs, 32);
			FAIL_IF(!a);
			llvm::Value* result32 = builder.CreateAnd(a, n64_insn->imm);
			FAIL_IF(!result32);
			FAIL_IF(!fnData.storeN64Int(builder, n64_insn->rt, result32, 32));
			if (!delay_slot)
				FAIL_IF(!fnData.branchN64(builder, n64_insn->addr + 4));
			break;
		}

		case N64_OP_ORI:
		case N64_OP_XORI:
		{
			llvm::Value* a = fnData.loadN64Int(builder, n64_insn->rs);
			FAIL_IF(!a);
			llvm::Value* result64 = n64_insn->opcode == N64_OP_ORI
				? builder.CreateOr(a, n64_insn->imm)
				: builder.CreateXor(a, n64_insn->imm);
			FAIL_IF(!result64);
			FAIL_IF(!fnData.storeN64Int(builder, n64_insn->rt, result64));
			if (!delay_slot)
				FAIL_IF(!fnData.branchN64(builder, n64_insn->addr + 4));
			break;
		}

		case N64_OP_LUI:
		{
			llvm::Value* a = builder.getInt64((int64_t) (n64_insn->imm << 16));
			FAIL_IF(!a);
			FAIL_IF(!fnData.storeN64Int(builder, n64_insn->rt, a));
			if (!delay_slot)
				FAIL_IF(!fnData.branchN64(builder, n64_insn->addr + 4));
			break;
		}

		case N64_OP_DADDI:
		case N64_OP_DADDIU:
		{
			llvm::Value* a = fnData.loadN64Int(builder, n64_insn->rs);
			FAIL_IF(!a);
			llvm::Value* result64 = builder.CreateAdd(
				a, builder.getInt64((int64_t) n64_insn->imm));
			FAIL_IF(!result64);
			FAIL_IF(!fnData.storeN64Int(builder, n64_insn->rt, result64));
			if (!delay_slot)
				FAIL_IF(!fnData.branchN64(builder, n64_insn->addr + 4));
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
			llvm::Value* rs32 = fnData.loadN64Int(builder, n64_insn->rs, 32);
			FAIL_IF(!rs32);
			llvm::Value* address = rs32;
			if (n64_insn->imm != 0) {
				address = builder.CreateAdd(rs32, builder.getInt32(n64_insn->imm));
				FAIL_IF(!address);
			}
			// Prepare for the call to C.
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
			FAIL_IF(!fnData.setPC(builder, n64_insn->runtime + 1));
			// Call C.
			FAIL_IF(!builder.CreateCall(accessor));

			// C will have modified the value of 'address' to be 0 if an N64
			// exception occurred. Reload it and compare.
			llvm::Value* new_address = builder.CreateLoad(llvm_address);
			FAIL_IF(!new_address);
			llvm::Value* new_address_is_0 = builder.CreateICmpEQ(
				new_address, builder.getInt32(0));
			llvm::BasicBlock* if_zero = llvm::BasicBlock::Create(
				*context,
				llvm::Twine(nameForAddr(n64_insn->addr, delay_slot)).concat("_Except"),
				builder.GetInsertBlock()->getParent());
			llvm::BasicBlock* if_nonzero = llvm::BasicBlock::Create(
				*context,
				llvm::Twine(nameForAddr(n64_insn->addr, delay_slot)).concat("_Normal"),
				builder.GetInsertBlock()->getParent());
			FAIL_IF(!new_address_is_0 || !if_zero || !if_nonzero);
			// Create a conditional branch that will go to our exiting block
			// (if_zero) if an N64 exception occurred, otherwise if_nonzero.
			// Having no exception is far more likely than having one.
			FAIL_IF(!builder.CreateCondBr(new_address_is_0, if_zero, if_nonzero,
				llvm::MDBuilder(*context).createBranchWeights(1, 1024)));
			builder.SetInsertPoint(if_zero);

			// if_zero: skip_jump = 0; (in case the load is in a delay slot)
			// return;
			if (delay_slot)
				FAIL_IF(!builder.CreateStore(builder.getInt32(0), llvm_skip_jump));
			FAIL_IF(!fnData.branchToStore(builder));

			// if_nonzero:
			builder.SetInsertPoint(if_nonzero);

			// Load the new value directly from &reg[rt] and, if the load was
			// signed, sign-extend the value.
			std::vector<llvm::Value*> reloadGepArgs;
			reloadGepArgs.push_back(builder.getInt32(0));
			reloadGepArgs.push_back(builder.getInt32(n64_insn->rt));

			llvm::Value* rt_ptr = builder.CreateInBoundsGEP(llvm_reg, reloadGepArgs);
			FAIL_IF(!rt_ptr);
			llvm::Value* rt64 = builder.CreateLoad(rt_ptr);
			FAIL_IF(!rt64);

			switch (n64_insn->opcode) {
				case N64_OP_LB:
				{
					llvm::Value* rt8 = builder.CreateTrunc(rt64, builder.getInt8Ty());
					FAIL_IF(!rt8);
					FAIL_IF(!fnData.storeN64Int(builder, n64_insn->rt, rt8, 8));
					break;
				}
				case N64_OP_LH:
				{
					llvm::Value* rt16 = builder.CreateTrunc(rt64, builder.getInt16Ty());
					FAIL_IF(!rt16);
					FAIL_IF(!fnData.storeN64Int(builder, n64_insn->rt, rt16, 16));
					break;
				}
				case N64_OP_LW:
				{
					llvm::Value* rt32 = builder.CreateTrunc(rt64, builder.getInt32Ty());
					FAIL_IF(!rt32);
					FAIL_IF(!fnData.storeN64Int(builder, n64_insn->rt, rt32, 32));
					break;
				}
				case N64_OP_LBU:
				case N64_OP_LHU:
				case N64_OP_LWU:
				case N64_OP_LD:
				{
					FAIL_IF(!fnData.storeN64Int(builder, n64_insn->rt, rt64));
					break;
				}
				default: break;
			}
			if (!delay_slot)
				FAIL_IF(!fnData.branchN64(builder, n64_insn->addr + 4));
			break;
		}

		case N64_OP_SB:
		case N64_OP_SH:
		case N64_OP_SW:
		case N64_OP_SD:
		{
			// Calculate the (N64) address to be stored to.
			llvm::Value* rs32 = fnData.loadN64Int(builder, n64_insn->rs, 32);
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
					llvm::Value* rt8 = fnData.loadN64Int(builder, n64_insn->rt, 8);
					FAIL_IF(!rt8);
					FAIL_IF(!builder.CreateStore(rt8, llvm_cpu_byte));
					break;
				}
				case N64_OP_SH:
				{
					// Here the value of 'hword' will be stored.
					llvm::Value* rt16 = fnData.loadN64Int(builder, n64_insn->rt, 16);
					FAIL_IF(!rt16);
					FAIL_IF(!builder.CreateStore(rt16, llvm_hword));
					break;
				}
				case N64_OP_SW:
				{
					// Here the value of 'word' will be stored.
					llvm::Value* rt32 = fnData.loadN64Int(builder, n64_insn->rt, 32);
					FAIL_IF(!rt32);
					FAIL_IF(!builder.CreateStore(rt32, llvm_word));
					break;
				}
				case N64_OP_SD:
				{
					// Here the value of 'dword' will be stored.
					llvm::Value* rt64 = fnData.loadN64Int(builder, n64_insn->rt);
					FAIL_IF(!rt64);
					FAIL_IF(!builder.CreateStore(rt64, llvm_dword));
					break;
				}
				default: break;
			}
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
			FAIL_IF(!fnData.setPC(builder, n64_insn->runtime + 1));
			// Call C.
			FAIL_IF(!builder.CreateCall(accessor));

			// C will have modified the value of 'address' to be 0 if an N64
			// exception occurred. Reload it and compare.
			llvm::Value* new_address = builder.CreateLoad(llvm_address);
			FAIL_IF(!new_address);
			llvm::Value* new_address_is_0 = builder.CreateICmpEQ(
				new_address, builder.getInt32(0));
			llvm::BasicBlock* if_zero = llvm::BasicBlock::Create(
				*context,
				llvm::Twine(nameForAddr(n64_insn->addr, delay_slot)).concat("_Except"),
				builder.GetInsertBlock()->getParent());
			llvm::BasicBlock* if_nonzero = llvm::BasicBlock::Create(
				*context,
				llvm::Twine(nameForAddr(n64_insn->addr, delay_slot)).concat("_Normal"),
				builder.GetInsertBlock()->getParent());
			FAIL_IF(!new_address_is_0 || !if_zero || !if_nonzero);
			// Create a conditional branch that will go to our exiting block
			// (if_zero) if an N64 exception occurred, otherwise if_nonzero.
			// Having no exception is far more likely than having one.
			FAIL_IF(!builder.CreateCondBr(new_address_is_0, if_zero, if_nonzero,
				llvm::MDBuilder(*context).createBranchWeights(1, 1024)));
			builder.SetInsertPoint(if_zero);

			// if_zero: skip_jump = 0; (in case the store is in a delay slot)
			// return;
			if (delay_slot)
				FAIL_IF(!builder.CreateStore(builder.getInt32(0), llvm_skip_jump));
			FAIL_IF(!fnData.branchToStore(builder));

			// if_nonzero:
			builder.SetInsertPoint(if_nonzero);
			if (!delay_slot)
				FAIL_IF(!fnData.branchN64(builder, n64_insn->addr + 4));
			break;
		}

		/* ... */

		case N64_OP_MFHI:
		{
			llvm::Value* hi = fnData.loadHI(builder);
			FAIL_IF(!hi);
			FAIL_IF(!fnData.storeN64Int(builder, n64_insn->rd, hi));
			if (!delay_slot)
				FAIL_IF(!fnData.branchN64(builder, n64_insn->addr + 4));
			break;
		}

		case N64_OP_MFLO:
		{
			llvm::Value* lo = fnData.loadLO(builder);
			FAIL_IF(!lo);
			FAIL_IF(!fnData.storeN64Int(builder, n64_insn->rd, lo));
			if (!delay_slot)
				FAIL_IF(!fnData.branchN64(builder, n64_insn->addr + 4));
			break;
		}

		case N64_OP_MTHI:
		{
			llvm::Value* a = fnData.loadN64Int(builder, n64_insn->rs);
			FAIL_IF(!a);
			FAIL_IF(!fnData.storeHI(builder, a));
			if (!delay_slot)
				FAIL_IF(!fnData.branchN64(builder, n64_insn->addr + 4));
			break;
		}

		case N64_OP_MTLO:
		{
			llvm::Value* a = fnData.loadN64Int(builder, n64_insn->rs);
			FAIL_IF(!a);
			FAIL_IF(!fnData.storeLO(builder, a));
			if (!delay_slot)
				FAIL_IF(!fnData.branchN64(builder, n64_insn->addr + 4));
			break;
		}

		case N64_OP_MULT:
		case N64_OP_MULTU:
		{
			// This is a 32-bit by 32-bit (un)signed multiplication with
			// 64-bit result. The upper and lower 32 bits go into HI and LO,
			// both sign-extended to 64 bits independently from each other.
			llvm::Value* a32 = fnData.loadN64Int(builder, n64_insn->rs, 32);
			llvm::Value* b32 = fnData.loadN64Int(builder, n64_insn->rt, 32);
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
			FAIL_IF(!fnData.storeHI(builder, hi64));
			FAIL_IF(!fnData.storeLO(builder, lo64));
			if (!delay_slot)
				FAIL_IF(!fnData.branchN64(builder, n64_insn->addr + 4));
			break;
		}

		/* ... */

		case N64_OP_DMULT:
		case N64_OP_DMULTU:
		{
			// This is a 64-bit by 64-bit (un)signed multiplication with
			// 128-bit result. The upper and lower 64 bits go into HI and LO.
			llvm::Value* a64 = fnData.loadN64Int(builder, n64_insn->rs);
			llvm::Value* b64 = fnData.loadN64Int(builder, n64_insn->rt);
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
			llvm::Value* hi128 = builder.CreateAShr(result128, 64);
			FAIL_IF(!hi128);
			llvm::Value* hi64 = builder.CreateTrunc(hi128, builder.getInt64Ty());
			llvm::Value* lo64 = builder.CreateTrunc(result128, builder.getInt64Ty());
			FAIL_IF(!hi64 || !lo64);
			FAIL_IF(!fnData.storeHI(builder, hi64));
			FAIL_IF(!fnData.storeLO(builder, lo64));
			if (!delay_slot)
				FAIL_IF(!fnData.branchN64(builder, n64_insn->addr + 4));
			break;
		}

		/* ... */

		case N64_OP_J:
		case N64_OP_JAL:
		{
			if (n64_insn->opcode == N64_OP_JAL) {
				FAIL_IF(!fnData.storeN64Int(builder, 31,
					builder.getInt64((int64_t) ((int32_t) n64_insn->addr + 8))));
			}
			FAIL_IF(!llvm_ir_for_delay_slot(builder, fnData, n64_insn + 1));
			// Count += cycles for [last_addr .. compile-time PC + 8]
			FAIL_IF(!llvm_ir_for_update_count(builder, fnData, n64_insn->addr + 8));
			// last_addr = compile-time branch target
			FAIL_IF(!llvm_ir_for_update_last_addr(builder, fnData, n64_insn->target));
			// if interrupt:
			//   set global PC variable; flag pending interrupt; goto store
			FAIL_IF(!llvm_ir_for_interrupt_check(builder, fnData, n64_insn,
				n64_insn->runtime + (((int32_t) n64_insn->target - (int32_t) n64_insn->addr) / 4)
			));
			// else:
			//   go to a basic block in this function
			//   -or-
			//   PC = compile-time branch target precomp_instr*; goto store
			llvm::BasicBlock* target_block = fnData.getOpcodeBlock(n64_insn->target);
			if (target_block) {
				FAIL_IF(!builder.CreateBr(target_block));
			} else {
				FAIL_IF(!fnData.setPC(builder,
					n64_insn->runtime + (((int32_t) n64_insn->target - (int32_t) n64_insn->addr) / 4)));
				FAIL_IF(!fnData.branchToStore(builder));
			}
			break;
		}

		/* ... */

		case N64_OP_BLTZ:
		case N64_OP_BLTZAL:
		case N64_OP_BGEZ:
		case N64_OP_BGEZAL:
		case N64_OP_BLEZ:
		case N64_OP_BGTZ:
		case N64_OP_BNE:
		case N64_OP_BEQ:
		{
			llvm::Value* cond = NULL;
			switch (n64_insn->opcode) {
				case N64_OP_BLTZAL:
					FAIL_IF(!fnData.storeN64Int(builder, 31,
						builder.getInt64((int64_t) ((int32_t) n64_insn->addr + 8))));
					/* fall through / 'but also...' */
				case N64_OP_BLTZ:
				{
					llvm::Value* a = fnData.loadN64Int(builder, n64_insn->rs);
					FAIL_IF(!a);
					cond = builder.CreateICmpSLT(a, builder.getInt64(0));
					break;
				}
				case N64_OP_BGEZAL:
					FAIL_IF(!fnData.storeN64Int(builder, 31,
						builder.getInt64((int64_t) ((int32_t) n64_insn->addr + 8))));
					/* fall through / 'but also...' */
				case N64_OP_BGEZ:
				{
					llvm::Value* a = fnData.loadN64Int(builder, n64_insn->rs);
					FAIL_IF(!a);
					cond = builder.CreateICmpSGE(a, builder.getInt64(0));
					break;
				}
				case N64_OP_BLEZ:
				{
					llvm::Value* a = fnData.loadN64Int(builder, n64_insn->rs);
					FAIL_IF(!a);
					cond = builder.CreateICmpSLE(a, builder.getInt64(0));
					break;
				}
				case N64_OP_BGTZ:
				{
					llvm::Value* a = fnData.loadN64Int(builder, n64_insn->rs);
					FAIL_IF(!a);
					cond = builder.CreateICmpSGT(a, builder.getInt64(0));
					break;
				}
				case N64_OP_BNE:
				{
					llvm::Value* a = fnData.loadN64Int(builder, n64_insn->rs);
					llvm::Value* b = fnData.loadN64Int(builder, n64_insn->rt);
					FAIL_IF(!a || !b);
					cond = builder.CreateICmpNE(a, b);
					break;
				}
				case N64_OP_BEQ:
				{
					llvm::Value* a = fnData.loadN64Int(builder, n64_insn->rs);
					llvm::Value* b = fnData.loadN64Int(builder, n64_insn->rt);
					FAIL_IF(!a || !b);
					cond = builder.CreateICmpEQ(a, b);
					break;
				}
				default: break;
			}
			FAIL_IF(!cond);
			// Now that we've gathered the condition (for the value of 'rs',
			// and maybe 'rt', before the delay slot), perform the delay slot.
			FAIL_IF(!llvm_ir_for_delay_slot(builder, fnData, n64_insn + 1));

			llvm::BasicBlock* if_true = llvm::BasicBlock::Create(
				*context, llvm::Twine(nameForAddr(n64_insn->addr)).concat("_True"),
				builder.GetInsertBlock()->getParent());
			FAIL_IF(!if_true);
			// If true, the uninterrupted run of instructions will end, so
			// update last_addr, check for interrupts and go to the target.
			// If false, go to PC + 2, which is guaranteed to be inside this
			// block or to lead to the "fallthrough block" (see llvm_aux.cpp).
			// Neither path is more likely.
			FAIL_IF(!builder.CreateCondBr(cond,
				if_true,
				fnData.getOpcodeBlock(n64_insn->addr + 8)));

			// if_true:
			builder.SetInsertPoint(if_true);
			//   Count += cycles for [last_addr .. compile-time PC + 8]
			FAIL_IF(!llvm_ir_for_update_count(builder, fnData, n64_insn->addr + 8));
			//   last_addr = compile-time branch target
			FAIL_IF(!llvm_ir_for_update_last_addr(builder, fnData, n64_insn->target));
			//   if interrupt:
			//     set global PC variable; flag pending interrupt; goto store
			FAIL_IF(!llvm_ir_for_interrupt_check(builder, fnData, n64_insn,
				n64_insn->runtime + (((int32_t) n64_insn->target - (int32_t) n64_insn->addr) / 4)
			));
			//   else:
			//     go to a basic block in this function
			//     -or-
			//     PC = compile-time branch target precomp_instr*; goto store
			llvm::BasicBlock* target_block = fnData.getOpcodeBlock(n64_insn->target);
			if (target_block) {
				FAIL_IF(!builder.CreateBr(target_block));
			} else {
				FAIL_IF(!fnData.setPC(builder,
					n64_insn->runtime + (((int32_t) n64_insn->target - (int32_t) n64_insn->addr) / 4)));
				FAIL_IF(!fnData.branchToStore(builder));
			}
			break;
		}

		case N64_OP_BLTZL:
		case N64_OP_BLTZALL:
		case N64_OP_BGEZL:
		case N64_OP_BGEZALL:
		case N64_OP_BLEZL:
		case N64_OP_BGTZL:
		case N64_OP_BNEL:
		case N64_OP_BEQL:
		{
			llvm::Value* cond = NULL;
			switch (n64_insn->opcode) {
				case N64_OP_BLTZALL:
					FAIL_IF(!fnData.storeN64Int(builder, 31,
						builder.getInt64((int64_t) ((int32_t) n64_insn->addr + 8))));
					/* fall through / 'but also...' */
				case N64_OP_BLTZL:
				{
					llvm::Value* a = fnData.loadN64Int(builder, n64_insn->rs);
					FAIL_IF(!a);
					cond = builder.CreateICmpSLT(a, builder.getInt64(0));
					break;
				}
				case N64_OP_BGEZALL:
					FAIL_IF(!fnData.storeN64Int(builder, 31,
						builder.getInt64((int64_t) ((int32_t) n64_insn->addr + 8))));
					/* fall through / 'but also...' */
				case N64_OP_BGEZL:
				{
					llvm::Value* a = fnData.loadN64Int(builder, n64_insn->rs);
					FAIL_IF(!a);
					cond = builder.CreateICmpSGE(a, builder.getInt64(0));
					break;
				}
				case N64_OP_BLEZL:
				{
					llvm::Value* a = fnData.loadN64Int(builder, n64_insn->rs);
					FAIL_IF(!a);
					cond = builder.CreateICmpSLE(a, builder.getInt64(0));
					break;
				}
				case N64_OP_BGTZL:
				{
					llvm::Value* a = fnData.loadN64Int(builder, n64_insn->rs);
					FAIL_IF(!a);
					cond = builder.CreateICmpSGT(a, builder.getInt64(0));
					break;
				}
				case N64_OP_BNEL:
				{
					llvm::Value* a = fnData.loadN64Int(builder, n64_insn->rs);
					llvm::Value* b = fnData.loadN64Int(builder, n64_insn->rt);
					FAIL_IF(!a || !b);
					cond = builder.CreateICmpNE(a, b);
					break;
				}
				case N64_OP_BEQL:
				{
					llvm::Value* a = fnData.loadN64Int(builder, n64_insn->rs);
					llvm::Value* b = fnData.loadN64Int(builder, n64_insn->rt);
					FAIL_IF(!a || !b);
					cond = builder.CreateICmpEQ(a, b);
					break;
				}
				default: break;
			}
			FAIL_IF(!cond);

			llvm::BasicBlock* if_true = llvm::BasicBlock::Create(
				*context, llvm::Twine(nameForAddr(n64_insn->addr)).concat("_True"),
				builder.GetInsertBlock()->getParent());
			FAIL_IF(!if_true);
			// If true, the uninterrupted run of instructions will end, so
			// update last_addr, check for interrupts and go to the target.
			// If false, go to PC + 2, which is guaranteed to be inside this
			// block or to lead to the "fallthrough block" (see llvm_aux.cpp).
			// The True path is more likely, as asserted by the MIPS code.
			// But we don't know how much more likely it is. Let's say 8x.
			FAIL_IF(!builder.CreateCondBr(cond,
				if_true,
				fnData.getOpcodeBlock(n64_insn->addr + 8),
				llvm::MDBuilder(*context).createBranchWeights(8, 1)));

			// if_true:
			builder.SetInsertPoint(if_true);
			// Branch Likely opcodes only execute their delay slot if they
			// succeed. So do that here.
			FAIL_IF(!llvm_ir_for_delay_slot(builder, fnData, n64_insn + 1));
			//   Count += cycles for [last_addr .. compile-time PC + 8]
			FAIL_IF(!llvm_ir_for_update_count(builder, fnData, n64_insn->addr + 8));
			//   last_addr = compile-time branch target
			FAIL_IF(!llvm_ir_for_update_last_addr(builder, fnData, n64_insn->target));
			//   if interrupt:
			//     set global PC variable; flag pending interrupt; goto store
			FAIL_IF(!llvm_ir_for_interrupt_check(builder, fnData, n64_insn,
				n64_insn->runtime + (((int32_t) n64_insn->target - (int32_t) n64_insn->addr) / 4)
			));
			//   else:
			//     go to a basic block in this function
			//     -or-
			//     PC = compile-time branch target precomp_instr*; goto store
			llvm::BasicBlock* target_block = fnData.getOpcodeBlock(n64_insn->target);
			if (target_block) {
				FAIL_IF(!builder.CreateBr(target_block));
			} else {
				FAIL_IF(!fnData.setPC(builder,
					n64_insn->runtime + (((int32_t) n64_insn->target - (int32_t) n64_insn->addr) / 4)));
				FAIL_IF(!fnData.branchToStore(builder));
			}
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
	FunctionData fnData(n64_insns, n64_insn_count);
	FAIL_IF(!fnData.begin(fn));

	for (uint32_t i = 0; i < n64_insn_count; i++) {
		const n64_insn_t* n64_insn = &n64_insns[i];
		llvm::BasicBlock* block = fnData.getOpcodeBlock(n64_insn->addr);
		fn->getBasicBlockList().push_back(block);
		builder.SetInsertPoint(block);
#ifdef LJ_SHOW_COMPILATION
		if (i > 0 && ((n64_insn - 1)->branch_flags & BRANCH_DELAY_SLOT)) {
			printf("/%s", get_n64_op_name(n64_insn->opcode));
		} else {
			printf(" %s", get_n64_op_name(n64_insn->opcode));
		}
#endif
		FAIL_IF(!llvm_ir_for_insn(builder, fnData, n64_insn, false /* delay slot */));
	}
#ifdef LJ_SHOW_COMPILATION
	printf("\n");
#endif

	return fnData.end();
}
