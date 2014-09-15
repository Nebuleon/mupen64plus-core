/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *   Mupen64plus - LLVM JIT - LLVM IR utilities                            *
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

#include <stdint.h>
#include <stdio.h> /* sprintf */

#include "llvm/IR/IRBuilder.h" /* IRBuilder<> */

#include "llvm_bridge.h"
#include "llvm_aux.hpp"
#include "../r4300.h" /* get declaration for precomp_instr */

FunctionData::FunctionData(const n64_insn_t* n64_insns, uint32_t n64_insn_count)
	: fn(NULL)
	, load_block(NULL)
	, opcode_blocks(NULL)
	, n64_insns(n64_insns)
	, n64_insn_count(n64_insn_count)
	, store_block(NULL)
	, n64_int_has_alloca(0)
	, n64_int_read(0)
	, n64_int_written(0)
	, n64_hi_has_alloca(false)
	, n64_hi_read(false)
	, n64_hi_written(false)
	, n64_lo_has_alloca(false)
	, n64_lo_read(false)
	, n64_lo_written(false)
{
}

bool FunctionData::begin(llvm::Function* fn)
{
	this->fn = fn;
	// Attach the load block to the function. It is the entry block.
	this->load_block = llvm::BasicBlock::Create(*context, "entry", fn);
	if (!this->load_block) return false;

	// Create blocks for opcodes and the fallthrough, but don't attach them.
	this->opcode_blocks = (llvm::BasicBlock**) malloc((this->n64_insn_count + 1) * sizeof(llvm::BasicBlock*));
	if (!this->opcode_blocks) return false;

	for (uint32_t i = 0; i <= this->n64_insn_count; i++) {
		this->opcode_blocks[i] = llvm::BasicBlock::Create(*context,
			nameForAddr(this->n64_insns[0].addr + i * 4));
		if (!this->opcode_blocks[i]) return false;
	}

	// Create the store block, but don't attach it.
	this->store_block = llvm::BasicBlock::Create(*context, "store");
	if (!this->store_block) return false;

	// Fill the fallthrough block with a PC update and branch to store.
	llvm::IRBuilder<> fallthroughBuilder(this->opcode_blocks[this->n64_insn_count]);
	if (!this->setPC(fallthroughBuilder, this->n64_insns[this->n64_insn_count - 1].runtime + 1)) return false;
	if (!fallthroughBuilder.CreateBr(this->store_block)) return false;

	return true;
}

llvm::BasicBlock* FunctionData::getStoreBlock()
{
	return this->store_block;
}

llvm::BasicBlock* FunctionData::getOpcodeBlock(uint32_t addr)
{
	if (addr < this->n64_insns[0].addr
	 || addr > this->n64_insns[0].addr + this->n64_insn_count * 4)
		return NULL;
	return this->opcode_blocks[(addr - this->n64_insns[0].addr) / 4];
}

bool FunctionData::branchN64(llvm::IRBuilder<>& builder, uint32_t addr)
{
	return builder.CreateBr(this->getOpcodeBlock(addr)) != NULL;
}

bool FunctionData::branchToStore(llvm::IRBuilder<>& builder)
{
	return builder.CreateBr(this->getStoreBlock()) != NULL;
}

bool FunctionData::setPC(llvm::IRBuilder<>& builder, precomp_instr* newPC)
{
	llvm::Value* newPCConstant = llvm::Constant::getIntegerValue(
		/* (1) Type: i8* */
		llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(*context)),
		/* (2) Value: (size: native pointer-sized integer, value: newPC) */
		llvm::APInt(sizeof(precomp_instr*) * 8, (uintptr_t) newPC)
	);
	if (!newPCConstant) return false;
	if (!builder.CreateStore(newPCConstant, llvm_PC)) return false;
	return true;
}

llvm::Value* FunctionData::loadN64Int(llvm::IRBuilder<>& builder, unsigned int src_reg, unsigned int bits)
{
	if (src_reg == 0)
		return llvm::Constant::getIntegerValue(
			llvm::Type::getIntNTy(*context, bits),
			llvm::APInt(bits, 0)
		);

	llvm::IRBuilder<> loadBuilder(this->load_block);
	if (!(this->n64_int_has_alloca & (UINT32_C(1) << src_reg))) {
		this->n64_int_alloca[src_reg] = loadBuilder.CreateAlloca(loadBuilder.getInt64Ty(),
			NULL,
			llvm::Twine("reg").concat(llvm::Twine(src_reg))
		);
		if (!this->n64_int_alloca[src_reg]) return NULL;
		this->n64_int_has_alloca |= UINT32_C(1) << src_reg;
	}

	if (!(this->n64_int_read & (UINT32_C(1) << src_reg))) {
		/* Generate the address of &reg[src_ptr], type i64. */
		llvm::Value* src_reg_ptr = loadBuilder.CreateConstInBoundsGEP2_32(llvm_reg, 0, src_reg);
		if (!src_reg_ptr) return NULL;
		/* Load the 64-bit register. */
		llvm::Value* value = loadBuilder.CreateLoad(src_reg_ptr,
			llvm::Twine("reg").concat(llvm::Twine(src_reg)).concat("Init")
		);
		if (!value) return NULL;
		/* Store its initial value into our alloca. */
		if (!loadBuilder.CreateStore(value, this->n64_int_alloca[src_reg])) return NULL;
		this->n64_int_read |= UINT32_C(1) << src_reg;
	}

	llvm::Value* result64 = builder.CreateLoad(this->n64_int_alloca[src_reg]);
	if (!result64) return NULL;
	return bits < 64
		? builder.CreateTrunc(result64, llvm::Type::getIntNTy(*context, bits))
		: result64;
}

bool FunctionData::storeN64Int(llvm::IRBuilder<>& builder, unsigned int dst_reg, llvm::Value* value, unsigned int bits)
{
	if (dst_reg == 0)
		return true; // Stored successfully, just with no operation

	if (!(this->n64_int_has_alloca & (UINT32_C(1) << dst_reg))) {
		llvm::IRBuilder<> loadBuilder(this->load_block);
		this->n64_int_alloca[dst_reg] = loadBuilder.CreateAlloca(loadBuilder.getInt64Ty(),
			NULL,
			llvm::Twine("reg").concat(llvm::Twine(dst_reg))
		);
		if (!this->n64_int_alloca[dst_reg]) return false;
		this->n64_int_has_alloca |= UINT32_C(1) << dst_reg;
	}

	if (!(this->n64_int_written & (UINT32_C(1) << dst_reg))) {
		llvm::IRBuilder<> storeBuilder(this->store_block);
		llvm::Value* alloca_value = storeBuilder.CreateLoad(this->n64_int_alloca[dst_reg],
			llvm::Twine("reg").concat(llvm::Twine(dst_reg)).concat("Val")
		);
		if (!alloca_value) return false;
		/* Generate the address of &reg[dst_ptr], type i64. */
		llvm::Value* dst_reg_ptr = storeBuilder.CreateConstInBoundsGEP2_32(llvm_reg, 0, dst_reg);
		if (!dst_reg_ptr) return false;
		/* Store the 64-bit register from the alloca. */
		if (!storeBuilder.CreateStore(alloca_value, dst_reg_ptr)) return false;
		this->n64_int_written |= UINT32_C(1) << dst_reg;
	}

	llvm::Value* value64 = bits < 64
		? builder.CreateSExt(value, builder.getInt64Ty())
		: value;
	if (!value64) return false;
	return builder.CreateStore(value64, this->n64_int_alloca[dst_reg]) != NULL;
}

llvm::Value* FunctionData::loadHI(llvm::IRBuilder<>& builder)
{
	llvm::IRBuilder<> loadBuilder(this->load_block);
	if (!this->n64_hi_has_alloca) {
		this->n64_hi_alloca = loadBuilder.CreateAlloca(loadBuilder.getInt64Ty(),
			NULL,
			"hi"
		);
		if (!this->n64_hi_alloca) return NULL;
		this->n64_hi_has_alloca = true;
	}

	if (!this->n64_hi_read) {
		llvm::Value* value = loadBuilder.CreateLoad(llvm_hi, "hiInit");
		if (!value) return NULL;
		if (!loadBuilder.CreateStore(value, this->n64_hi_alloca)) return NULL;
		this->n64_hi_read = true;
	}

	return builder.CreateLoad(this->n64_hi_alloca);
}

bool FunctionData::storeHI(llvm::IRBuilder<>& builder, llvm::Value* value)
{
	if (!this->n64_hi_has_alloca) {
		llvm::IRBuilder<> loadBuilder(this->load_block);
		this->n64_hi_alloca = loadBuilder.CreateAlloca(loadBuilder.getInt64Ty(),
			NULL,
			"hi"
		);
		if (!this->n64_hi_alloca) return false;
		this->n64_hi_has_alloca = true;
	}

	if (!this->n64_hi_written) {
		llvm::IRBuilder<> storeBuilder(this->store_block);
		llvm::Value* alloca_value = storeBuilder.CreateLoad(this->n64_hi_alloca, "hiVal");
		if (!alloca_value) return false;
		if (!storeBuilder.CreateStore(alloca_value, llvm_hi)) return false;
		this->n64_hi_written = true;
	}

	return builder.CreateStore(value, this->n64_hi_alloca) != NULL;
}

llvm::Value* FunctionData::loadLO(llvm::IRBuilder<>& builder)
{
	llvm::IRBuilder<> loadBuilder(this->load_block);
	if (!this->n64_lo_has_alloca) {
		this->n64_lo_alloca = loadBuilder.CreateAlloca(loadBuilder.getInt64Ty(),
			NULL,
			"lo"
		);
		if (!this->n64_lo_alloca) return NULL;
		this->n64_lo_has_alloca = true;
	}

	if (!this->n64_lo_read) {
		llvm::Value* value = loadBuilder.CreateLoad(llvm_lo, "loInit");
		if (!value) return NULL;
		if (!loadBuilder.CreateStore(value, this->n64_lo_alloca)) return NULL;
		this->n64_lo_read = true;
	}

	return builder.CreateLoad(this->n64_lo_alloca);
}

bool FunctionData::storeLO(llvm::IRBuilder<>& builder, llvm::Value* value)
{
	if (!this->n64_lo_has_alloca) {
		llvm::IRBuilder<> loadBuilder(this->load_block);
		this->n64_lo_alloca = loadBuilder.CreateAlloca(loadBuilder.getInt64Ty(),
			NULL,
			"lo"
		);
		if (!this->n64_lo_alloca) return false;
		this->n64_lo_has_alloca = true;
	}

	if (!this->n64_lo_written) {
		llvm::IRBuilder<> storeBuilder(this->store_block);
		llvm::Value* alloca_value = storeBuilder.CreateLoad(this->n64_lo_alloca, "loVal");
		if (!alloca_value) return false;
		if (!storeBuilder.CreateStore(alloca_value, llvm_lo)) return false;
		this->n64_lo_written = true;
	}

	return builder.CreateStore(value, this->n64_lo_alloca) != NULL;
}

bool FunctionData::end()
{
	// End of load block goes to the first opcode.
	llvm::IRBuilder<> loadBuilder(this->load_block);
	if (!loadBuilder.CreateBr(this->opcode_blocks[0])) return false;

	// Fallthrough block needs to be attached to the function.
	fn->getBasicBlockList().push_back(this->opcode_blocks[this->n64_insn_count]);

	// Store block needs to be given 'ret void' and attached to the function.
	llvm::IRBuilder<> storeBuilder(this->store_block);
	if (!storeBuilder.CreateRetVoid()) return false;
	fn->getBasicBlockList().push_back(store_block);

	return true;
}

FunctionData::~FunctionData()
{
	free(this->opcode_blocks);
}

std::string nameForAddr(uint32_t addr)
{
	char name[9];
	sprintf(name, "%.8X", addr);
	return std::string(name);
}
