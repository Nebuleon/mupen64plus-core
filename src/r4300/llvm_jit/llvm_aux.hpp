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

#include <stdbool.h>
#include <stdint.h> /* to get [u]intN_t in the global namespace */

#include "llvm/IR/IRBuilder.h" /* IRBuilder<> */

#include "n64ops.h"

class FunctionData {
private:
	llvm::Function* fn;

	// Basic block containing 'alloca' instructions to allocate stack slots
	// for N64 registers used in the function. It will also contain 'load' and
	// 'store' instructions to fill the slots with the current value of the
	// N64 registers, if they are ever read in the function. This is the entry
	// block. It is given its branch to the basic block backing the first
	// opcode in the end() method.
	llvm::BasicBlock* load_block;

	// Basic blocks backing the opcodes. Each of these implement one opcode.
	// For the branch opcodes having a delay slot, the delay slot is emitted
	// along with the condition and its use, and the delay slot is emitted on
	// its own in the following basic block, in case it is jumped to.
	// Each is attached to the function where it is needed.
	// There is one more block to correspond to the opcode after the last one.
	// If reached, it will set the 'PC' variable to the address of the
	// 'precomp_instr' structure that would implement the opcode found there,
	// then go to the store_block.
	llvm::BasicBlock** opcode_blocks;
	const n64_insn_t* n64_insns;
	uint32_t n64_insn_count;

	// Basic block containing 'load' instructions from the allocas and 'store'
	// instructions to memory. This is the exit block. It is given 'ret void'
	// in the end() method, and is not attached to the function before the
	// end() method either.
	llvm::BasicBlock* store_block;

	uint32_t n64_int_has_alloca; // bit N is set if n64_int_alloca[N] is valid
	uint32_t n64_int_written; // bit N is set if reg[N] was stored in footer
	llvm::Value* n64_int_alloca[32]; // 'alloca i64' instructions for GPRs

	uint32_t n64_cp0_has_alloca; // bit N is set if n64_cp0_alloca[N] is valid
	uint32_t n64_cp0_written; // bit N is set if CP0 N was stored in footer
	llvm::Value* n64_cp0_alloca[32]; // 'alloca i64' instructions for GPRs

	bool n64_hi_has_alloca;
	bool n64_hi_written;
	llvm::Value* n64_hi_alloca;

	bool n64_lo_has_alloca;
	bool n64_lo_written;
	llvm::Value* n64_lo_alloca;

public:
	FunctionData(const n64_insn_t* n64_insns, uint32_t n64_insn_count);
	~FunctionData();

	// Creates basic blocks (not attaching them, except the load block) for
	// the given Function.
	// Returns false if there was insufficient memory.
	bool begin(llvm::Function* fn);

	// Returns the store block. This is the target of LLVM branches that
	// follow N64 exceptions.
	llvm::BasicBlock* getStoreBlock();

	// Returns the basic block (attached or not) backing the opcode at the
	// specified N64 address. The address may be that of the fallthrough
	// block. Returns NULL if the address is out of bounds for the function
	// being compiled.
	llvm::BasicBlock* getOpcodeBlock(uint32_t addr);

	// Emits a branch to the basic block backing the opcode at the given N64
	// address.
	// Returns false if there was insufficient memory.
	bool branchN64(llvm::IRBuilder<>& builder, uint32_t addr);

	// Emits a branch to the store block. This is most useful after N64
	// exceptions.
	// Returns false if there was insufficient memory.
	bool branchToStore(llvm::IRBuilder<>& builder);

	// Emits an update of the Program Counter.
	// Returns false if there was insufficient memory.
	bool setPC(llvm::IRBuilder<>& builder, precomp_instr* newPC);

	// Requests loading the value of one of the N64 GPRs. An alloca is made
	// and loaded with the initial value of the GPR in the load block if
	// needed.
	// 'bits' is the number of lower bits required from the GPR; a truncation
	// is written if needed.
	// Returns NULL if there was insufficient memory.
	llvm::Value* loadN64Int(llvm::IRBuilder<>& builder, unsigned int src_reg, unsigned int bits = 64);

	// Requests storing a value into one of the N64 GPRs. An alloca is made
	// in the load block and written back in the store block if needed.
	// 'bits' is the number of bits in 'value'; a sign-extension is written
	// if needed.
	// Returns false if there was insufficient memory.
	bool storeN64Int(llvm::IRBuilder<>& builder, unsigned int dst_reg, llvm::Value* value, unsigned int bits = 64);

	// Requests loading the value of one of the N64 Coprocessor 0 registers.
	// An alloca is made and loaded with the initial value of the register in
	// the load block if needed. The loaded value is always 32 bits.
	// Returns NULL if there was insufficient memory.
	llvm::Value* loadN64Cop0(llvm::IRBuilder<>& builder, unsigned int src_reg);

	// Requests storing a value into one of the N64 Coprocessor 0 registers.
	// An alloca is made in the load block and written back in the store block
	// if needed. The stored value must be 32-bit.
	// Returns false if there was insufficient memory.
	bool storeN64Cop0(llvm::IRBuilder<>& builder, unsigned int dst_reg, llvm::Value* value);

	// Requests loading the value of the multiplier unit's HI register.
	// An alloca is made and loaded with the initial value of HI in the load
	// block if needed.
	// Returns NULL if there was insufficient memory.
	llvm::Value* loadHI(llvm::IRBuilder<>& builder);

	// Requests storing a value into the multiplier unit's HI register.
	// An alloca is made in the load block and written back in the store block
	// if needed.
	// Returns false if there was insufficient memory.
	bool storeHI(llvm::IRBuilder<>& builder, llvm::Value* value);

	// Requests loading the value of the multiplier unit's LO register.
	// An alloca is made and loaded with the initial value of LO in the load
	// block if needed.
	// Returns NULL if there was insufficient memory.
	llvm::Value* loadLO(llvm::IRBuilder<>& builder);

	// Requests storing a value into the multiplier unit's LO register.
	// An alloca is made in the load block and written back in the store block
	// if needed.
	// Returns false if there was insufficient memory.
	bool storeLO(llvm::IRBuilder<>& builder, llvm::Value* value);

	// Attaches the fallthrough basic block (which sets the 'PC' variable if
	// reached) and the store block to the function, and gives them and the
	// load block their terminator instructions as required by LLVM IR.
	// Returns false if there was insufficient memory.
	bool end();

private:
	// Creates an alloca for an N64 GPR, filled with its current value.
	// Returns false if there was insufficient memory.
	bool allocN64Int(unsigned int src_reg);

	// Creates an alloca for an N64 Coprocessor 0 register, filled with its
	// current value.
	// Returns false if there was insufficient memory.
	bool allocN64Cop0(unsigned int src_reg);

	// Creates an alloca for the multiplier unit's HI register, filled with
	// its current value.
	// Returns false if there was insufficient memory.
	bool allocHI();

	// Creates an alloca for the multiplier unit's LO register, filled with
	// its current value.
	// Returns false if there was insufficient memory.
	bool allocLO();
};

std::string nameForAddr(uint32_t addr, bool delay_slot = false);
