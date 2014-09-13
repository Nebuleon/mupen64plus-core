/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *   Mupen64plus - LLVM JIT - Initialisation and finalisation code         *
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

#ifndef __LLVM_JIT_BRIDGE_H__
#define __LLVM_JIT_BRIDGE_H__

#include <stdint.h>

#include "n64ops.h"

/* This file contains prototypes for the LLVM JIT functions accessible to both
 * C++ (which needs to be made aware that its name-mangling is not wanted) and
 * C (which is running the show, but cannot call C++ LLVM functions).
 * The implementations of these prototypes are in llvm_*.cpp. */

#ifdef __cplusplus
extern "C" {
#else
#  include <stdbool.h>
#endif

// Type of a function that is called without arguments and returns nothing.
// This is the type of function that backs cached interpreter opcodes, as well
// as LLVM-generated functions that back JIT-compiled opcodes.
typedef void (*op_func_t) (void);

int llvm_init();
int llvm_init_globals();
void llvm_exit();
void llvm_destroy_globals();

/* Creates an LLVM Function object, named after the passed-in Program Counter
 * value and containing a basic block called 'entry', and returns it.
 * The return type should be llvm::Function*, but C can't deal with that. */
void* llvm_function_create(uint32_t addr);

/*
 * Adds IR to an LLVM Function object to replicate the effect of the N64
 * opcodes starting at 'n64_insns' and going on for 'n64_insn_count' opcodes.
 * 
 * The function may decide, at run time, that a branch, jump or exception must
 * occur, and that a new block is next in line to be executed. It may do this
 * in two ways:
 * a) Set the 'PC' variable to the address of the precomp_instr structure
 *    backing the next opcode to be executed, then return;
 *    -or-
 * b) If the function implementing the opcode following, in program order, the
 *    last executed opcode is a native function, tail-call (i.e. jump to) it;
 *    -or-
 * b) If the function implementing the opcode following, in program order, the
 *    last executed opcode is backed by the cached interpreter, set the 'PC'
 *    variable to the address of its precomp_instr structure, then tail-call
 *    (i.e. jump to) it.
 * 
 * At the end of the function, it is as if a branch occurs to the next
 * location to be executed in program order.
 * 
 * In:
 *   fn_ptr: Pointer to the LLVM Function object to fill with IR.
 *     The type of this argument should be llvm::Function*, but C can't deal
 *     with that.
 *   n64_insns: Pointer to the start of the N64 instruction structures (from
 *     n64ops.c) to use as opcodes.
 *   n64_insn_count: Number of valid N64 instruction structures after
 *    'n64_insns'.
 * Returns:
 *   true if compilation succeeded; false if there was a problem.
 * Input assertions:
 *   a) fn_ptr != NULL.
 *   b) fn_ptr was returned by an earlier call to 'llvm_function_create' and
 *      has not been passed to 'llvm_function_delete'.
 *   c) n64_insns != NULL.
 *   d) n64_insns[0] to n64_insns[n64_insn_count - 1] are dereferenceable.
 *   e) n64_insn_count > 0.
 * Output assertions:
 *   The LLVM Function object, initially empty, is filled with IR for a valid
 *   function that performs the function of at least the first opcode in
 *   'n64_insns'.
 */
bool llvm_ir_for_n64(void* fn_ptr, const n64_insn_t* n64_insns, uint32_t n64_insn_count);

/* Returns a pointer to the native function emitted by LLVM for the passed-in
 * LLVM Function.
 * The argument type should be llvm::Function*, but C can't deal with that. */
op_func_t llvm_function_emit(void* fn_ptr);

/* Deletes the passed-in LLVM Function object. After a call to this function,
 * the passed-in object and anything derived from it is invalid.
 * The argument type should be llvm::Function*, but C can't deal with that. */
void llvm_function_delete(void* fn_ptr);

#ifdef __cplusplus
} /* extern "C" */

#  include "llvm/IR/LLVMContext.h" /* LLVMContext */
#  include "llvm/ExecutionEngine/ExecutionEngine.h" /* EngineBuilder etc. */
#  include "llvm/IR/GlobalVariable.h" /* GlobalVariable */
#  include "llvm/IR/Module.h" /* Module */

extern llvm::LLVMContext* context;
extern llvm::Module* code_cache;
extern llvm::ExecutionEngine* engine;

extern llvm::GlobalVariable* llvm_reg; /* GlobalVariable representing 'reg' */
extern llvm::GlobalVariable* llvm_hi; /* GlobalVariable representing 'hi' */
extern llvm::GlobalVariable* llvm_lo; /* GlobalVariable representing 'lo' */
extern llvm::GlobalVariable* llvm_PC; /* GlobalVariable representing 'PC' */
extern llvm::GlobalVariable* llvm_readmemb;
extern llvm::GlobalVariable* llvm_readmemh;
extern llvm::GlobalVariable* llvm_readmem;
extern llvm::GlobalVariable* llvm_readmemd;
extern llvm::GlobalVariable* llvm_writememb;
extern llvm::GlobalVariable* llvm_writememh;
extern llvm::GlobalVariable* llvm_writemem;
extern llvm::GlobalVariable* llvm_writememd;
extern llvm::GlobalVariable* llvm_address;
extern llvm::GlobalVariable* llvm_rdword;
extern llvm::GlobalVariable* llvm_cpu_byte;
extern llvm::GlobalVariable* llvm_hword;
extern llvm::GlobalVariable* llvm_word;
extern llvm::GlobalVariable* llvm_dword;
extern llvm::GlobalVariable* llvm_invalid_code;
extern llvm::GlobalVariable* llvm_skip_jump;
#endif /* __cplusplus */

#endif /* !__LLVM_JIT_BRIDGE_H__ */
