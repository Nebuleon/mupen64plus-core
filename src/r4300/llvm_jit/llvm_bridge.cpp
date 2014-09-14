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

#include <string>
#include <stdio.h> /* printf */

#include "llvm/Transforms/Scalar.h" /* declarations for optimisation passes */
#include "llvm/Support/TargetSelect.h" /* InitializeNativeTarget */
#include "llvm/ExecutionEngine/ExecutionEngine.h" /* EngineBuilder etc. */
#include "llvm/ExecutionEngine/JIT.h" /* A static constructor creates a JIT */
#include "llvm/IR/LLVMContext.h" /* LLVMContext, getGlobalContext */
#include "llvm/PassManager.h" /* FunctionPassManager */
#ifdef LJ_VERIFY_LLVM_IR
#  include <iostream> /* cout */
#  include "llvm/Support/raw_os_ostream.h"
#  include "llvm/IR/Verifier.h"
#endif
#include "llvm/Support/ManagedStatic.h" /* llvm_shutdown */
#include "llvm/Target/TargetMachine.h" /* TargetMachine */
#include "llvm/IR/Module.h" /* Module */

#include "llvm_bridge.h"

llvm::LLVMContext* context; // Modules and types are created in this context.
llvm::Module* code_cache; // All functions will be emitted into this.
llvm::ExecutionEngine* engine;

llvm::FunctionType* void_to_void; // Represents a void(void) function in LLVM.

llvm::FunctionPassManager* optimizer;

#ifdef LJ_VERIFY_LLVM_IR
llvm::raw_os_ostream* raw_cout;
#endif

int llvm_init()
{
	context = new llvm::LLVMContext();
	if (!context) {
		printf("LLVM Error: Failed to create the LLVMContext to contain LLVM system data\n");
		return -1;
	}

	code_cache = new llvm::Module("", *context);
	if (!code_cache) {
		printf("LLVM Error: Failed to create the LLVM Module to contain recompiled code\n");
		return -1;
	}

	// This is the execution engine that will either JIT-compile or interpret
	//   (we hope this will be a JIT compiler) the LLVM IR in the code cache.
	llvm::InitializeNativeTarget();

	std::string errorString;
	engine = llvm::EngineBuilder(code_cache)
		.setErrorStr(&errorString)
		.setEngineKind(llvm::EngineKind::JIT)
		.create();
	if (!engine) {
		printf("LLVM Error: Failed to create the LLVM JIT engine: %s\n", errorString.c_str());
		return -1;
	} else {
		llvm::StringRef target = engine->getTargetMachine()->getTargetTriple();
		printf("LLVM Info: Created a LLVM JIT for %s\n",
			std::string(target.data() /* "may not be null-terminated */, target.size()).c_str());
	}

	// Don't look up ANY symbols in our executable. It may be stripped.
	engine->DisableSymbolSearching();

	// Grab the FunctionType of functions:
	// * receiving void (second argument to FunctionType::get); and
	// * returning void (first argument to FunctionType::get).
	// That's all we use, so we can keep a copy of this around.
	std::vector<llvm::Type*> voids;
	void_to_void = llvm::FunctionType::get(
		llvm::Type::getVoidTy(*context), voids, false);

#ifdef LJ_VERIFY_LLVM_IR
	raw_cout = new llvm::raw_os_ostream(std::cout);
#endif

	if (llvm_init_globals() != 0)
		return -1;

	optimizer = new llvm::FunctionPassManager(code_cache);
	if (!optimizer) {
		printf("LLVM Warning: Failed to create the LLVM FunctionPassManager to optimise code\n");
	}
	// Instruction combining must be performed first to reduce the number
	// of instructions that need to be read during further optimisations.
	optimizer->add(llvm::createInstructionCombiningPass());
	// Reduce the number of identical operations performed multiple times
	// on the same operands.
	optimizer->add(llvm::createGVNPass());
	optimizer->doInitialization();

	return 0;
}

void* llvm_function_create(uint32_t addr)
{
	char name[9];
	sprintf(name, "%.8X", addr);
	std::string sName(name);

	llvm::Function* result = llvm::Function::Create(
		void_to_void, llvm::Function::WeakAnyLinkage, sName, code_cache);
	llvm::BasicBlock::Create(*context, "entry", result);
	return result;
}

op_func_t llvm_function_emit(void* fn_ptr)
{
#ifdef LJ_SHOW_LLVM_IR
	((llvm::Function*) fn_ptr)->dump();
#endif

	// Optimise the IR of the function.
	if (optimizer)
		optimizer->run(*(llvm::Function*) fn_ptr);

#ifdef LJ_VERIFY_LLVM_IR
	if (llvm::verifyFunction(*(llvm::Function*) fn_ptr, raw_cout)) {
		printf("LLVM Warning: The preceding LLVM function did not pass verification.\n");
	} else {
		printf("LLVM Info: The preceding LLVM function passed verification.\n");
	}
#endif
	return (op_func_t) engine->getPointerToFunction((llvm::Function*) fn_ptr);
}

void llvm_function_delete(void* fn_ptr)
{
	((llvm::Function*) fn_ptr)->eraseFromParent();
}

void llvm_exit()
{
	llvm_destroy_globals();

	optimizer->doFinalization();
	delete optimizer;
	optimizer = NULL;

	void_to_void = NULL;
	delete engine;
	engine = NULL;
	delete context;
	context = NULL;

#ifdef LJ_VERIFY_LLVM_IR
	delete raw_cout;
	raw_cout = NULL;
#endif

	llvm::llvm_shutdown();
}
