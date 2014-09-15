/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *   Mupen64plus - LLVM JIT - Global variables                             *
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

#include "llvm/IR/GlobalValue.h" /* GlobalValue thread local mode constants */
#include "llvm/IR/GlobalVariable.h" /* GlobalVariable */

#include "llvm_bridge.h"
#include "../r4300.h" /* get declaration for PC, reg, hi, lo, skip_jump */
#include "memory/memory.h" /* get declaration for address, readmem... */
#include "../cached_interp.h" /* get declaration for invalid_code */

llvm::GlobalVariable* llvm_reg;
llvm::GlobalVariable* llvm_hi;
llvm::GlobalVariable* llvm_lo;
llvm::GlobalVariable* llvm_PC;
llvm::GlobalVariable* llvm_readmemb;
llvm::GlobalVariable* llvm_readmemh;
llvm::GlobalVariable* llvm_readmem;
llvm::GlobalVariable* llvm_readmemd;
llvm::GlobalVariable* llvm_writememb;
llvm::GlobalVariable* llvm_writememh;
llvm::GlobalVariable* llvm_writemem;
llvm::GlobalVariable* llvm_writememd;
llvm::GlobalVariable* llvm_address;
llvm::GlobalVariable* llvm_rdword;
llvm::GlobalVariable* llvm_cpu_byte;
llvm::GlobalVariable* llvm_hword;
llvm::GlobalVariable* llvm_word;
llvm::GlobalVariable* llvm_dword;
llvm::GlobalVariable* llvm_invalid_code;
llvm::GlobalVariable* llvm_skip_jump;

int llvm_init_globals()
{
	llvm_reg = new llvm::GlobalVariable(
		*code_cache /* (1) Module which will contain this global variable */,
		/* (2) Type: [32 x i64] */
		llvm::ArrayType::get(llvm::Type::getInt64Ty(*context), 32),
		false /* (3) Not constant */,
		llvm::GlobalValue::ExternalLinkage /* (4) Linkage */,
		NULL /* (5) Initialiser (explicitly uninitialised by LLVM!) */,
		"reg" /* (6) Name */
	);
	if (!llvm_reg) return -1;
	llvm_hi = new llvm::GlobalVariable(
		*code_cache,
		/* (2) Type: i64 */
		llvm::Type::getInt64Ty(*context),
		false, llvm::GlobalValue::ExternalLinkage, NULL, "hi"
	);
	if (!llvm_hi) return -1;
	llvm_lo = new llvm::GlobalVariable(
		*code_cache,
		/* (2) Type: i64 */
		llvm::Type::getInt64Ty(*context),
		false, llvm::GlobalValue::ExternalLinkage, NULL, "lo"
	);
	if (!llvm_lo) return -1;
	llvm_PC = new llvm::GlobalVariable(
		*code_cache,
		/* (2) Type: i8* */
		llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(*context)),
		false, llvm::GlobalValue::ExternalLinkage, NULL, "PC"
	);
	if (!llvm_PC) return -1;
	/* Type: [65536 x void()*] */
	llvm::ArrayType* accessor_array_type = llvm::ArrayType::get(
		llvm::PointerType::getUnqual(llvm::FunctionType::get(llvm::Type::getVoidTy(*context), false)),
		65536);
	llvm_readmemb = new llvm::GlobalVariable(
		*code_cache, accessor_array_type,
		false, llvm::GlobalValue::ExternalLinkage, NULL, "readmemb"
	);
	if (!llvm_readmemb) return -1;
	llvm_readmemh = new llvm::GlobalVariable(
		*code_cache, accessor_array_type,
		false, llvm::GlobalValue::ExternalLinkage, NULL, "readmemh"
	);
	if (!llvm_readmemh) return -1;
	llvm_readmem = new llvm::GlobalVariable(
		*code_cache, accessor_array_type,
		false, llvm::GlobalValue::ExternalLinkage, NULL, "readmem"
	);
	if (!llvm_readmem) return -1;
	llvm_readmemd = new llvm::GlobalVariable(
		*code_cache, accessor_array_type,
		false, llvm::GlobalValue::ExternalLinkage, NULL, "readmemd"
	);
	if (!llvm_readmemd) return -1;
	llvm_writememb = new llvm::GlobalVariable(
		*code_cache, accessor_array_type,
		false, llvm::GlobalValue::ExternalLinkage, NULL, "writememb"
	);
	if (!llvm_writememb) return -1;
	llvm_writememh = new llvm::GlobalVariable(
		*code_cache, accessor_array_type,
		false, llvm::GlobalValue::ExternalLinkage, NULL, "writememh"
	);
	if (!llvm_writememh) return -1;
	llvm_writemem = new llvm::GlobalVariable(
		*code_cache, accessor_array_type,
		false, llvm::GlobalValue::ExternalLinkage, NULL, "writemem"
	);
	if (!llvm_writemem) return -1;
	llvm_writememd = new llvm::GlobalVariable(
		*code_cache, accessor_array_type,
		false, llvm::GlobalValue::ExternalLinkage, NULL, "writememd"
	);
	if (!llvm_writememd) return -1;
	llvm_address = new llvm::GlobalVariable(
		*code_cache,
		/* (2) Type: i32 */
		llvm::Type::getInt32Ty(*context),
		false, llvm::GlobalValue::ExternalLinkage, NULL, "address"
	);
	if (!llvm_address) return -1;
	llvm_rdword = new llvm::GlobalVariable(
		*code_cache,
		/* (2) Type: i64* */
		llvm::PointerType::getUnqual(llvm::Type::getInt64Ty(*context)),
		false, llvm::GlobalValue::ExternalLinkage, NULL, "rdword"
	);
	if (!llvm_rdword) return -1;
	llvm_cpu_byte = new llvm::GlobalVariable(
		*code_cache,
		/* (2) Type: i8 */
		llvm::Type::getInt8Ty(*context),
		false, llvm::GlobalValue::ExternalLinkage, NULL, "cpu_byte"
	);
	if (!llvm_cpu_byte) return -1;
	llvm_hword = new llvm::GlobalVariable(
		*code_cache,
		/* (2) Type: i16 */
		llvm::Type::getInt16Ty(*context),
		false, llvm::GlobalValue::ExternalLinkage, NULL, "hword"
	);
	if (!llvm_hword) return -1;
	llvm_word = new llvm::GlobalVariable(
		*code_cache,
		/* (2) Type: i32 */
		llvm::Type::getInt32Ty(*context),
		false, llvm::GlobalValue::ExternalLinkage, NULL, "word"
	);
	if (!llvm_word) return -1;
	llvm_dword = new llvm::GlobalVariable(
		*code_cache,
		/* (2) Type: i64 */
		llvm::Type::getInt64Ty(*context),
		false, llvm::GlobalValue::ExternalLinkage, NULL, "dword"
	);
	if (!llvm_dword) return -1;
	llvm_invalid_code = new llvm::GlobalVariable(
		*code_cache,
		/* Type: [1048576 x i8] */
		llvm::ArrayType::get(llvm::Type::getInt8Ty(*context), 0x100000),
		false, llvm::GlobalValue::ExternalLinkage, NULL, "invalid_code"
	);
	if (!llvm_invalid_code) return -1;
	llvm_skip_jump = new llvm::GlobalVariable(
		*code_cache,
		/* (2) Type: i32 */
		llvm::Type::getInt32Ty(*context),
		false, llvm::GlobalValue::ExternalLinkage, NULL, "skip_jump"
	);
	if (!llvm_skip_jump) return -1;
#ifdef LJ_SHOW_LLVM_IR
	code_cache->dump();
#endif
	engine->updateGlobalMapping(llvm_reg, reg);
	engine->updateGlobalMapping(llvm_hi, &hi);
	engine->updateGlobalMapping(llvm_lo, &lo);
	engine->updateGlobalMapping(llvm_PC, &PC);
	engine->updateGlobalMapping(llvm_readmemb, readmemb);
	engine->updateGlobalMapping(llvm_readmemh, readmemh);
	engine->updateGlobalMapping(llvm_readmem, readmem);
	engine->updateGlobalMapping(llvm_readmemd, readmemd);
	engine->updateGlobalMapping(llvm_writememb, writememb);
	engine->updateGlobalMapping(llvm_writememh, writememh);
	engine->updateGlobalMapping(llvm_writemem, writemem);
	engine->updateGlobalMapping(llvm_writememd, writememd);
	engine->updateGlobalMapping(llvm_address, &address);
	engine->updateGlobalMapping(llvm_rdword, &rdword);
	engine->updateGlobalMapping(llvm_cpu_byte, &cpu_byte);
	engine->updateGlobalMapping(llvm_hword, &hword);
	engine->updateGlobalMapping(llvm_word, &word);
	engine->updateGlobalMapping(llvm_dword, &dword);
	engine->updateGlobalMapping(llvm_invalid_code, invalid_code);
	engine->updateGlobalMapping(llvm_skip_jump, &skip_jump);
	return 0;
}

void llvm_destroy_globals()
{
	llvm_reg->eraseFromParent();
	llvm_hi->eraseFromParent();
	llvm_lo->eraseFromParent();
	llvm_PC->eraseFromParent();
	llvm_readmemb->eraseFromParent();
	llvm_readmemh->eraseFromParent();
	llvm_readmem->eraseFromParent();
	llvm_readmemd->eraseFromParent();
	llvm_writememb->eraseFromParent();
	llvm_writememh->eraseFromParent();
	llvm_writemem->eraseFromParent();
	llvm_writememd->eraseFromParent();
	llvm_address->eraseFromParent();
	llvm_rdword->eraseFromParent();
	llvm_cpu_byte->eraseFromParent();
	llvm_hword->eraseFromParent();
	llvm_word->eraseFromParent();
	llvm_dword->eraseFromParent();
	llvm_invalid_code->eraseFromParent();
	llvm_skip_jump->eraseFromParent();
	llvm_reg = llvm_hi = llvm_lo = llvm_PC =
		llvm_readmemb = llvm_readmemh = llvm_readmem = llvm_readmemd =
		llvm_writememb = llvm_writememh = llvm_writemem = llvm_writememd =
		llvm_address = llvm_rdword =
		llvm_cpu_byte = llvm_hword = llvm_word = llvm_dword =
		llvm_invalid_code = llvm_skip_jump = NULL;
}
