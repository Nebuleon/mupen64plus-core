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
#include "../r4300.h" /* get declaration for PC, reg, hi, lo */

llvm::GlobalVariable* llvm_reg;
llvm::GlobalVariable* llvm_hi;
llvm::GlobalVariable* llvm_lo;
llvm::GlobalVariable* llvm_PC;

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
	engine->updateGlobalMapping(llvm_reg, reg);
	engine->updateGlobalMapping(llvm_hi, &hi);
	engine->updateGlobalMapping(llvm_lo, &lo);
	engine->updateGlobalMapping(llvm_PC, &PC);
	return 0;
}

void llvm_destroy_globals()
{
	llvm_reg->eraseFromParent();
	llvm_hi->eraseFromParent();
	llvm_lo->eraseFromParent();
	llvm_PC->eraseFromParent();
	llvm_reg = llvm_hi = llvm_lo = llvm_PC = NULL;
}
