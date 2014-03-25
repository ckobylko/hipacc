//
// Copyright (c) 2012, University of Erlangen-Nuremberg
// Copyright (c) 2012, Siemens AG
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// 1. Redistributions of source code must retain the above copyright notice, this
//    list of conditions and the following disclaimer.
// 2. Redistributions in binary form must reproduce the above copyright notice,
//    this list of conditions and the following disclaimer in the documentation
//    and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
// ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
// ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
// ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//

//===--- Filterscript.h - Implements the Filterscript code generator. ----------------===//
//
// This file implements the Filterscript code generator.
//
//===---------------------------------------------------------------------------------===//

#ifndef _BACKEND_FILTER_SCRIPT_H_
#define _BACKEND_FILTER_SCRIPT_H_

#include "AndroidBase.h"
#include "CodeGeneratorBaseImplT.h"

namespace clang
{
namespace hipacc
{
namespace Backend
{
	class Filterscript final : public AndroidBase
	{
	private:

		enum class CompilerSwitchTypeEnum
		{
			RsPackage
		};


	public:

		class CodeGenerator final : public CodeGeneratorBaseImplT< CompilerSwitchTypeEnum >
		{
		private:

			typedef CodeGeneratorBaseImplT< CompilerSwitchTypeEnum >	BaseType;
			typedef BaseType::CompilerSwitchInfoType					CompilerSwitchInfoType;

		protected:

			virtual size_t _HandleSwitch(CompilerSwitchTypeEnum eSwitch, CommonDefines::ArgumentVectorType &rvecArguments, size_t szCurrentIndex) override;

		public:

			inline CodeGenerator(::clang::hipacc::CompilerOptions *pCompilerOptions) : BaseType(pCompilerOptions, "Filterscript", "filterscript", "Emit Filterscript code for Android")
			{
				_InitSwitch< AndroidSwitches::RsPackage >( CompilerSwitchTypeEnum::RsPackage );
			}
		};

	};
} // end namespace Backend
} // end namespace hipacc
} // end namespace clang


#endif  // _BACKEND_FILTER_SCRIPT_H_

// vim: set ts=2 sw=2 sts=2 et ai:

