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

//===--- OpenCL_CPU.h - Implements the OpenCL code generator for CPUs. ---------------===//
//
// This file implements the OpenCL code generator for CPUs.
//
//===---------------------------------------------------------------------------------===//

#ifndef _BACKEND_OPENCL_CPU_H_
#define _BACKEND_OPENCL_CPU_H_

#include "AcceleratorDeviceBase.h"
#include "CodeGeneratorBaseImplT.h"

namespace clang
{
namespace hipacc
{
namespace Backend
{
	class OpenCL_CPU final : public AcceleratorDeviceBase
	{
	private:

		enum class CompilerSwitchTypeEnum
		{
			ExploreConfig,
			UseLocal,
			Vectorize
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

			inline CodeGenerator(::clang::hipacc::CompilerOptions *pCompilerOptions) : BaseType(pCompilerOptions, "OpenCL for CPU", "opencl-cpu", "Emit OpenCL code for CPU devices")
			{
				_InitSwitch< AcceleratorDeviceSwitches::ExploreConfig >( CompilerSwitchTypeEnum::ExploreConfig );
				_InitSwitch< AcceleratorDeviceSwitches::UseLocal	  >( CompilerSwitchTypeEnum::UseLocal );
				_InitSwitch< AcceleratorDeviceSwitches::Vectorize	  >( CompilerSwitchTypeEnum::Vectorize );
			}
		};
	};
} // end namespace Backend
} // end namespace hipacc
} // end namespace clang


#endif  // _BACKEND_OPENCL_CPU_H_

// vim: set ts=2 sw=2 sts=2 et ai:

