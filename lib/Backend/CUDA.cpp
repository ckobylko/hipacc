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

//===--- CUDA.cpp - Implements the NVidia CUDA code generator. -----------------------===//
//
// This file implements the NVidia CUDA code generator.
//
//===---------------------------------------------------------------------------------===//

#include "hipacc/Backend/CUDA.h"
#include "hipacc/Device/TargetDescription.h"

using namespace clang::hipacc::Backend;
using namespace std;

size_t CUDA::CodeGenerator::_HandleSwitch(CompilerSwitchTypeEnum eSwitch, CommonDefines::ArgumentVectorType &rvecArguments, size_t szCurrentIndex)
{
	string	strCurrentSwitch = rvecArguments[szCurrentIndex];
	size_t	szReturnIndex = szCurrentIndex;

	switch (eSwitch)
	{
	case CompilerSwitchTypeEnum::EmitPadding:
		{
			string strOption = _FetchSwitchOption(rvecArguments, szCurrentIndex);
			GetCompilerOptions().setPadding(_ParseIntegerOption(strOption, strCurrentSwitch));
			++szReturnIndex;
		}
		break;
	case CompilerSwitchTypeEnum::ExploreConfig:
		{
			GetCompilerOptions().setExploreConfig(USER_ON);
		}
		break;
	case CompilerSwitchTypeEnum::PixelsPerThread:
		{
			string strOption = _FetchSwitchOption(rvecArguments, szCurrentIndex);
			GetCompilerOptions().setPixelsPerThread(_ParseIntegerOption(strOption, strCurrentSwitch));
			++szReturnIndex;
		}
		break;
	case CompilerSwitchTypeEnum::Target:
		{
			string strOption = _FetchSwitchOption(rvecArguments, szCurrentIndex);
			GetCompilerOptions().setTargetDevice(AcceleratorDeviceBase::_ParseTargetOption(strOption));
			++szReturnIndex;
		}
		break;
	case CompilerSwitchTypeEnum::TimeKernels:
		{
			GetCompilerOptions().setTimeKernels(USER_ON);
		}
		break;
	case CompilerSwitchTypeEnum::UseConfig:
		{
			string strOption = _FetchSwitchOption(rvecArguments, szCurrentIndex);
		
			int x = 0, y = 0;
			if (sscanf(strOption.c_str(), "%dx%d", &x, &y) != 2)
			{
				throw RuntimeErrorException("ERROR: Expected valid configuration specification for -use-config switch.\n\n");
			}

			GetCompilerOptions().setKernelConfig(x, y);
			++szReturnIndex;
		}
		break;
	case CompilerSwitchTypeEnum::UseLocal:
		{
			string strOption = _FetchSwitchOption(rvecArguments, szCurrentIndex);
			GetCompilerOptions().setLocalMemory(_ParseOnOffOption(strOption, strCurrentSwitch));
			++szReturnIndex;
		}
	case CompilerSwitchTypeEnum::UseTextures:
		{
			string strOption = _FetchSwitchOption(rvecArguments, szCurrentIndex);
			GetCompilerOptions().setTextureMemory(AcceleratorDeviceBase::_ParseTextureOption(strOption));
			++szReturnIndex;
		}
		break;
	case CompilerSwitchTypeEnum::Vectorize:
		{
			string strOption = _FetchSwitchOption(rvecArguments, szCurrentIndex);
			GetCompilerOptions().setVectorizeKernels(_ParseOnOffOption(strOption, strCurrentSwitch));
			++szReturnIndex;
		}
		break;
	default:	throw UnhandledSwitchException(strCurrentSwitch, GetName());
	}

	return szReturnIndex;
}

void CUDA::CodeGenerator::_CheckConfiguration()
{
	// Check base configuration
	BaseType::_CheckConfiguration();


	HipaccDevice ConfiguredTargetDevive(GetCompilerOptions());

	// Check target device
	if (! ConfiguredTargetDevive.isNVIDIAGPU())
	{
		throw RuntimeErrorException("ERROR: CUDA code generation selected, but no CUDA - capable target device specified!\n  Please select correct target device/code generation back end combination.\n\n");
	}


	// Check texture support
	if (GetCompilerOptions().useTextureMemory(USER_ON))
	{
		// Writing to Array2D textures has been introduced with Fermi architecture
		if ( (GetCompilerOptions().getTextureType() == Array2D) && (GetCompilerOptions().getTargetDevice() < FERMI_20) )
		{
			llvm::errs() << "Warning: 'Array2D' texture memory only supported for Fermi and later on (CC >= 2.0)!  Using 'Linear2D' instead!\n";

			GetCompilerOptions().setTextureMemory(Linear2D);
		}

		// Ldg (load via texture cache) was introduced with Kepler architecture
		if ( (GetCompilerOptions().getTextureType() == Ldg) && (GetCompilerOptions().getTargetDevice() < KEPLER_35) )
		{
			llvm::errs() << "Warning: 'Ldg' texture memory only supported for Kepler and later on (CC >= 3.5)!  Using 'Linear1D' instead!\n";

			GetCompilerOptions().setTextureMemory(Linear1D);
		}
	}
}


// vim: set ts=2 sw=2 sts=2 et ai:

