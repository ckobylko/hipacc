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

//===--- Renderscript.cpp - Implements the Renderscript code generator. --------------===//
//
// This file implements the Renderscript code generator.
//
//===---------------------------------------------------------------------------------===//

#include "hipacc/Backend/Renderscript.h"

using namespace clang::hipacc::Backend;
using namespace std;

Renderscript::CodeGenerator::CompilerSwitchEntryType Renderscript::CodeGenerator::_GetSwitchEntry(CompilerSwitchTypeEnum eSwitch) const
{
	CompilerSwitchEntryType SwitchEntry;

	SwitchEntry.second.SetSwitchType(eSwitch);

	switch (eSwitch)
	{
	case CompilerSwitchTypeEnum::EmitPadding:
		SwitchEntry.first = AcceleratorDeviceSwitches::EmitPaddingSwitch();
		SwitchEntry.second.SetAdditionalOptions(AcceleratorDeviceSwitches::EmitPaddingSwitchAdditionalOptions());
		SwitchEntry.second.SetDescription(AcceleratorDeviceSwitches::EmitPaddingSwitchDescription());
		break;
	case CompilerSwitchTypeEnum::PixelsPerThread:
		SwitchEntry.first = AcceleratorDeviceSwitches::PixelsPerThreadSwitch();
		SwitchEntry.second.SetAdditionalOptions(AcceleratorDeviceSwitches::PixelsPerThreadSwitchAdditionalOptions());
		SwitchEntry.second.SetDescription(AcceleratorDeviceSwitches::PixelsPerThreadSwitchDescription());
		break;
	case CompilerSwitchTypeEnum::RsPackage:
		SwitchEntry.first = AndroidSwitches::RsPackageSwitch();
		SwitchEntry.second.SetAdditionalOptions(AndroidSwitches::RsPackageSwitchAdditionalOptions());
		SwitchEntry.second.SetDescription(AndroidSwitches::RsPackageSwitchDescription());
		break;
	default:	throw BackendException("Unknown switch type");
	}

	return SwitchEntry;
}

size_t Renderscript::CodeGenerator::_HandleSwitch(CompilerSwitchTypeEnum eSwitch, CommonDefines::ArgumentVectorType &rvecArguments, size_t szCurrentIndex)
{
	// TODO: Implement

	return szCurrentIndex;
}


// vim: set ts=2 sw=2 sts=2 et ai:

