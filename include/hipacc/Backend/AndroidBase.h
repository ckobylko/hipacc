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

//===--- AndroidBase.h - Base for all Android-based backends. ------------------------===//
//
// This file contains the base class for all Android-based backends.
//
//===---------------------------------------------------------------------------------===//

#ifndef _BACKEND_ANDROID_BASE_H_
#define _BACKEND_ANDROID_BASE_H_

#include "BackendExceptions.h"
#include <string>

namespace clang
{
namespace hipacc
{
namespace Backend
{
	class AndroidBase
	{
	protected:

		class AndroidSwitches final
		{
		public:

			static std::string RsPackageSwitch()					{ return "-rs-package"; }
			static std::string RsPackageSwitchAdditionalOptions()	{ return "<string>"; }
			static std::string RsPackageSwitchDescription()			{ return " Specify Renderscript package name. (default: \"org.hipacc.rs\")"; }
		};


		inline static std::string _CheckRsPackageOption(std::string strRsPackageOption)
		{
			std::string strPackageEnding(".rs");

			if (strRsPackageOption.length() < strPackageEnding.length())
			{
				throw InvalidOptionException(AndroidSwitches::RsPackageSwitch(), strRsPackageOption);
			}
			
			std::string strOptionEnding = strRsPackageOption.substr(strRsPackageOption.length() - strPackageEnding.length());

			if (strOptionEnding != strPackageEnding)
			{
				throw InvalidOptionException(AndroidSwitches::RsPackageSwitch(), strRsPackageOption);
			}

			return strRsPackageOption;
		}
	};
} // end namespace Backend
} // end namespace hipacc
} // end namespace clang


#endif  // _BACKEND_ANDROID_BASE_H_

// vim: set ts=2 sw=2 sts=2 et ai:

