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

//===--- ICodeGenerator.h - Base interface for all code generators. ------------------===//
//
// This file implements the base interface for all code generators.
//
//===---------------------------------------------------------------------------------===//

#ifndef _BACKEND_ICODEGENERATOR_H_
#define _BACKEND_ICODEGENERATOR_H_

#include <memory>
#include <string>
#include <utility>
#include <vector>

namespace clang
{
namespace hipacc
{
namespace Backend
{
	class ICodeGenerator
	{
	public:
		virtual ~ICodeGenerator()	{}


		virtual std::string GetDescription() const = 0;
		virtual std::string GetEmissionKey() const = 0;
		virtual std::string	GetName() const = 0;

		virtual std::pair< std::string, std::string > GetCompilerSwitches() const = 0;

		virtual void Configure(std::vector< std::string > & rvecArguments) = 0;
	};


	typedef std::shared_ptr< ICodeGenerator >	ICodeGeneratorPtr;

} // end namespace Backend
} // end namespace hipacc
} // end namespace clang


#endif  // _BACKEND_ICODEGENERATOR_H_

// vim: set ts=2 sw=2 sts=2 et ai:

