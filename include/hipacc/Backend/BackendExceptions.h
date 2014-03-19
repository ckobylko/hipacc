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

//===--- BackendExceptions.h - Definitions of the exception types which can be thrown by the backend. ---===//
//
// This file implements the exception types which can be thrown by the backend.
//
//===----------------------------------------------------------------------------------------------------===//

#ifndef _BACKEND_EXCEPTIONS_H_
#define _BACKEND_EXCEPTIONS_H_

#include <stdexcept>
#include <string>

namespace clang
{
namespace hipacc
{
namespace Backend
{
	/** \brief	Root class of all exceptions which can be thrown by the backend. **/
	class BackendException : public std::runtime_error
	{
	private:

		typedef std::runtime_error	BaseType;

	public:

		inline BackendException(std::string strMessage) : BaseType(std::string("Backend exception: ") + strMessage)	{}

		virtual ~BackendException()	{}
	};

	/** \brief	Root class for all internal errors of the backend (errors due to invalid assumptions at design time). **/
	class InternalErrorException : public BackendException
	{
	private:

		typedef BackendException	BaseType;

	public:

		inline InternalErrorException(std::string strMessage) : BaseType(std::string("Internal error: ") + strMessage)	{}

		virtual ~InternalErrorException()	{}
	};

	/** \brief	Root class for all run-time errors of the backend (errors due to invalid inputs). **/
	class RuntimeErrorException : public BackendException
	{
	private:

		typedef BackendException	BaseType;

	public:

		inline RuntimeErrorException(std::string strMessage) : BaseType(std::string("Runtime error: ") + strMessage)	{}

		virtual ~RuntimeErrorException()	{}
	};


	class DuplicateSwitchEntryException final : public InternalErrorException
	{
	private:

		typedef InternalErrorException	BaseType;

	public:

		inline DuplicateSwitchEntryException(std::string strSwitch) : BaseType(std::string("The switch \"") + strSwitch + std::string("\" has already been defined!")) {}

		inline DuplicateSwitchEntryException(std::string strSwitch, std::string strGeneratorName) : BaseType(std::string("The switch \"") + strSwitch +
																											 std::string("\" has already been defined in code generator \"") +
																											 strGeneratorName + std::string("\"!"))
		{}
	};

	class UnhandledSwitchException final : public InternalErrorException
	{
	private:

		typedef InternalErrorException	BaseType;

	public:

		inline UnhandledSwitchException(std::string strSwitch) : BaseType(std::string("Handler for switch \"") + strSwitch + std::string("\" is missing!")) {}

		inline UnhandledSwitchException(std::string strSwitch, std::string strGeneratorName) : BaseType( std::string("Handler for switch \"") + strSwitch +
																										 std::string("\" is missing in code generator \"") + 
																										 strGeneratorName + std::string("\"!") )
		{}
	};

	class AbortException final : public RuntimeErrorException
	{
	private:

		typedef RuntimeErrorException	BaseType;

		int _iExitCode;

	public:

		inline AbortException(int iExitCode) : BaseType("Abort!"), _iExitCode(iExitCode)	{}

		inline int GetExitCode()	{ return _iExitCode; }
	};


	class UnknownSwitchException final : public RuntimeErrorException
	{
	private:

		typedef RuntimeErrorException	BaseType;

	public:

		inline UnknownSwitchException(std::string strSwitch, std::string strGeneratorName) : BaseType( std::string("The switch \"") + strSwitch +
																									   std::string("\" is not supported in code generator \"") +
																									   strGeneratorName + std::string("\"!") )
		{}
	};

	class InvalidOptionException final : public RuntimeErrorException
	{
	private:

		typedef RuntimeErrorException	BaseType;

	public:

		inline InvalidOptionException(std::string strSwitch, std::string strOption) : BaseType( std::string("The option \"") + strOption +
																								std::string("\" is invalid for the switch \"") +
																								strSwitch + std::string("\"!") )
		{}
	};

	class MissingOptionException final : public RuntimeErrorException
	{
	private:

		typedef RuntimeErrorException	BaseType;

	public:

		inline MissingOptionException(std::string strSwitch) : BaseType(std::string("The required option for switch \"") + strSwitch + std::string("\" is missing!"))
		{}

		inline MissingOptionException(std::string strSwitch, std::string strGeneratorName) : BaseType( std::string("The required option for switch \"") + strSwitch +
																									   std::string("\" is missing for code generator \"") +
																									   strGeneratorName + std::string("\"!") )
		{}
	};
} // end namespace Backend
} // end namespace hipacc
} // end namespace clang


#endif  // _BACKEND_EXCEPTIONS_H_

// vim: set ts=2 sw=2 sts=2 et ai:

