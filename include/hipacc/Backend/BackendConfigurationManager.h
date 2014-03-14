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

//===--- BackendConfigurationManager.h - Configures the hipacc compiler backend ---===//
//
// This file implements the configuration of the hipacc compiler backend and code generator.
//
//===------------------------------------------------------------------------------===//

#ifndef _BACKEND_CONFIGURATION_MANAGER_H_
#define _BACKEND_CONFIGURATION_MANAGER_H_

#include <map>
#include <ostream>
#include <string>
#include <vector>
#include "BackendExceptions.h"

namespace clang
{
namespace hipacc
{
namespace Backend
{
	class BackendConfigurationManager final
	{
	private:

		enum class CompilerSwitchTypeEnum
		{
			Help, Version
		};

		class CompilerSwitchInfo final
		{
		private:

			CompilerSwitchTypeEnum	_eSwitchType;
			std::string			_strDescription;

		public:

			inline CompilerSwitchInfo()		{}

			inline CompilerSwitchInfo(CompilerSwitchTypeEnum eType, std::string strDescription)
			{
				_eSwitchType	= eType;
				_strDescription	= strDescription;
			}

			inline CompilerSwitchInfo(CompilerSwitchInfo &crRVal)
			{
				*this = crRVal;
			}

			inline CompilerSwitchInfo& operator=(CompilerSwitchInfo &crRVal)
			{
				_eSwitchType	= crRVal._eSwitchType;
				_strDescription = crRVal._strDescription;

				return *this;
			}


			inline CompilerSwitchTypeEnum	GetSwitchType()	 const		{ return _eSwitchType; }
			inline std::string				GetDescription() const		{ return _strDescription; }
		};


		typedef std::map< std::string, CompilerSwitchInfo >		CompilerSwitchMapType;
		typedef std::map< std::string, std::string >			DuplicateSwitchMapType;


	private:

		class KnownSwitches final
		{
		public:

			typedef std::pair< std::string, std::string >	SwitchInfoPair;

			static SwitchInfoPair GetSwitchInfo(CompilerSwitchTypeEnum eType);

		public:

			inline static std::string HelpSwitch()				{ return "--help"; }
			inline static std::string HelpSwitchDescription()	{ return "Display available options"; }

			inline static std::string VersionSwitch()				{ return "--version"; }
			inline static std::string VersionSwitchDescription()	{ return "Display version information"; }
		};


	private:

		CompilerSwitchMapType	_mapKnownSwitches;
		DuplicateSwitchMapType	_mapDuplicateSwitches;

		void _InitSwitch(CompilerSwitchTypeEnum eType);

		std::string _TranslateDuplicateSwitch( std::string strSwitch );

		size_t _HandleSwitch(std::string strSwitch, std::vector< std::string > & rvecArguments, size_t szCurIndex);

		std::string _GetPadString(size_t szPadSize);
		void _PrintUsage();
		void _PrintSwitches(std::vector< std::pair< std::string, std::string > > & rvecSwitches);

	public:

		BackendConfigurationManager();

		BackendConfigurationManager(const BackendConfigurationManager&) = delete;
		BackendConfigurationManager& operator=(const BackendConfigurationManager&) = delete;


		void Configure(std::vector< std::string > & rvecArguments);

	};

} // end namespace Backend
} // end namespace hipacc
} // end namespace clang


#endif  // _BACKEND_CONFIGURATION_MANAGER_H_

// vim: set ts=2 sw=2 sts=2 et ai:

