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
#include <utility>
#include <vector>
#include <type_traits>
#include "hipacc/Config/CompilerOptions.h"
#include "BackendExceptions.h"
#include "CommonDefines.h"
#include "ICodeGenerator.h"

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
			Emit, Help, Version
		};


		typedef CommonDefines::CompilerSwitchInfoT< CompilerSwitchTypeEnum >	CompilerSwitchInfoType;


		typedef std::map< std::string, CompilerSwitchInfoType >		CompilerSwitchMapType;
		typedef std::map< std::string, std::string >				SwitchAliasMapType;
		typedef std::map< std::string, ICodeGeneratorPtr >			CodeGeneratorsMapType;


	private:

		class KnownSwitches final
		{
		public:

			typedef std::vector< std::string >		AliasesVectorType;

		public:

			inline static std::string EmissionSwitchBase()			{ return "-emit-"; }


			struct Help final
			{
				inline static std::string Key()					{ return "--help"; }
				inline static std::string AdditionalOptions()	{ return ""; }
				inline static std::string Description()			{ return "Display available options"; }

				inline static AliasesVectorType GetAliases()
				{
					AliasesVectorType vecDuplicates;

					vecDuplicates.push_back("-help");

					return vecDuplicates;
				}
			};

			struct Version final
			{
				inline static std::string Key()					{ return "--version"; }
				inline static std::string AdditionalOptions()	{ return ""; }
				inline static std::string Description()			{ return "Display version information"; }

				inline static AliasesVectorType GetAliases()
				{
					AliasesVectorType vecDuplicates;

					vecDuplicates.push_back("-version");

					return vecDuplicates;
				}
			};
		};


	private:

		::clang::hipacc::CompilerOptions	*_pCompilerOptions;

		CompilerSwitchMapType	_mapKnownSwitches;
		SwitchAliasMapType		_mapSwitchAliases;
		CodeGeneratorsMapType	_mapCodeGenerators;

		ICodeGeneratorPtr		_spSelectedCodeGenerator;


		template <class GeneratorType>
		void _InitCodeGenerator()
		{
			static_assert(std::is_base_of< ICodeGenerator, GeneratorType >::value, "Code generators must be derived from \"ICodeGenerator\"");

			ICodeGeneratorPtr spCodeGenerator( new GeneratorType(_pCompilerOptions) );

			std::string strEmissionKey = KnownSwitches::EmissionSwitchBase() + spCodeGenerator->GetEmissionKey();

			if (_mapCodeGenerators.find(strEmissionKey) != _mapCodeGenerators.end())
			{
				throw DuplicateSwitchEntryException(strEmissionKey);
			}
			else
			{
				_mapCodeGenerators[strEmissionKey]	= spCodeGenerator;
				_mapKnownSwitches[strEmissionKey]	= CompilerSwitchInfoType(CompilerSwitchTypeEnum::Emit, spCodeGenerator->GetDescription());
			}
		}


		template <class SwitchClass>
		void _InitSwitch(CompilerSwitchTypeEnum eSwitch)
		{
			// Extract switch key
			std::string strSwitch = SwitchClass::Key();

			// Check for duplicate switch entry
			if (_mapKnownSwitches.find(strSwitch) != _mapKnownSwitches.end())
			{
				throw DuplicateSwitchEntryException(strSwitch);
			}
			else
			{
				// Enter switch into the "known switches" map
				CompilerSwitchInfoType SwitchInfo;

				SwitchInfo.SetAdditionalOptions(SwitchClass::AdditionalOptions());
				SwitchInfo.SetDescription(SwitchClass::Description());
				SwitchInfo.SetSwitchType(eSwitch);

				_mapKnownSwitches[strSwitch] = SwitchInfo;


				// Set all switches Aliases
				KnownSwitches::AliasesVectorType vecAliases = SwitchClass::GetAliases();

				for each (std::string strAlias in vecAliases)
				{
					_mapSwitchAliases[strAlias] = strSwitch;
				}
			}
		}


		std::string _TranslateSwitchAlias( std::string strSwitch );

		size_t _HandleSwitch(std::string strSwitch, CommonDefines::ArgumentVectorType & rvecArguments, size_t szCurIndex);

		std::string _GetPadString(size_t szPadSize);
		void _PrintUsage();
		void _PrintSwitches(CommonDefines::SwitchDisplayInfoVectorType & rvecSwitches);

	public:

		BackendConfigurationManager(::clang::hipacc::CompilerOptions *pCompilerOptions);

		BackendConfigurationManager(const BackendConfigurationManager&) = delete;
		BackendConfigurationManager& operator=(const BackendConfigurationManager&) = delete;


		void Configure(CommonDefines::ArgumentVectorType & rvecArguments);
	};

} // end namespace Backend
} // end namespace hipacc
} // end namespace clang


#endif  // _BACKEND_CONFIGURATION_MANAGER_H_

// vim: set ts=2 sw=2 sts=2 et ai:

