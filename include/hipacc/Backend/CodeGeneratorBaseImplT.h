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

//===--- CodeGeneratorBaseImplT.h - Base class for all code generators. --------------===//
//
// This file implements the base generic base class for all code generators.
//
//===---------------------------------------------------------------------------------===//

#ifndef _BACKEND_CODE_GENERATOR_BASE_IMPL_T_H_
#define _BACKEND_CODE_GENERATOR_BASE_IMPL_T_H_

#include "hipacc/Config/CompilerOptions.h"
#include "BackendExceptions.h"
#include "CommonDefines.h"
#include "ICodeGenerator.h"
#include <map>
#include <string>
#include <utility>


namespace clang
{
namespace hipacc
{
namespace Backend
{
	template < typename SwitchTypeEnum >
	class CodeGeneratorBaseImplT : public ICodeGenerator
	{
	protected:

		typedef CommonDefines::CompilerSwitchInfoT< SwitchTypeEnum >	CompilerSwitchInfoType;

		class CodeGeneratorDescriptorBase
		{
		private:

			::clang::hipacc::TargetCode	_eTargetCode;
			std::string					_strName;
			std::string					_strEmissionKey;
			std::string					_strDescription;

		protected:

			inline CodeGeneratorDescriptorBase()	{}

			inline void SetDescription( std::string strNewDescription )					{ _strDescription	= strNewDescription; }
			inline void SetEmissionKey( std::string strNewEmissionKey )					{ _strEmissionKey	= strNewEmissionKey; }
			inline void SetName( std::string strNewName )								{ _strName			= strNewName; }
			inline void SetTargetCode( ::clang::hipacc::TargetCode eNewTargetCode )		{ _eTargetCode		= eNewTargetCode; }

		public:

			inline CodeGeneratorDescriptorBase(const CodeGeneratorDescriptorBase &crRVal)	{ *this = crRVal; }

			inline CodeGeneratorDescriptorBase& operator=(const CodeGeneratorDescriptorBase &crRVal)
			{
				_eTargetCode	= crRVal._eTargetCode;
				_strName		= crRVal._strName;
				_strEmissionKey	= crRVal._strEmissionKey;
				_strDescription	= crRVal._strDescription;

				return *this;
			}

			virtual ~CodeGeneratorDescriptorBase()	{}

			inline std::string					Description() const		{ return _strDescription; }
			inline std::string					EmissionKey() const		{ return _strEmissionKey; }
			inline std::string					Name() const			{ return _strName; }
			inline ::clang::hipacc::TargetCode	TargetCode() const		{ return _eTargetCode; }
		};


	private:

		typedef std::map< std::string, CompilerSwitchInfoType >			CompilerSwitchMapType;

		::clang::hipacc::CompilerOptions	*_pCompilerOptions;

		CompilerSwitchMapType	_mapKnownSwitches;

		const CodeGeneratorDescriptorBase	_Descriptor;


	protected:

		inline ::clang::hipacc::CompilerOptions& GetCompilerOptions()		{ return *_pCompilerOptions; }


		template <class SwitchClass>
		void _InitSwitch(SwitchTypeEnum eSwitch)
		{
			std::string strSwitch = SwitchClass::Key();

			if (_mapKnownSwitches.find(strSwitch) != _mapKnownSwitches.end())
			{
				throw DuplicateSwitchEntryException(strSwitch, GetName());
			}
			else
			{
				CompilerSwitchInfoType SwitchInfo;

				SwitchInfo.SetAdditionalOptions(SwitchClass::AdditionalOptions());
				SwitchInfo.SetDescription(SwitchClass::Description());
				SwitchInfo.SetSwitchType(eSwitch);

				_mapKnownSwitches[strSwitch] = SwitchInfo;
			}
		}


		template <class SwitchClass>
		typename SwitchClass::OptionParser::ReturnType _ParseOption(CommonDefines::ArgumentVectorType &rvecArguments, size_t szSwitchIndex, size_t szOptionOffset = static_cast<size_t>(1))
		{
			// Fetch option
			if (rvecArguments.size() <= szSwitchIndex + szOptionOffset)
			{
				throw MissingOptionException(rvecArguments[szSwitchIndex], GetName());
			}

			std::string strOption = rvecArguments[szSwitchIndex + szOptionOffset];

			// Parse option
			try
			{
				return typename SwitchClass::OptionParser::Parse(strOption);
			}
			catch (InvalidOptionException &)
			{
				throw;
			}
			catch (BackendException &e)
			{
				llvm::errs() << "ERROR: " << e.what() << "\n\n";
				throw InvalidOptionException(rvecArguments[szSwitchIndex], strOption);
			}
		}


		virtual size_t	_HandleSwitch(SwitchTypeEnum eSwitch, CommonDefines::ArgumentVectorType &rvecArguments, size_t szCurrentIndex) = 0;

		virtual void _CheckConfiguration()
		{
			// kernels are timed internally by the runtime in case of exploration
			if (GetCompilerOptions().timeKernels(USER_ON) && GetCompilerOptions().exploreConfig(USER_ON))
			{
				GetCompilerOptions().setTimeKernels(OFF);
			}
		}


	public:

		CodeGeneratorBaseImplT(::clang::hipacc::CompilerOptions *pCompilerOptions, const CodeGeneratorDescriptorBase &crDescriptor) :	_pCompilerOptions(pCompilerOptions),
																																		_Descriptor(crDescriptor)
		{
			if (_pCompilerOptions == nullptr)
			{
				throw BackendException("Compiler options have not been set");
			}
		}

		virtual ~CodeGeneratorBaseImplT()
		{
			_mapKnownSwitches.clear();
		}


		/** \name ICodeGenerator members */
		//@{
		virtual std::string GetDescription() const final override	{ return _Descriptor.Description(); }
		virtual std::string GetEmissionKey() const final override	{ return _Descriptor.EmissionKey(); }
		virtual std::string	GetName() const final override			{ return _Descriptor.Name(); }


		virtual CommonDefines::SwitchDisplayInfoVectorType GetCompilerSwitches() const final override
		{
			CommonDefines::SwitchDisplayInfoVectorType vecKnownSwitches;
			vecKnownSwitches.reserve( _mapKnownSwitches.size() );

			for (auto itSwitch = _mapKnownSwitches.begin(); itSwitch != _mapKnownSwitches.end(); itSwitch++)
			{
				vecKnownSwitches.push_back(itSwitch->second.CreateDisplayInfo(itSwitch->first));
			}

			return vecKnownSwitches;
		}


		virtual void Configure(CommonDefines::ArgumentVectorType & rvecArguments) final override
		{
			// Set the target code for this code generator
			GetCompilerOptions().setTargetCode(_Descriptor.TargetCode());

			// Parse all command line switches and options by the derived code generator
			for (size_t i = static_cast<size_t>(0); i < rvecArguments.size(); ++i)
			{
				std::string strSwitch = rvecArguments[i];

				auto itSwitchEntry = _mapKnownSwitches.find(strSwitch);

				if (itSwitchEntry == _mapKnownSwitches.end())
				{
					throw UnknownSwitchException(strSwitch, GetName());
				}

				i = _HandleSwitch(itSwitchEntry->second.GetSwitchType(), rvecArguments, i);
			}


			// Finally, check the configuration
			_CheckConfiguration();
		}
		//@}
	};
} // end namespace Backend
} // end namespace hipacc
} // end namespace clang


#endif  // _BACKEND_CODE_GENERATOR_BASE_IMPL_T_H_

// vim: set ts=2 sw=2 sts=2 et ai:

