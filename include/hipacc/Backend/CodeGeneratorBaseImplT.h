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
	private:

		typedef CommonDefines::CompilerSwitchInfoT< SwitchTypeEnum >	CompilerSwitchInfoType;
		typedef std::map< std::string, CompilerSwitchInfoType >			CompilerSwitchMapType;


		const std::string _strDescription;
		const std::string _strEmissionKey;
		const std::string _strName;

		CompilerSwitchMapType	_mapKnownSwitches;


	protected:

		typedef std::pair< std::string, CompilerSwitchInfoType >		CompilerSwitchEntryType;


		void _InitSwitch(SwitchTypeEnum eSwitch)
		{
			CompilerSwitchEntryType SwitchEntry = _GetSwitchEntry(eSwitch);

			std::string strSwitch = SwitchEntry.first;

			if (_mapKnownSwitches.find(strSwitch) != _mapKnownSwitches)
			{
				throw DuplicateSwitchEntryException(strSwitch, GetName());
			}
			else
			{
				_mapKnownSwitches[strSwitch] = SwitchEntry.second;
			}
		}


		virtual CompilerSwitchEntryType _GetSwitchEntry(SwitchTypeEnum eSwitch) const = 0;
		virtual size_t					_HandleSwitch(SwitchTypeEnum eSwitch, CommonDefines::ArgumentVectorType &rvecArguments, size_t szCurrentIndex) = 0;


	public:

		CodeGeneratorBaseImplT(std::string strName, std::string strEmissionKey, std::string strDescription) : _strDescription(strDescription),
																											  _strEmissionKey(strEmissionKey),
																											  _strName(strName)
		{
		}

		virtual ~CodeGeneratorBaseImplT()
		{
			_mapKnownSwitches.clear();
		}


		/** \name ICodeGenerator members */
		//@{
		virtual std::string GetDescription() const final override	{ return _strDescription; }
		virtual std::string GetEmissionKey() const final override	{ return _strEmissionKey; }
		virtual std::string	GetName() const final override			{ return _strName; }


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
		}
		//@}
	};
} // end namespace Backend
} // end namespace hipacc
} // end namespace clang


#endif  // _BACKEND_CODE_GENERATOR_BASE_IMPL_T_H_

// vim: set ts=2 sw=2 sts=2 et ai:

