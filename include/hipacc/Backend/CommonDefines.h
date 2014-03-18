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

//===--- CommonDefines.h - Common definitions for the Backend library. ---------------===//
//
// This file contains common definitions and classes for the Backend library.
//
//===---------------------------------------------------------------------------------===//

#ifndef _BACKEND_COMMON_DEFINES_H_
#define _BACKEND_COMMON_DEFINES_H_

#include <string>
#include <utility>
#include <vector>
#include <type_traits>

namespace clang
{
namespace hipacc
{
namespace Backend
{
	class CommonDefines final
	{
	public:

		typedef std::vector< std::string >				ArgumentVectorType;

		typedef std::pair< std::string, std::string >	SwitchDisplayInfoType;
		typedef std::vector< SwitchDisplayInfoType >	SwitchDisplayInfoVectorType;


		template < typename SwitchTypeEnum >
		class CompilerSwitchInfoT final
		{
		private:

			static_assert( std::is_enum<SwitchTypeEnum>::value, "The \"switch type\"-type must be an enum or strongly-typed enum!" );

			SwitchTypeEnum		_eSwitchType;
			std::string			_strDescription;
			std::string			_strAdditionalOptions;

		public:

			inline CompilerSwitchInfoT()		{}

			inline CompilerSwitchInfoT(SwitchTypeEnum eType, std::string strDescription, std::string strAdditionalOptions = "")
			{
				_eSwitchType			= eType;
				_strDescription			= strDescription;
				_strAdditionalOptions	= strAdditionalOptions;
			}

			inline CompilerSwitchInfoT(CompilerSwitchInfoT &crRVal)
			{
				*this = crRVal;
			}

			inline CompilerSwitchInfoT& operator=(CompilerSwitchInfoT &crRVal)
			{
				_eSwitchType			= crRVal._eSwitchType;
				_strDescription			= crRVal._strDescription;
				_strAdditionalOptions	= crRVal._strAdditionalOptions;

				return *this;
			}


			inline SwitchDisplayInfoType CreateDisplayInfo(std::string strSwitch) const
			{
				SwitchDisplayInfoType SwitchInfo(strSwitch, GetDescription());

				if (!_strAdditionalOptions.empty())
				{
					SwitchInfo.first += std::string(" ") + _strAdditionalOptions;
				}

				return SwitchInfo;
			}


			inline std::string	GetAdditionalOptions() const						{ return _strAdditionalOptions; }
			inline void			SetAdditionalOptions(std::string strNewOptions)		{ _strAdditionalOptions = strNewOptions; }

			inline std::string	GetDescription() const								{ return _strDescription; }
			inline void			SetDescription(std::string strNewDescription)		{ _strDescription = strNewDescription; }

			inline SwitchTypeEnum	GetSwitchType()	 const							{ return _eSwitchType; }
			inline void				SetSwitchType(SwitchTypeEnum eNewSwitchType)	{ _eSwitchType = eNewSwitchType; }
		};

	};
} // end namespace Backend
} // end namespace hipacc
} // end namespace clang


#endif  // _BACKEND_COMMON_DEFINES_H_

// vim: set ts=2 sw=2 sts=2 et ai:

