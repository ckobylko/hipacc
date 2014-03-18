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

//===--- BackendConfigurationManager.cpp - Configures the hipacc compiler backend ---===//
//
// This file implements the configuration of the hipacc compiler backend and code generator.
//
//===--------------------------------------------------------------------------------===//

#include "hipacc/Backend/BackendConfigurationManager.h"
#include "hipacc/Config/config.h"
#include "llvm/Support/raw_ostream.h"

using namespace clang::hipacc::Backend;
using namespace std;

CommonDefines::SwitchDisplayInfoType BackendConfigurationManager::KnownSwitches::GetSwitchInfo(BackendConfigurationManager::CompilerSwitchTypeEnum eType)
{
	switch (eType)
	{
	case CompilerSwitchTypeEnum::Help:		return CommonDefines::SwitchDisplayInfoType(HelpSwitch(),	 HelpSwitchDescription());
	case CompilerSwitchTypeEnum::Version:	return CommonDefines::SwitchDisplayInfoType(VersionSwitch(), VersionSwitchDescription());
	default:	throw BackendException("Unknown switch type");
	}
}


BackendConfigurationManager::BackendConfigurationManager() : _spSelectedCodeGenerator(nullptr)
{
	// Init all known common switches
	_InitSwitch(CompilerSwitchTypeEnum::Help);
	_InitSwitch(CompilerSwitchTypeEnum::Version);


	// Set links for duplicate switches
	_mapDuplicateSwitches["-help"]		= KnownSwitches::HelpSwitch();
	_mapDuplicateSwitches["-version"]	= KnownSwitches::VersionSwitch();
}
	

void BackendConfigurationManager::_InitSwitch(CompilerSwitchTypeEnum eType)
{
	CommonDefines::SwitchDisplayInfoType Info = KnownSwitches::GetSwitchInfo(eType);

	if (_mapKnownSwitches.find(Info.first) != _mapKnownSwitches.end())
	{
		throw DuplicateSwitchEntryException(Info.first);
	}

	_mapKnownSwitches[Info.first] = CompilerSwitchInfo(eType, Info.second);
}


string BackendConfigurationManager::_TranslateDuplicateSwitch(string strSwitch)
{
	auto itTranslatedSwitch = _mapDuplicateSwitches.find(strSwitch);

	if (itTranslatedSwitch != _mapDuplicateSwitches.end())
	{
		return itTranslatedSwitch->second;
	}
	else
	{
		return strSwitch;
	}
}


string BackendConfigurationManager::_GetPadString(size_t szPadSize)
{
	string strPadString("");
	strPadString.resize(szPadSize, ' ');
	return strPadString;
}

void BackendConfigurationManager::_PrintUsage()
{
	// Print head-lines
	llvm::errs() << "OVERVIEW: HIPAcc - Heterogeneous Image Processing Acceleration framework\n\n";
	llvm::errs() << "USAGE:  hipacc [options] <input>\n\n";
	llvm::errs() << "OPTIONS:\n\n";


	// Format and print known common switches
	CommonDefines::SwitchDisplayInfoVectorType vecSwitches;

	for (auto itSwitch = _mapKnownSwitches.begin(); itSwitch != _mapKnownSwitches.end(); itSwitch++)
	{
		CommonDefines::SwitchDisplayInfoType	CurrentSwitch;

		CurrentSwitch.first		= itSwitch->first;
		CurrentSwitch.second	= itSwitch->second.GetDescription();

		vecSwitches.push_back(CurrentSwitch);
	}

	_PrintSwitches(vecSwitches);

}

void BackendConfigurationManager::_PrintSwitches(CommonDefines::SwitchDisplayInfoVectorType & rvecSwitches)
{
	const size_t cszPrintWidth			= 50;
	const size_t cszMinDescriptionWidth = 20;
	const size_t cszPadLeft				=  2;
	const size_t cszDescriptionDistance =  2;


	// Fetch maximum width of switch string
	size_t szMaxSwitchWidth = static_cast<size_t>(0);
	for each (auto itCurrentSwitch in rvecSwitches)
	{
		size_t szCurrentSize = itCurrentSwitch.first.length();

		if (szCurrentSize > szMaxSwitchWidth)
		{
			szMaxSwitchWidth = szCurrentSize;
		}
	}

	// Compute padded switch width and description width
	szMaxSwitchWidth += cszPadLeft + cszDescriptionDistance;
	size_t szDescriptionWidth = cszMinDescriptionWidth;
	if (cszMinDescriptionWidth + szMaxSwitchWidth < cszPrintWidth)
	{
		szDescriptionWidth = cszPrintWidth - szMaxSwitchWidth;
	}


	// Re-format every switch entry and print it
	for each (auto itCurrentSwitch in rvecSwitches)
	{
		// Pad the switch key
		string strPrintString = _GetPadString(cszPadLeft) + itCurrentSwitch.first;
		strPrintString += _GetPadString(szMaxSwitchWidth - strPrintString.length());

		// Break the description into pieces
		vector<string> vecDescriptionSubStrings;
		{
			string strDescription = itCurrentSwitch.second;

			// Find all new-line characters
			vector<int> vecNewLinePositions;
			vecNewLinePositions.push_back(-1);
			while (true)
			{
				string::size_type szNextNewLinePos = strDescription.find_first_of( '\n', static_cast<string::size_type>(vecNewLinePositions.back() + 1) );

				if (szNextNewLinePos != string::npos)
				{
					vecNewLinePositions.push_back(static_cast<int>(szNextNewLinePos));
				}
				else
				{
					// Push the position behind the last character to vector
					vecNewLinePositions.push_back(static_cast<int>(strDescription.length()));
					break;
				}
			}

			// Break the description into sections
			for (size_t i = static_cast<size_t>( 0 ); i < vecNewLinePositions.size() - 1; ++i)
			{
				// Fetch the current section between two new-line characters
				string::size_type szSectionOffset	= static_cast<string::size_type>( vecNewLinePositions[i] + 1 );
				string::size_type szSectionLength	= static_cast<string::size_type>( vecNewLinePositions[i + 1] ) - szSectionOffset;

				string strCurrentSection = strDescription.substr( static_cast<string::size_type>( vecNewLinePositions[i] + 1 ) );

				// Break the current section into pieces of the maximum display width
				for (size_t szPieceOffset = static_cast<size_t>(0); szPieceOffset < strCurrentSection.length(); szPieceOffset += szDescriptionWidth)
				{
					size_t szPieceLength = szDescriptionWidth;
					if (szPieceOffset + szPieceLength >= strCurrentSection.length())
					{
						szPieceLength = strCurrentSection.length() - szPieceOffset;
					}

					vecDescriptionSubStrings.push_back(strCurrentSection.substr(szPieceOffset, szPieceLength));
				}
			}
		}

		// Build the final print-string and print it
		strPrintString += vecDescriptionSubStrings[0] + string("\n");
		for (size_t i = static_cast<size_t>(1); i < vecDescriptionSubStrings.size(); ++i)
		{
			strPrintString += _GetPadString(szMaxSwitchWidth) + vecDescriptionSubStrings[i] + string("\n");
		}

		llvm::errs() << strPrintString;
	}
}


size_t BackendConfigurationManager::_HandleSwitch(std::string strSwitch, CommonDefines::ArgumentVectorType & rvecArguments, size_t szCurIndex)
{
	CompilerSwitchTypeEnum eSwitchType = _mapKnownSwitches[strSwitch].GetSwitchType();

	size_t szLastParsedSwitch = szCurIndex;

	switch (eSwitchType)
	{
	case CompilerSwitchTypeEnum::Emit:
	{
		if (_spSelectedCodeGenerator)
		{
			throw RuntimeErrorException("Only one code generator can be selected for the compiler invocation!");
		}
		else
		{
			_spSelectedCodeGenerator = _mapCodeGenerators[strSwitch];
		}

		break;
	}
	case CompilerSwitchTypeEnum::Help:
	{
		_PrintUsage();

		throw AbortException(EXIT_SUCCESS);
	}
	case CompilerSwitchTypeEnum::Version:
	{
		llvm::errs() << "hipacc version " << HIPACC_VERSION << " (" << GIT_REPOSITORY " " << GIT_VERSION << ")\n";

		throw AbortException(EXIT_SUCCESS);
	}
	default: throw UnhandledSwitchException(strSwitch);
	}

	return szLastParsedSwitch;
}


void BackendConfigurationManager::Configure(CommonDefines::ArgumentVectorType & rvecArguments)
{
	try
	{
		CommonDefines::ArgumentVectorType vecUnknownArguments;

		for (size_t i = static_cast<size_t>(0); i < rvecArguments.size(); ++i)
		{
			string strArgument = _TranslateDuplicateSwitch(rvecArguments[i]);

			auto itSwitch = _mapKnownSwitches.find(strArgument);

			if (itSwitch != _mapKnownSwitches.end())
			{
				i = _HandleSwitch(strArgument, rvecArguments, i);
			}
			else
			{
				vecUnknownArguments.push_back(rvecArguments[i]);
			}
		}


		// Configure the selected code generator
		if (_spSelectedCodeGenerator)
		{
			_spSelectedCodeGenerator->Configure(vecUnknownArguments);
		}
		else
		{
			throw RuntimeErrorException("No code generator has been selected! Did you forget the \"-emit-<X>\" switch?");
		}
	}
	catch (AbortException &e)
	{
		exit(e.GetExitCode());
	}
}


// vim: set ts=2 sw=2 sts=2 et ai:

