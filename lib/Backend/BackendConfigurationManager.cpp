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

// Supported code generators
#include "hipacc/Backend/CPU_x86.h"
#include "hipacc/Backend/CUDA.h"
#include "hipacc/Backend/Filterscript.h"
#include "hipacc/Backend/OpenCL_ACC.h"
#include "hipacc/Backend/OpenCL_CPU.h"
#include "hipacc/Backend/OpenCL_GPU.h"
#include "hipacc/Backend/Renderscript.h"

using namespace clang::hipacc::Backend;
using namespace clang::hipacc;
using namespace std;

BackendConfigurationManager::BackendConfigurationManager(CompilerOptions *pCompilerOptions) : _pCompilerOptions(pCompilerOptions), _spSelectedCodeGenerator(nullptr)
{
  _strInputFile = "";
  _strOutputFile = "";

  if (_pCompilerOptions == nullptr)
  {
    throw BackendException("Compiler options have not been set");
  }


  // Init all known common switches
  _InitSwitch< KnownSwitches::Help        >(CompilerSwitchTypeEnum::Help);
  _InitSwitch< KnownSwitches::OutputFile  >(CompilerSwitchTypeEnum::OutputFile);
  _InitSwitch< KnownSwitches::Version     >(CompilerSwitchTypeEnum::Version);


  // Init known backends
  _InitBackend<CPU_x86>();
  _InitBackend<CUDA>();
  _InitBackend<OpenCL_ACC>();
  _InitBackend<OpenCL_CPU>();
  _InitBackend<OpenCL_GPU>();
  _InitBackend<Renderscript>();
  _InitBackend<Filterscript>();
}


string BackendConfigurationManager::_TranslateSwitchAlias(string strSwitch)
{
  auto itTranslatedSwitch = _mapSwitchAliases.find(strSwitch);

  if (itTranslatedSwitch != _mapSwitchAliases.end())
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
    vecSwitches.push_back(itSwitch->second.CreateDisplayInfo(itSwitch->first));
  }

  _PrintSwitches(vecSwitches);


  // Print the specific switches for all known code generators
  for each (auto itCodeGenerator in _mapCodeGenerators)
  {
    ICodeGeneratorPtr spCodeGenerator = itCodeGenerator.second;

    CommonDefines::SwitchDisplayInfoVectorType vecCodeGeneratorSwitches = spCodeGenerator->GetCompilerSwitches();

    if (! vecCodeGeneratorSwitches.empty())
    {
      llvm::errs() << "\nSpecific options for code generator \"" << spCodeGenerator->GetName() << "\":\n\n";

      _PrintSwitches(vecCodeGeneratorSwitches);
    }
  }
}

void BackendConfigurationManager::_PrintSwitches(CommonDefines::SwitchDisplayInfoVectorType & rvecSwitches)
{
  const size_t cszPrintWidth          = 110;
  const size_t cszMinDescriptionWidth =  20;
  const size_t cszPadLeft             =   2;
  const size_t cszDescriptionDistance =   2;


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
    strPrintString       += _GetPadString(szMaxSwitchWidth - strPrintString.length());

    // Break the description into pieces
    vector<string> vecDescriptionSubStrings;
    {
      string strDescription = itCurrentSwitch.second;

      // Find all new-line characters
      vector<int> vecNewLinePositions;
      vecNewLinePositions.push_back(-1);
      while (true)
      {
        string::size_type szNextNewLinePos = strDescription.find_first_of('\n', static_cast<string::size_type>(vecNewLinePositions.back() + 1));

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
      for (size_t i = static_cast<size_t>(0); i < vecNewLinePositions.size() - 1; ++i)
      {
        // Fetch the current section between two new-line characters
        string::size_type szSectionOffset = static_cast<string::size_type>(vecNewLinePositions[i] + 1);
        string::size_type szSectionLength = static_cast<string::size_type>(vecNewLinePositions[i + 1]) - szSectionOffset;

        string strCurrentSection = strDescription.substr(szSectionOffset, szSectionLength);

        // Break the current section into pieces of the maximum display width
        for (size_t szPieceOffset = static_cast<size_t>(0); szPieceOffset < strCurrentSection.length();)
        {
          size_t szPieceLength = szDescriptionWidth;

          if (szPieceOffset + szPieceLength >= strCurrentSection.length())
          {
            // Whole rest of the description can be displayed in one line
            vecDescriptionSubStrings.push_back(strCurrentSection.substr(szPieceOffset));
            break;
          }
          else
          {
            // The rest of the description still needs to be broken into several lines
            string strCurrentPiece = strCurrentSection.substr(szPieceOffset, szPieceLength);

            string::size_type szLastWhiteSpace = strCurrentPiece.find_last_of(' ');

            if (szLastWhiteSpace == string::npos)
            {
              vecDescriptionSubStrings.push_back(strCurrentPiece);
              szPieceOffset += szPieceLength;
            }
            else
            {
              // Whitespace found => Break at whitespace
              vecDescriptionSubStrings.push_back(strCurrentPiece.substr(0, szLastWhiteSpace));
              szPieceOffset += szLastWhiteSpace + 1;
            }
          }
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

  size_t szReturnIndex = szCurIndex;

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
    }
    break;
  case CompilerSwitchTypeEnum::OutputFile:
    {
      if (_strOutputFile != "")
      {
        throw RuntimeErrorException("Only one output file can be specified for the compiler invocation");
      }
      else if (szCurIndex >= rvecArguments.size())
      {
        throw RuntimeErrors::MissingOptionException(strSwitch);
      }
      else
      {
        _strOutputFile = rvecArguments[szCurIndex + 1];
        ++szReturnIndex;
      }
    }
    break;
  case CompilerSwitchTypeEnum::Help:
    {
      _PrintUsage();
      throw RuntimeErrors::AbortException(EXIT_SUCCESS);
    }
  case CompilerSwitchTypeEnum::Version:
    {
      llvm::errs() << "hipacc version " << HIPACC_VERSION << " (" << GIT_REPOSITORY " " << GIT_VERSION << ")\n";
      throw RuntimeErrors::AbortException(EXIT_SUCCESS);
    }
  default:  throw InternalErrors::UnhandledSwitchException(strSwitch);
  }

  return szReturnIndex;
}


void BackendConfigurationManager::Configure(CommonDefines::ArgumentVectorType & rvecArguments)
{
  try
  {
    CommonDefines::ArgumentVectorType vecUnknownArguments;

    for (size_t i = static_cast<size_t>(0); i < rvecArguments.size(); ++i)
    {
      if ((i + 1) == rvecArguments.size())
      {
        // Last argument must be input file
        _strInputFile = rvecArguments[i];
      }
      else
      {
        string strArgument = _TranslateSwitchAlias(rvecArguments[i]);

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
    }

    if (_strInputFile == "")
    {
      throw RuntimeErrorException("No input file has been specified!");
    }
    else if (_strOutputFile == "")
    {
      throw RuntimeErrorException(string("No output file has been specified! Did you forget the \"") + KnownSwitches::OutputFile::Key() + string("\" switch?"));
    }

    // Configure the selected code generator
    if (_spSelectedCodeGenerator)
    {
      _spSelectedCodeGenerator->Configure(vecUnknownArguments);
    }
    else
    {
      throw RuntimeErrorException(string("No code generator has been selected! Did you forget the \"") + KnownSwitches::EmissionSwitchBase() + string("<X>\" switch?"));
    }
  }
  catch (RuntimeErrors::AbortException &e)
  {
    exit(e.GetExitCode());
  }
}


CommonDefines::ArgumentVectorType BackendConfigurationManager::GetClangArguments()
{
  CommonDefines::ArgumentVectorType vecClangArguments;

  // Add HIPAcc runtime include paths
  vecClangArguments.push_back(string("-I") + string(RUNTIME_INCLUDES));
  vecClangArguments.push_back(string("-I") + string(RUNTIME_INCLUDES) + string("/dsl"));


#ifdef USE_MINGW
  // Add MinGW system include paths
  vecClangArguments.push_back("-isystem");
  vecClangArguments.push_back(MINGW_INCLUDE_ROOT);

  vecClangArguments.push_back("-isystem");
  vecClangArguments.push_back(MINGW_INCLUDE_ROOT_CPP);

  vecClangArguments.push_back("-isystem");
  vecClangArguments.push_back(string(MINGW_INCLUDE_ROOT_CPP) + string("/c++"));

  vecClangArguments.push_back("-isystem");
  vecClangArguments.push_back(string(MINGW_INCLUDE_ROOT_CPP) + string("/c++/mingw32"));
#endif

  // Add exception support
  vecClangArguments.push_back("-fexceptions");

  // Set C++ 11 support
  vecClangArguments.push_back("-std=c++11");

  // Add output file
  vecClangArguments.push_back("-o");
  vecClangArguments.push_back(_strOutputFile);

  // Add input file
  vecClangArguments.push_back(_strInputFile);

  return vecClangArguments;
}


// vim: set ts=2 sw=2 sts=2 et ai:

