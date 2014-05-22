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

//===--- InstructionSets.cpp - Definition of known vector instruction sets. ----------===//
//
// This file contains definitions of known vector instruction sets.
//
//===---------------------------------------------------------------------------------===//

#include "hipacc/Backend/BackendExceptions.h"
#include "hipacc/Backend/InstructionSets.h"
#include "llvm/Support/raw_ostream.h"


#define VERBOSE_INIT_MODE 1

using namespace clang::hipacc::Backend::Vectorization;
using namespace clang::hipacc::Backend;
using namespace clang;
using namespace std;


ClangASTHelper::FunctionDeclarationVectorType InstructionSetBase::_GetFunctionDecl(string strFunctionName)
{
  auto itFunctionDecl = _mapKnownFuncDecls.find(strFunctionName);

  if (itFunctionDecl != _mapKnownFuncDecls.end())
  {
    return itFunctionDecl->second;
  }
  else
  {
    throw InternalErrorException(string("Cannot find function \"") + strFunctionName + string("\" !"));
  }
}

InstructionSetBase::InstructionSetBase(ASTContext &rAstContext, string strFunctionNamePrefix) : _ASTHelper(rAstContext)
{
  const size_t cszPrefixLength = strFunctionNamePrefix.size();
  ClangASTHelper::FunctionDeclarationVectorType vecFunctionDecls = _ASTHelper.GetKnownFunctionDeclarations();

  for each (auto itFuncDecl in vecFunctionDecls)
  {
    string strFuncName = ClangASTHelper::GetFullyQualifiedFunctionName(itFuncDecl);

    bool bAddFuncDecl = true;
    if (! strFunctionNamePrefix.empty())
    {
      if (strFuncName.size() < cszPrefixLength)
      {
        bAddFuncDecl = false;
      }
      else if (strFuncName.substr(0, cszPrefixLength) != strFunctionNamePrefix)
      {
        bAddFuncDecl = false;
      }
    }

    if (bAddFuncDecl)
    {
      _mapKnownFuncDecls[strFuncName].push_back(itFuncDecl);
    }
  }
}


InstructionSetSSE::InstructionSetSSE(ASTContext &rAstContext) : InstructionSetBase(rAstContext, _GetIntrinsicPrefix())
{
  _InitIntrinsicsMap();


#ifdef VERBOSE_INIT_MODE
  llvm::errs() << "\n\nIntrinsic functions for instruction set \"SSE\" (" << _mapIntrinsicsSSE.size() << " methods):\n";
#endif

  for each (auto itIntrinsic in _mapIntrinsicsSSE)
  {
    IntrinsicInfoPairType &rIntrinsicInfo = itIntrinsic.second;

    auto vecFunctions = _GetFunctionDecl(rIntrinsicInfo.first);

    if (vecFunctions.size() != static_cast<size_t>(1))
    {
      throw InternalErrorException(string("Found ambiguous entry for intrinsic function \"") + rIntrinsicInfo.first + string("\" !"));
    }

    rIntrinsicInfo.second = vecFunctions.front();

#ifdef VERBOSE_INIT_MODE
    llvm::errs() << "\n" << rIntrinsicInfo.second->getResultType().getAsString() << " " << rIntrinsicInfo.second->getNameAsString() << "(";
    for (unsigned int uiParam = 0; uiParam < rIntrinsicInfo.second->getNumParams(); ++uiParam)
    {
      ParmVarDecl *pParam = rIntrinsicInfo.second->getParamDecl(uiParam);
      llvm::errs() << pParam->getType().getAsString();
      if ((uiParam + 1) < rIntrinsicInfo.second->getNumParams())
      {
        llvm::errs() << ", ";
      }
    }
    llvm::errs() << ")";
#endif
  }

#ifdef VERBOSE_INIT_MODE
  llvm::errs() << "\n\n";
#endif
}

void InstructionSetSSE::_InitIntrinsicsMap()
{
  _InitIntrinsic( IntrinsicsSSEEnum::AddFloat,                    "add_ps"      );
  _InitIntrinsic( IntrinsicsSSEEnum::AndFloat,                    "and_ps"      );
  _InitIntrinsic( IntrinsicsSSEEnum::AndNotFloat,                 "andnot_ps"   );
  _InitIntrinsic( IntrinsicsSSEEnum::BroadCastFloat,              "set1_ps"     );
  _InitIntrinsic( IntrinsicsSSEEnum::CompareEqualFloat,           "cmpeq_ps"    );
  _InitIntrinsic( IntrinsicsSSEEnum::CompareGreaterEqualFloat,    "cmpge_ps"    );
  _InitIntrinsic( IntrinsicsSSEEnum::CompareGreaterThanFloat,     "cmpgt_ps"    );
  _InitIntrinsic( IntrinsicsSSEEnum::CompareLessEqualFloat,       "cmple_ps"    );
  _InitIntrinsic( IntrinsicsSSEEnum::CompareLessThanFloat,        "cmplt_ps"    );
  _InitIntrinsic( IntrinsicsSSEEnum::CompareNotEqualFloat,        "cmpneq_ps"   );
  _InitIntrinsic( IntrinsicsSSEEnum::CompareNotGreaterEqualFloat, "cmpnge_ps"   );
  _InitIntrinsic( IntrinsicsSSEEnum::CompareNotGreaterThanFloat,  "cmpngt_ps"   );
  _InitIntrinsic( IntrinsicsSSEEnum::CompareNotLessEqualFloat,    "cmpnle_ps"   );
  _InitIntrinsic( IntrinsicsSSEEnum::CompareNotLessThanFloat,     "cmpnlt_ps"   );
  _InitIntrinsic( IntrinsicsSSEEnum::DivideFloat,                 "div_ps"      );
  _InitIntrinsic( IntrinsicsSSEEnum::LoadFloat,                   "loadu_ps"    );
  _InitIntrinsic( IntrinsicsSSEEnum::MaxFloat,                    "max_ps"      );
  _InitIntrinsic( IntrinsicsSSEEnum::MinFloat,                    "min_ps"      );
  _InitIntrinsic( IntrinsicsSSEEnum::MoveMaskFloat,               "movemask_ps" );
  _InitIntrinsic( IntrinsicsSSEEnum::MultiplyFloat,               "mul_ps"      );
  _InitIntrinsic( IntrinsicsSSEEnum::OrFloat,                     "or_ps"       );
  _InitIntrinsic( IntrinsicsSSEEnum::ReciprocalFloat,             "rcp_ps"      );
  _InitIntrinsic( IntrinsicsSSEEnum::ReciprocalSqrtFloat,         "rsqrt_ps"    );
  _InitIntrinsic( IntrinsicsSSEEnum::SetFloat,                    "set_ps"      );
  _InitIntrinsic( IntrinsicsSSEEnum::SetReverseFloat,             "setr_ps"     );
  _InitIntrinsic( IntrinsicsSSEEnum::SetZeroFloat,                "setzero_ps"  );
  _InitIntrinsic( IntrinsicsSSEEnum::SqrtFloat,                   "sqrt_ps"     );
  _InitIntrinsic( IntrinsicsSSEEnum::StoreFloat,                  "storeu_ps"   );
  _InitIntrinsic( IntrinsicsSSEEnum::SubtractFloat,               "sub_ps"      );
  _InitIntrinsic( IntrinsicsSSEEnum::XorFloat,                    "xor_ps"      );
}




// vim: set ts=2 sw=2 sts=2 et ai:

