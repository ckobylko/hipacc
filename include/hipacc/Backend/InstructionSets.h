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

//===--- InstructionSets.h - Definition of known vector instruction sets. ------------===//
//
// This file contains definitions of known vector instruction sets.
//
//===---------------------------------------------------------------------------------===//

#ifndef _BACKEND_INSTRUCTION_SETS_H_
#define _BACKEND_INSTRUCTION_SETS_H_

#include "ClangASTHelper.h"
#include "VectorizationAST.h"
#include <map>
#include <memory>
#include <string>
#include <utility>


namespace clang
{
namespace hipacc
{
namespace Backend
{
namespace Vectorization
{
  typedef AST::BaseClasses::TypeInfo::KnownTypes    VectorElementTypes;


  class InstructionSetBase
  {
  private:

    typedef std::map< std::string, ClangASTHelper::FunctionDeclarationVectorType >   FunctionDeclMapType;

    ClangASTHelper        _ASTHelper;
    FunctionDeclMapType   _mapKnownFuncDecls;

  protected:

    ClangASTHelper::FunctionDeclarationVectorType _GetFunctionDecl(std::string strFunctionName);

  public:

    InstructionSetBase(::clang::ASTContext &rAstContext, std::string strFunctionNamePrefix = "");

    virtual ~InstructionSetBase()
    {
      _mapKnownFuncDecls.clear();
    }


  public:

    virtual size_t GetVectorWidthBytes() const = 0;

  };

  typedef std::shared_ptr< InstructionSetBase >   InstructionSetBasePtr;


  class InstructionSetSSE final : public InstructionSetBase
  {
  private:

    enum class IntrinsicsSSEEnum
    {
      AddFloat,
      AndFloat,
      AndNotFloat,
      BroadCastFloat,
      CompareEqualFloat,
      CompareGreaterEqualFloat,
      CompareGreaterThanFloat,
      CompareLessEqualFloat,
      CompareLessThanFloat,
      CompareNotEqualFloat,
      CompareNotGreaterEqualFloat,
      CompareNotGreaterThanFloat,
      CompareNotLessEqualFloat,
      CompareNotLessThanFloat,
      DivideFloat,
      LoadFloat,
      MaxFloat,
      MinFloat,
      MoveMaskFloat,
      MultiplyFloat,
      OrFloat,
      ReciprocalFloat,
      ReciprocalSqrtFloat,
      SetFloat,
      SetReverseFloat,
      SetZeroFloat,
      SqrtFloat,
      StoreFloat,
      SubtractFloat,
      XorFloat
    };


    typedef std::pair< std::string, ::clang::FunctionDecl* >      IntrinsicInfoPairType;
    typedef std::map< IntrinsicsSSEEnum, IntrinsicInfoPairType >  IntrinsicMapType;


  private:

    IntrinsicMapType    _mapIntrinsicsSSE;

    inline void _InitIntrinsic(IntrinsicsSSEEnum eIntrinType, std::string strIntrinName)
    {
      _mapIntrinsicsSSE[eIntrinType] = IntrinsicInfoPairType(_GetIntrinsicPrefix() + strIntrinName, nullptr);
    }

    void _InitIntrinsicsMap();


  protected:

    static inline std::string _GetIntrinsicPrefix() { return "_mm_"; }

  public:

    InstructionSetSSE(::clang::ASTContext &rAstContext);

    virtual ~InstructionSetSSE()
    {
      _mapIntrinsicsSSE.clear();
    }


  public:

    virtual size_t GetVectorWidthBytes() const final override   { return static_cast< size_t >( 16 ); }

  };


} // end namespace Vectorization
} // end namespace Backend
} // end namespace hipacc
} // end namespace clang


#endif  // _BACKEND_INSTRUCTION_SETS_H_

// vim: set ts=2 sw=2 sts=2 et ai:

