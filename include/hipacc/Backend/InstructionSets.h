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

#define VERBOSE_INIT_MODE 1   // Uncomment this for a print-out of the inited intrinsic functions

#ifdef VERBOSE_INIT_MODE
#include "llvm/Support/raw_ostream.h"
#endif


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
  protected:

    typedef std::pair< std::string, ::clang::FunctionDecl* >      IntrinsicInfoPairType;

    template < typename IntrinsicIDType >   using IntrinsicMapTemplateType = std::map< IntrinsicIDType, IntrinsicInfoPairType >;

  private:

    typedef std::map< std::string, ClangASTHelper::FunctionDeclarationVectorType >   FunctionDeclMapType;

    ClangASTHelper        _ASTHelper;
    FunctionDeclMapType   _mapKnownFuncDecls;

    std::string           _strIntrinsicPrefix;

  protected:

    ClangASTHelper::FunctionDeclarationVectorType _GetFunctionDecl(std::string strFunctionName);

    template < typename IntrinsicIDType >
    inline void _InitIntrinsic(IntrinsicMapTemplateType< IntrinsicIDType > &rIntrinMap, IntrinsicIDType eIntrinType, std::string strIntrinName)
    {
      rIntrinMap[eIntrinType] = IntrinsicInfoPairType(_strIntrinsicPrefix + strIntrinName, nullptr);
    }

    template < typename IntrinsicIDType >
    inline void _LookupIntrinsics(IntrinsicMapTemplateType< IntrinsicIDType > &rIntrinMap, std::string strInstructionSetName)
    {
      #ifdef VERBOSE_INIT_MODE
      llvm::errs() << "\n\nIntrinsic functions for instruction set \"" << strInstructionSetName << "\" (" << rIntrinMap.size() << " methods):\n";
      #endif

      for each (auto itIntrinsic in rIntrinMap)
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


  class InstructionSetSSE : public InstructionSetBase
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
      ShuffleFloat,
      SqrtFloat,
      StoreFloat,
      SubtractFloat,
      XorFloat
    };


    typedef InstructionSetBase::IntrinsicMapTemplateType< IntrinsicsSSEEnum >   IntrinsicMapType;


  private:

    IntrinsicMapType    _mapIntrinsicsSSE;

    inline void _InitIntrinsic(IntrinsicsSSEEnum eIntrinType, std::string strIntrinName)
    {
      InstructionSetBase::_InitIntrinsic(_mapIntrinsicsSSE, eIntrinType, strIntrinName);
    }

    void _InitIntrinsicsMap();

    inline void _LookupIntrinsics()
    {
      InstructionSetBase::_LookupIntrinsics(_mapIntrinsicsSSE, "SSE");
    }


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


  class InstructionSetSSE2 : public InstructionSetSSE
  {
  private:

    enum class IntrinsicsSSE2Enum
    {
      AddDouble,                    AddInt8,                AddInt16,                AddInt32,                AddInt64,
      AndDouble,                    AndInteger,             AndNotDouble,            AndNotInteger,
      BroadCastDouble,              BroadCastInt8,          BroadCastInt16,          BroadCastInt32,          BroadCastInt64,
      CastDoubleToFloat,            CastDoubleToInteger,    CastFloatToDouble,       CastFloatToInteger,      CastIntegerToDouble, CastIntegerToFloat,
      CompareEqualDouble,           CompareEqualInt8,       CompareEqualInt16,       CompareEqualInt32,
      CompareGreaterEqualDouble,
      CompareGreaterThanDouble,     CompareGreaterThanInt8, CompareGreaterThanInt16, CompareGreaterThanInt32,
      CompareLessEqualDouble,
      CompareLessThanDouble,        CompareLessThanInt8,    CompareLessThanInt16,    CompareLessThanInt32,
      CompareNotEqualDouble,
      CompareNotGreaterEqualDouble,
      CompareNotGreaterThanDouble,
      CompareNotLessEqualDouble,
      CompareNotLessThanDouble,
      ConvertDoubleFloat,           ConvertDoubleInt32,     ConvertFloatDouble,      ConvertFloatInt32,       ConvertInt32Double,  ConvertInt32Float,
      DivideDouble,
      ExtractInt16,
      InsertInt16,
      LoadDouble,                   LoadInteger,
      MaxDouble,                    MaxUInt8,               MaxInt16,
      MinDouble,                    MinUInt8,               MinInt16,
      MoveMaskDouble,               MoveMaskInt8,
      MultiplyDouble,               MultiplyInt16,
      OrDouble,                     OrInteger,
      PackInt16ToInt8,              PackInt16ToUInt8,       PackInt32ToInt16,
      SetDouble,                    SetInt8,                SetInt16,                SetInt32,                SetInt64,
      SetReverseDouble,             SetReverseInt8,         SetReverseInt16,         SetReverseInt32,
      SetZeroDouble,                SetZeroInteger,
      ShiftLeftInt16,               ShiftLeftInt32,         ShiftLeftVectorBytes,
      ShiftRightInt16,              ShiftRightInt32,        ShiftRightVectorBytes,
      ShuffleInt16High,             ShuffleInt16Low,        ShuffleInt32,
      SqrtDouble,
      StoreDouble,                  StoreInteger,           StoreConditionalInteger,
      SubtractDouble,               SubtractInt8,           SubtractInt16,           SubtractInt32,           SubtractInt64,
      XorDouble,                    XorInteger
    };

    typedef InstructionSetBase::IntrinsicMapTemplateType< IntrinsicsSSE2Enum >  IntrinsicMapType;


  private:

    IntrinsicMapType    _mapIntrinsicsSSE2;

    inline void _InitIntrinsic(IntrinsicsSSE2Enum eIntrinType, std::string strIntrinName)
    {
      InstructionSetBase::_InitIntrinsic(_mapIntrinsicsSSE2, eIntrinType, strIntrinName);
    }

    void _InitIntrinsicsMap();

    inline void _LookupIntrinsics()
    {
      InstructionSetBase::_LookupIntrinsics(_mapIntrinsicsSSE2, "SSE2");
    }


  public:

    InstructionSetSSE2(::clang::ASTContext &rAstContext);

    virtual ~InstructionSetSSE2()
    {
      _mapIntrinsicsSSE2.clear();
    }

  };

} // end namespace Vectorization
} // end namespace Backend
} // end namespace hipacc
} // end namespace clang


#ifdef VERBOSE_INIT_MODE
#undef VERBOSE_INIT_MODE
#endif


#endif  // _BACKEND_INSTRUCTION_SETS_H_

// vim: set ts=2 sw=2 sts=2 et ai:

