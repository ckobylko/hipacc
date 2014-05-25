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



using namespace clang::hipacc::Backend::Vectorization;
using namespace clang::hipacc::Backend;
using namespace clang;
using namespace std;


// Implementation of class InstructionSetBase
InstructionSetBase::InstructionSetBase(ASTContext &rAstContext, string strFunctionNamePrefix) : _ASTHelper(rAstContext), _strIntrinsicPrefix(strFunctionNamePrefix)
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

void InstructionSetBase::_CreateIntrinsicDeclaration(string strFunctionName, const QualType &crReturnType, const ClangASTHelper::QualTypeVectorType &crvecArgTypes, const ClangASTHelper::StringVectorType &crvecArgNames)
{
  if (_mapKnownFuncDecls.find(strFunctionName) == _mapKnownFuncDecls.end())
  {
    _mapKnownFuncDecls[ strFunctionName ].push_back( _ASTHelper.CreateFunctionDeclaration(strFunctionName, crReturnType, crvecArgNames, crvecArgTypes) );
  }
}

void InstructionSetBase::_CreateIntrinsicDeclaration(string strFunctionName, const QualType &crReturnType, const QualType &crArgType1, string strArgName1, const QualType &crArgType2, string strArgName2)
{
  ClangASTHelper::QualTypeVectorType  vecArgTypes;
  ClangASTHelper::StringVectorType    vecArgNames;

  vecArgTypes.push_back( crArgType1 );
  vecArgNames.push_back( strArgName1 );

  vecArgTypes.push_back( crArgType2 );
  vecArgNames.push_back( strArgName2 );

  _CreateIntrinsicDeclaration(strFunctionName, crReturnType, vecArgTypes, vecArgNames);
}

void InstructionSetBase::_CreateIntrinsicDeclaration(string strFunctionName, const QualType &crReturnType, const QualType &crArgType1, string strArgName1, const QualType &crArgType2, string strArgName2, const QualType &crArgType3, string strArgName3)
{
  ClangASTHelper::QualTypeVectorType  vecArgTypes;
  ClangASTHelper::StringVectorType    vecArgNames;

  vecArgTypes.push_back( crArgType1 );
  vecArgNames.push_back( strArgName1 );

  vecArgTypes.push_back( crArgType2 );
  vecArgNames.push_back( strArgName2 );

  vecArgTypes.push_back( crArgType3 );
  vecArgNames.push_back( strArgName3 );

  _CreateIntrinsicDeclaration(strFunctionName, crReturnType, vecArgTypes, vecArgNames);
}

void InstructionSetBase::_CreateMissingIntrinsicsSSE()
{
  // Get float vector type
  QualType  qtFloatVector = _GetFunctionReturnType("_mm_setzero_ps");

  // Create missing SSE intrinsic functions
  _CreateIntrinsicDeclaration( "_mm_shuffle_ps", qtFloatVector, qtFloatVector, "a", qtFloatVector, "b", _ASTHelper.GetASTContext().UnsignedIntTy, "imm" );
}

void InstructionSetBase::_CreateMissingIntrinsicsSSE2()
{
  // Get required types
  QualType  qtDoubleVector  = _GetFunctionReturnType("_mm_setzero_pd");
  QualType  qtIntegerVector = _GetFunctionReturnType("_mm_setzero_si128");
  QualType  qtInt           = _ASTHelper.GetASTContext().IntTy;

  // Create missing SSE2 intrinsic functions
  _CreateIntrinsicDeclaration( "_mm_slli_si128",      qtIntegerVector, qtIntegerVector, "a", qtInt,          "imm" );
  _CreateIntrinsicDeclaration( "_mm_srli_si128",      qtIntegerVector, qtIntegerVector, "a", qtInt,          "imm" );
  _CreateIntrinsicDeclaration( "_mm_shufflehi_epi16", qtIntegerVector, qtIntegerVector, "a", qtInt,          "imm" );
  _CreateIntrinsicDeclaration( "_mm_shufflelo_epi16", qtIntegerVector, qtIntegerVector, "a", qtInt,          "imm" );
  _CreateIntrinsicDeclaration( "_mm_shuffle_epi32",   qtIntegerVector, qtIntegerVector, "a", qtInt,          "imm" );
  _CreateIntrinsicDeclaration( "_mm_shuffle_pd",      qtDoubleVector,  qtDoubleVector,  "a", qtDoubleVector, "b", qtInt, "imm" );
}

void InstructionSetBase::_CreateMissingIntrinsicsSSE4_1()
{
  // Get required types
  QualType  qtFloatVector   = _GetFunctionReturnType("_mm_setzero_ps");
  QualType  qtIntegerVector = _GetFunctionReturnType("_mm_setzero_si128");
  QualType  qtInt64         = _ASTHelper.GetASTContext().LongLongTy;
  QualType  qtInt           = _ASTHelper.GetASTContext().IntTy;
  QualType  qtConstInt      = qtInt;
  qtConstInt.addConst();

  // Create missing SSE2 intrinsic functions
  _CreateIntrinsicDeclaration( "_mm_extract_ps",    qtInt,   qtFloatVector,   "a", qtConstInt, "imm");
  _CreateIntrinsicDeclaration( "_mm_extract_epi8",  qtInt,   qtIntegerVector, "a", qtConstInt, "imm");
  _CreateIntrinsicDeclaration( "_mm_extract_epi32", qtInt,   qtIntegerVector, "a", qtConstInt, "imm");
  _CreateIntrinsicDeclaration( "_mm_extract_epi64", qtInt64, qtIntegerVector, "a", qtConstInt, "imm");

  _CreateIntrinsicDeclaration( "_mm_insert_ps",    qtFloatVector,   qtFloatVector,   "a", qtFloatVector, "b", qtConstInt, "imm");
  _CreateIntrinsicDeclaration( "_mm_insert_epi8",  qtIntegerVector, qtIntegerVector, "a", qtInt,         "i", qtConstInt, "imm");
  _CreateIntrinsicDeclaration( "_mm_insert_epi32", qtIntegerVector, qtIntegerVector, "a", qtInt,         "i", qtConstInt, "imm");
  _CreateIntrinsicDeclaration( "_mm_insert_epi64", qtIntegerVector, qtIntegerVector, "a", qtInt64,       "i", qtConstInt, "imm");
}

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

QualType InstructionSetBase::_GetFunctionReturnType(string strFuntionName)
{
  auto vecFunctionsDecls = _GetFunctionDecl(strFuntionName);

  if (vecFunctionsDecls.size() != static_cast<size_t>(1))
  {
    throw InternalErrorException(string("The function declaration \"") + strFuntionName + string("\" is ambiguous!"));
  }

  return vecFunctionsDecls.front()->getResultType();
}



// Implementation of class InstructionSetSSE
InstructionSetSSE::InstructionSetSSE(ASTContext &rAstContext) : InstructionSetBase(rAstContext, _GetIntrinsicPrefix())
{
  _InitIntrinsicsMap();

  _CreateMissingIntrinsicsSSE();  // Only required due to Clang's incomplete intrinsic headers

  _LookupIntrinsics();
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
  _InitIntrinsic( IntrinsicsSSEEnum::ShuffleFloat,                "shuffle_ps"  );
  _InitIntrinsic( IntrinsicsSSEEnum::SqrtFloat,                   "sqrt_ps"     );
  _InitIntrinsic( IntrinsicsSSEEnum::StoreFloat,                  "storeu_ps"   );
  _InitIntrinsic( IntrinsicsSSEEnum::SubtractFloat,               "sub_ps"      );
  _InitIntrinsic( IntrinsicsSSEEnum::XorFloat,                    "xor_ps"      );
}

Expr* InstructionSetSSE::BroadCast(VectorElementTypes eElementType, Expr *pBroadCastValue)
{
  switch (eElementType)
  {
  case VectorElementTypes::Float: return _CreateFunctionCall(IntrinsicsSSEEnum::BroadCastFloat, pBroadCastValue);
  default:                        throw RuntimeErrorException("Only floating point data type supported for instruction set \"SSE\"!");
  }
}

Expr* InstructionSetSSE::CheckActiveElements(VectorElementTypes eMaskElementType, ActiveElementsCheckType eCheckType, Expr *pMaskExpr)
{
  switch (eMaskElementType)
  {
  case VectorElementTypes::Float:
  {
    int32_t             iTestValue      = (eCheckType == ActiveElementsCheckType::All) ? 0xF   : 0;
    BinaryOperatorKind  eCompareOpType  = (eCheckType == ActiveElementsCheckType::Any) ? BO_NE : BO_EQ;

    CallExpr        *pMoveMask      = _CreateFunctionCall(IntrinsicsSSEEnum::MoveMaskFloat, pMaskExpr);
    IntegerLiteral  *pTestConstant  = _GetASTHelper().CreateIntegerLiteral(iTestValue);

    return _GetASTHelper().CreateBinaryOperator( pMoveMask, pTestConstant, eCompareOpType, _GetASTHelper().GetASTContext().BoolTy );
  }
  default:  throw RuntimeErrorException("Only floating point data type supported for instruction set \"SSE\"!");
  }
}

Expr* InstructionSetSSE::CreateVector(VectorElementTypes eElementType, const ClangASTHelper::ExpressionVectorType &crvecElements, bool bReversedOrder)
{
  IntrinsicsSSEEnum eIntrinID = IntrinsicsSSEEnum::SetFloat;

  switch (eElementType)
  {
  case VectorElementTypes::Float:
    {
      eIntrinID = bReversedOrder ? IntrinsicsSSEEnum::SetReverseFloat : IntrinsicsSSEEnum::SetFloat;
      break;
    }
  default:  throw RuntimeErrorException("Only floating point data type supported for instruction set \"SSE\"!");
  }

  if (crvecElements.size() != GetVectorElementCount(eElementType))
  {
    throw RuntimeErrorException("The number of init expressions must be equal to the vector element count!");
  }

  return _CreateFunctionCall(eIntrinID, crvecElements);
}

Expr* InstructionSetSSE::CreateZeroVector(VectorElementTypes eElementType)
{
  switch (eElementType)
  {
  case VectorElementTypes::Float: return _CreateFunctionCall(IntrinsicsSSEEnum::SetZeroFloat);
  default:                        throw RuntimeErrorException("Only floating point data type supported for instruction set \"SSE\"!");
  }
}



// Implementation of class InstructionSetSSE2
InstructionSetSSE2::InstructionSetSSE2(ASTContext &rAstContext) : BaseType(rAstContext)
{
  _InitIntrinsicsMap();

  _CreateMissingIntrinsicsSSE2();  // Only required due to Clang's incomplete intrinsic headers

  _LookupIntrinsics();
}

void InstructionSetSSE2::_InitIntrinsicsMap()
{
  // Addition functions
  _InitIntrinsic( IntrinsicsSSE2Enum::AddDouble, "add_pd"    );
  _InitIntrinsic( IntrinsicsSSE2Enum::AddInt8,   "add_epi8"  );
  _InitIntrinsic( IntrinsicsSSE2Enum::AddInt16,  "add_epi16" );
  _InitIntrinsic( IntrinsicsSSE2Enum::AddInt32,  "add_epi32" );
  _InitIntrinsic( IntrinsicsSSE2Enum::AddInt64,  "add_epi64" );

  // Bitwise "and" and "and not"
  _InitIntrinsic( IntrinsicsSSE2Enum::AndDouble,     "and_pd"       );
  _InitIntrinsic( IntrinsicsSSE2Enum::AndInteger,    "and_si128"    );
  _InitIntrinsic( IntrinsicsSSE2Enum::AndNotDouble,  "andnot_pd"    );
  _InitIntrinsic( IntrinsicsSSE2Enum::AndNotInteger, "andnot_si128" );

  // Broadcast functions
  _InitIntrinsic( IntrinsicsSSE2Enum::BroadCastDouble, "set1_pd"     );
  _InitIntrinsic( IntrinsicsSSE2Enum::BroadCastInt8,   "set1_epi8"   );
  _InitIntrinsic( IntrinsicsSSE2Enum::BroadCastInt16,  "set1_epi16"  );
  _InitIntrinsic( IntrinsicsSSE2Enum::BroadCastInt32,  "set1_epi32"  );
  _InitIntrinsic( IntrinsicsSSE2Enum::BroadCastInt64,  "set1_epi64x" );

  // Vector cast functions (change bit-representation, no conversion)
  _InitIntrinsic( IntrinsicsSSE2Enum::CastDoubleToFloat,   "castpd_ps"    );
  _InitIntrinsic( IntrinsicsSSE2Enum::CastDoubleToInteger, "castpd_si128" );
  _InitIntrinsic( IntrinsicsSSE2Enum::CastFloatToDouble,   "castps_pd"    );
  _InitIntrinsic( IntrinsicsSSE2Enum::CastFloatToInteger,  "castps_si128" );
  _InitIntrinsic( IntrinsicsSSE2Enum::CastIntegerToDouble, "castsi128_pd" );
  _InitIntrinsic( IntrinsicsSSE2Enum::CastIntegerToFloat,  "castsi128_ps" );

  // Comparison methods
  {
    // Compare equal
    _InitIntrinsic( IntrinsicsSSE2Enum::CompareEqualDouble, "cmpeq_pd"    );
    _InitIntrinsic( IntrinsicsSSE2Enum::CompareEqualInt8,   "cmpeq_epi8"  );
    _InitIntrinsic( IntrinsicsSSE2Enum::CompareEqualInt16,  "cmpeq_epi16" );
    _InitIntrinsic( IntrinsicsSSE2Enum::CompareEqualInt32,  "cmpeq_epi32" );

    // Compare "greater equal"
    _InitIntrinsic( IntrinsicsSSE2Enum::CompareGreaterEqualDouble, "cmpge_pd" );

    // Compare "greater than"
    _InitIntrinsic( IntrinsicsSSE2Enum::CompareGreaterThanDouble, "cmpgt_pd"    );
    _InitIntrinsic( IntrinsicsSSE2Enum::CompareGreaterThanInt8,   "cmpgt_epi8"  );
    _InitIntrinsic( IntrinsicsSSE2Enum::CompareGreaterThanInt16,  "cmpgt_epi16" );
    _InitIntrinsic( IntrinsicsSSE2Enum::CompareGreaterThanInt32,  "cmpgt_epi32" );

    // Compare "less equal"
    _InitIntrinsic( IntrinsicsSSE2Enum::CompareLessEqualDouble, "cmple_pd" );

    // Compare "less than"
    _InitIntrinsic( IntrinsicsSSE2Enum::CompareLessThanDouble, "cmplt_pd"    );
    _InitIntrinsic( IntrinsicsSSE2Enum::CompareLessThanInt8,   "cmplt_epi8"  );
    _InitIntrinsic( IntrinsicsSSE2Enum::CompareLessThanInt16,  "cmplt_epi16" );
    _InitIntrinsic( IntrinsicsSSE2Enum::CompareLessThanInt32,  "cmplt_epi32" );

    // Negated comparison methods
    _InitIntrinsic( IntrinsicsSSE2Enum::CompareNotEqualDouble,        "cmpneq_pd" );
    _InitIntrinsic( IntrinsicsSSE2Enum::CompareNotGreaterEqualDouble, "cmpnge_pd" );
    _InitIntrinsic( IntrinsicsSSE2Enum::CompareNotGreaterThanDouble,  "cmpngt_pd" );
    _InitIntrinsic( IntrinsicsSSE2Enum::CompareNotLessEqualDouble,    "cmpnle_pd" );
    _InitIntrinsic( IntrinsicsSSE2Enum::CompareNotLessThanDouble,     "cmpnlt_pd" );
  }

  // Convert functions
  _InitIntrinsic( IntrinsicsSSE2Enum::ConvertDoubleFloat, "cvtpd_ps"     );
  _InitIntrinsic( IntrinsicsSSE2Enum::ConvertDoubleInt32, "cvttpd_epi32" );   // Use truncation
  _InitIntrinsic( IntrinsicsSSE2Enum::ConvertFloatDouble, "cvtps_pd"     );
  _InitIntrinsic( IntrinsicsSSE2Enum::ConvertFloatInt32,  "cvttps_epi32" );   // Use truncation
  _InitIntrinsic( IntrinsicsSSE2Enum::ConvertInt32Double, "cvtepi32_pd"  );
  _InitIntrinsic( IntrinsicsSSE2Enum::ConvertInt32Float,  "cvtepi32_ps"  );

  // Division functions
  _InitIntrinsic( IntrinsicsSSE2Enum::DivideDouble, "div_pd" );

  // Extract / Insert functions
  _InitIntrinsic( IntrinsicsSSE2Enum::ExtractInt16, "extract_epi16" );
  _InitIntrinsic( IntrinsicsSSE2Enum::InsertInt16,  "insert_epi16"  );

  // Load functions
  _InitIntrinsic( IntrinsicsSSE2Enum::LoadDouble,  "loadu_pd"   );
  _InitIntrinsic( IntrinsicsSSE2Enum::LoadInteger, "loadu_si128" );

  // Maximum / Minimum functions
  _InitIntrinsic( IntrinsicsSSE2Enum::MaxDouble, "max_pd"    );
  _InitIntrinsic( IntrinsicsSSE2Enum::MaxUInt8,  "max_epu8"  );
  _InitIntrinsic( IntrinsicsSSE2Enum::MaxInt16,  "max_epi16" );
  _InitIntrinsic( IntrinsicsSSE2Enum::MinDouble, "min_pd"    );
  _InitIntrinsic( IntrinsicsSSE2Enum::MinUInt8,  "min_epu8"  );
  _InitIntrinsic( IntrinsicsSSE2Enum::MinInt16,  "min_epi16" );

  // Mask conversion functions
  _InitIntrinsic( IntrinsicsSSE2Enum::MoveMaskDouble, "movemask_epi8" );
  _InitIntrinsic( IntrinsicsSSE2Enum::MoveMaskInt8,   "movemask_pd"   );

  // Multiplication functions
  _InitIntrinsic( IntrinsicsSSE2Enum::MultiplyDouble, "mul_pd"      );
  _InitIntrinsic( IntrinsicsSSE2Enum::MultiplyInt16,  "mullo_epi16" );

  // Bitwise "or" functions
  _InitIntrinsic( IntrinsicsSSE2Enum::OrDouble,  "or_pd"    );
  _InitIntrinsic( IntrinsicsSSE2Enum::OrInteger, "or_si128" );

  // Integer packing functions
  _InitIntrinsic( IntrinsicsSSE2Enum::PackInt16ToInt8,  "packs_epi16"  );
  _InitIntrinsic( IntrinsicsSSE2Enum::PackInt16ToUInt8, "packus_epi16" );
  _InitIntrinsic( IntrinsicsSSE2Enum::PackInt32ToInt16, "packs_epi32"  );

  // Set methods
  _InitIntrinsic( IntrinsicsSSE2Enum::SetDouble, "set_pd"     );
  _InitIntrinsic( IntrinsicsSSE2Enum::SetInt8,   "set_epi8"   );
  _InitIntrinsic( IntrinsicsSSE2Enum::SetInt16,  "set_epi16"  );
  _InitIntrinsic( IntrinsicsSSE2Enum::SetInt32,  "set_epi32"  );
  _InitIntrinsic( IntrinsicsSSE2Enum::SetInt64,  "set_epi64x" );

  // Set reverse methods
  _InitIntrinsic( IntrinsicsSSE2Enum::SetReverseDouble, "setr_pd"     );
  _InitIntrinsic( IntrinsicsSSE2Enum::SetReverseInt8,   "setr_epi8"   );
  _InitIntrinsic( IntrinsicsSSE2Enum::SetReverseInt16,  "setr_epi16"  );
  _InitIntrinsic( IntrinsicsSSE2Enum::SetReverseInt32,  "setr_epi32"  );

  // Zero vector creation functions
  _InitIntrinsic( IntrinsicsSSE2Enum::SetZeroDouble,  "setzero_pd"    );
  _InitIntrinsic( IntrinsicsSSE2Enum::SetZeroInteger, "setzero_si128" );

  // Shift functions
  _InitIntrinsic( IntrinsicsSSE2Enum::ShiftLeftInt16,        "sll_epi16"  );
  _InitIntrinsic( IntrinsicsSSE2Enum::ShiftLeftInt32,        "sll_epi32"  );
  _InitIntrinsic( IntrinsicsSSE2Enum::ShiftLeftVectorBytes,  "slli_si128" );
  _InitIntrinsic( IntrinsicsSSE2Enum::ShiftRightInt16,       "sra_epi16"  );
  _InitIntrinsic( IntrinsicsSSE2Enum::ShiftRightInt32,       "sra_epi32"  );
  _InitIntrinsic( IntrinsicsSSE2Enum::ShiftRightVectorBytes, "srli_si128" );

  // Shuffle functions
  _InitIntrinsic( IntrinsicsSSE2Enum::ShuffleDouble,    "shuffle_pd"      );
  _InitIntrinsic( IntrinsicsSSE2Enum::ShuffleInt16High, "shufflehi_epi16" );
  _InitIntrinsic( IntrinsicsSSE2Enum::ShuffleInt16Low,  "shufflelo_epi16" );
  _InitIntrinsic( IntrinsicsSSE2Enum::ShuffleInt32,     "shuffle_epi32"   );

  // Square root functions
  _InitIntrinsic( IntrinsicsSSE2Enum::SqrtDouble, "sqrt_pd" );

  // Store functions
  _InitIntrinsic( IntrinsicsSSE2Enum::StoreDouble,             "storeu_pd"       );
  _InitIntrinsic( IntrinsicsSSE2Enum::StoreInteger,            "storeu_si128"    );
  _InitIntrinsic( IntrinsicsSSE2Enum::StoreConditionalInteger, "maskmoveu_si128" );

  // Subtraction functions
  _InitIntrinsic( IntrinsicsSSE2Enum::SubtractDouble, "sub_pd"    );
  _InitIntrinsic( IntrinsicsSSE2Enum::SubtractInt8,   "sub_epi8"  );
  _InitIntrinsic( IntrinsicsSSE2Enum::SubtractInt16,  "sub_epi16" );
  _InitIntrinsic( IntrinsicsSSE2Enum::SubtractInt32,  "sub_epi32" );
  _InitIntrinsic( IntrinsicsSSE2Enum::SubtractInt64,  "sub_epi64" );

  // Bitwise "xor" functions
  _InitIntrinsic( IntrinsicsSSE2Enum::XorDouble,  "xor_pd"    );
  _InitIntrinsic( IntrinsicsSSE2Enum::XorInteger, "xor_si128" );
}

Expr* InstructionSetSSE2::BroadCast(VectorElementTypes eElementType, Expr *pBroadCastValue)
{
  switch (eElementType)
  {
  case VectorElementTypes::Double:                                  return _CreateFunctionCall(IntrinsicsSSE2Enum::BroadCastDouble, pBroadCastValue);
  case VectorElementTypes::Int8:  case VectorElementTypes::UInt8:   return _CreateFunctionCall(IntrinsicsSSE2Enum::BroadCastInt8,   pBroadCastValue);
  case VectorElementTypes::Int16: case VectorElementTypes::UInt16:  return _CreateFunctionCall(IntrinsicsSSE2Enum::BroadCastInt16,  pBroadCastValue);
  case VectorElementTypes::Int32: case VectorElementTypes::UInt32:  return _CreateFunctionCall(IntrinsicsSSE2Enum::BroadCastInt32,  pBroadCastValue);
  case VectorElementTypes::Int64: case VectorElementTypes::UInt64:  return _CreateFunctionCall(IntrinsicsSSE2Enum::BroadCastInt64,  pBroadCastValue);
  default:                                                          return BaseType::BroadCast(eElementType, pBroadCastValue);
  }
}

Expr* InstructionSetSSE2::CheckActiveElements(VectorElementTypes eMaskElementType, ActiveElementsCheckType eCheckType, Expr *pMaskExpr)
{
  int32_t             iTestValue      = 0;
  IntrinsicsSSE2Enum  eMoveMaskID     = IntrinsicsSSE2Enum::MoveMaskDouble;

  switch (eMaskElementType)
  {
  case VectorElementTypes::Double:
    {
      eMoveMaskID = IntrinsicsSSE2Enum::MoveMaskDouble;
      iTestValue  = (eCheckType == ActiveElementsCheckType::All) ? 0x3 : 0;
      break;
    }
  case VectorElementTypes::Int8:  case VectorElementTypes::Int16:  case VectorElementTypes::Int32:  case VectorElementTypes::Int64:
  case VectorElementTypes::UInt8: case VectorElementTypes::UInt16: case VectorElementTypes::UInt32: case VectorElementTypes::UInt64:
    {
      eMoveMaskID = IntrinsicsSSE2Enum::MoveMaskInt8;
      iTestValue  = (eCheckType == ActiveElementsCheckType::All) ? 0xFFFF : 0;
      break;
    }

  default:  return BaseType::CheckActiveElements(eMaskElementType, eCheckType, pMaskExpr);
  }

  CallExpr        *pMoveMask      = _CreateFunctionCall(eMoveMaskID, pMaskExpr);
  IntegerLiteral  *pTestConstant  = _GetASTHelper().CreateIntegerLiteral(iTestValue);

  BinaryOperatorKind  eCompareOpType = (eCheckType == ActiveElementsCheckType::Any) ? BO_NE : BO_EQ;

  return _GetASTHelper().CreateBinaryOperator(pMoveMask, pTestConstant, eCompareOpType, _GetASTHelper().GetASTContext().BoolTy);
}

Expr* InstructionSetSSE2::CreateVector(VectorElementTypes eElementType, const ClangASTHelper::ExpressionVectorType &crvecElements, bool bReversedOrder)
{
  IntrinsicsSSE2Enum eIntrinID = IntrinsicsSSE2Enum::SetDouble;

  switch (eElementType)
  {
  case VectorElementTypes::Double:
    {
      eIntrinID = bReversedOrder ? IntrinsicsSSE2Enum::SetReverseDouble : IntrinsicsSSE2Enum::SetDouble;
      break;
    }
  case VectorElementTypes::Int8:  case VectorElementTypes::UInt8:
    {
      eIntrinID = bReversedOrder ? IntrinsicsSSE2Enum::SetReverseInt8 : IntrinsicsSSE2Enum::SetInt8;
      break;
    }
  case VectorElementTypes::Int16: case VectorElementTypes::UInt16:
    {
      eIntrinID = bReversedOrder ? IntrinsicsSSE2Enum::SetReverseInt16 : IntrinsicsSSE2Enum::SetInt16;
      break;
    }
  case VectorElementTypes::Int32: case VectorElementTypes::UInt32:
    {
      eIntrinID = bReversedOrder ? IntrinsicsSSE2Enum::SetReverseInt32 : IntrinsicsSSE2Enum::SetInt32;
      break;
    }
  case VectorElementTypes::Int64: case VectorElementTypes::UInt64:
    {
      if (bReversedOrder)
      {
        ClangASTHelper::ExpressionVectorType vecElements;

        for (auto itElem = crvecElements.end(); itElem != crvecElements.begin(); itElem--)
        {
          vecElements.push_back( *(itElem-1) );
        }

        return CreateVector(eElementType, vecElements, false);
      }
      else
      {
        eIntrinID = IntrinsicsSSE2Enum::SetInt64;
      }

      break;
    }
  default:  BaseType::CreateVector(eElementType, crvecElements, bReversedOrder);
  }

  if (crvecElements.size() != GetVectorElementCount(eElementType))
  {
    throw RuntimeErrorException("The number of init expressions must be equal to the vector element count!");
  }

  return _CreateFunctionCall(eIntrinID, crvecElements);
}

Expr* InstructionSetSSE2::CreateZeroVector(VectorElementTypes eElementType)
{
  switch ( eElementType )
  {
  case VectorElementTypes::Double:  return _CreateFunctionCall(IntrinsicsSSE2Enum::SetZeroDouble);
  case VectorElementTypes::Int8:
  case VectorElementTypes::Int16:
  case VectorElementTypes::Int32:
  case VectorElementTypes::Int64:
  case VectorElementTypes::UInt8:
  case VectorElementTypes::UInt16:
  case VectorElementTypes::UInt32:
  case VectorElementTypes::UInt64:  return _CreateFunctionCall(IntrinsicsSSE2Enum::SetZeroInteger);
  default:                          return BaseType::CreateZeroVector( eElementType );
  }
}



// Implementation of class InstructionSetSSE3
InstructionSetSSE3::InstructionSetSSE3(ASTContext &rAstContext) : BaseType(rAstContext)
{
  _InitIntrinsicsMap();

  _LookupIntrinsics();
}

void InstructionSetSSE3::_InitIntrinsicsMap()
{
  _InitIntrinsic (IntrinsicsSSE3Enum::LoadInteger, "lddqu_si128" );
}



// Implementation of class InstructionSetSSSE3
InstructionSetSSSE3::InstructionSetSSSE3(ASTContext &rAstContext) : BaseType(rAstContext)
{
  _InitIntrinsicsMap();

  _LookupIntrinsics();
}

void InstructionSetSSSE3::_InitIntrinsicsMap()
{
  // Absolute value computation functions
  _InitIntrinsic( IntrinsicsSSSE3Enum::AbsoluteInt8,  "abs_epi8"  );
  _InitIntrinsic( IntrinsicsSSSE3Enum::AbsoluteInt16, "abs_epi16" );
  _InitIntrinsic( IntrinsicsSSSE3Enum::AbsoluteInt32, "abs_epi32" );

  // Shuffle functions
  _InitIntrinsic( IntrinsicsSSSE3Enum::ShuffleInt8, "shuffle_epi8" );

  // Sign change functions
  _InitIntrinsic( IntrinsicsSSSE3Enum::SignInt8,  "sign_epi8"  );
  _InitIntrinsic( IntrinsicsSSSE3Enum::SignInt16, "sign_epi16" );
  _InitIntrinsic( IntrinsicsSSSE3Enum::SignInt32, "sign_epi32" );
}



// Implementation of class InstructionSetSSE4_1
InstructionSetSSE4_1::InstructionSetSSE4_1(ASTContext &rAstContext) : BaseType(rAstContext)
{
  _InitIntrinsicsMap();

  _CreateMissingIntrinsicsSSE4_1();

  _LookupIntrinsics();
}

void InstructionSetSSE4_1::_InitIntrinsicsMap()
{
  // Blending functions
  _InitIntrinsic( IntrinsicsSSE4_1Enum::BlendDouble,  "blendv_pd"   );
  _InitIntrinsic( IntrinsicsSSE4_1Enum::BlendFloat,   "blendv_ps"   );
  _InitIntrinsic( IntrinsicsSSE4_1Enum::BlendInteger, "blendv_epi8" );

  // Comparison functions
  _InitIntrinsic( IntrinsicsSSE4_1Enum::CompareEqualInt64, "cmpeq_epi64" );

  // Convert functions for signed integers
  _InitIntrinsic( IntrinsicsSSE4_1Enum::ConvertInt8Int16,  "cvtepi8_epi16"  );
  _InitIntrinsic( IntrinsicsSSE4_1Enum::ConvertInt8Int32,  "cvtepi8_epi32"  );
  _InitIntrinsic( IntrinsicsSSE4_1Enum::ConvertInt8Int64,  "cvtepi8_epi64"  );
  _InitIntrinsic( IntrinsicsSSE4_1Enum::ConvertInt16Int32, "cvtepi16_epi32" );
  _InitIntrinsic( IntrinsicsSSE4_1Enum::ConvertInt16Int64, "cvtepi16_epi64" );
  _InitIntrinsic( IntrinsicsSSE4_1Enum::ConvertInt32Int64, "cvtepi32_epi64" );

  // Convert functions for unsigned integers
  _InitIntrinsic( IntrinsicsSSE4_1Enum::ConvertUInt8Int16,  "cvtepu8_epi16"  );
  _InitIntrinsic( IntrinsicsSSE4_1Enum::ConvertUInt8Int32,  "cvtepu8_epi32"  );
  _InitIntrinsic( IntrinsicsSSE4_1Enum::ConvertUInt8Int64,  "cvtepu8_epi64"  );
  _InitIntrinsic( IntrinsicsSSE4_1Enum::ConvertUInt16Int32, "cvtepu16_epi32" );
  _InitIntrinsic( IntrinsicsSSE4_1Enum::ConvertUInt16Int64, "cvtepu16_epi64" );
  _InitIntrinsic( IntrinsicsSSE4_1Enum::ConvertUInt32Int64, "cvtepu32_epi64" );

  // Extract functions
  _InitIntrinsic( IntrinsicsSSE4_1Enum::ExtractFloat, "extract_ps"    );
  _InitIntrinsic( IntrinsicsSSE4_1Enum::ExtractInt8,  "extract_epi8"  );
  _InitIntrinsic( IntrinsicsSSE4_1Enum::ExtractInt32, "extract_epi32" );
  _InitIntrinsic( IntrinsicsSSE4_1Enum::ExtractInt64, "extract_epi64" );

  // Insert functions
  _InitIntrinsic( IntrinsicsSSE4_1Enum::InsertFloat, "insert_ps"    );
  _InitIntrinsic( IntrinsicsSSE4_1Enum::InsertInt8,  "insert_epi8"  );
  _InitIntrinsic( IntrinsicsSSE4_1Enum::InsertInt32, "insert_epi32" );
  _InitIntrinsic( IntrinsicsSSE4_1Enum::InsertInt64, "insert_epi64" );

  // Maximum functions
  _InitIntrinsic( IntrinsicsSSE4_1Enum::MaxInt8,   "max_epi8"  );
  _InitIntrinsic( IntrinsicsSSE4_1Enum::MaxInt32,  "max_epi32" );
  _InitIntrinsic( IntrinsicsSSE4_1Enum::MaxUInt16, "max_epu16" );
  _InitIntrinsic( IntrinsicsSSE4_1Enum::MaxUInt32, "max_epu32" );

  // Minimum functions
  _InitIntrinsic( IntrinsicsSSE4_1Enum::MinInt8,   "min_epi8"  );
  _InitIntrinsic( IntrinsicsSSE4_1Enum::MinInt32,  "min_epi32" );
  _InitIntrinsic( IntrinsicsSSE4_1Enum::MinUInt16, "min_epu16" );
  _InitIntrinsic( IntrinsicsSSE4_1Enum::MinUInt32, "min_epu32" );

  // Multiply functions
  _InitIntrinsic( IntrinsicsSSE4_1Enum::MultiplyInt32, "mullo_epi32" );

  // Packing functions
  _InitIntrinsic( IntrinsicsSSE4_1Enum::PackInt32ToUInt16, "packus_epi32" );

  // Testing functions
  _InitIntrinsic( IntrinsicsSSE4_1Enum::TestControl, "testc_si128" );
}



// vim: set ts=2 sw=2 sts=2 et ai:

