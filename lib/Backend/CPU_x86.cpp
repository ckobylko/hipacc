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

//===--- CPU_x86.cpp - Implements the C++ code generator for x86-based CPUs. ---------===//
//
// This file implements the C++ code generator for CPUs which are based on the x86-microarchitecture.
//
//===---------------------------------------------------------------------------------===//

#include "hipacc/AST/ASTNode.h"
#include "hipacc/Backend/CPU_x86.h"
#include "hipacc/Backend/Vectorizer.h"
#include <list>
#include <map>
#include <sstream>
#include <utility>

using namespace clang::hipacc::Backend;
using namespace clang::hipacc;
using namespace clang;
using namespace std;


ArraySubscriptExpr* CPU_x86::DumpInstructionSet::_CreateArraySubscript(DeclRefExpr *pArrayRef, int32_t iIndex)
{
  return _ASTHelper.CreateArraySubscriptExpression( pArrayRef, _ASTHelper.CreateLiteral(iIndex), pArrayRef->getType()->getAsArrayTypeUnsafe()->getElementType() );
}

StringLiteral* CPU_x86::DumpInstructionSet::_CreateElementTypeString(VectorElementTypes eElementType)
{
  return _ASTHelper.CreateStringLiteral( TypeInfo::GetTypeString(eElementType) );
}

QualType CPU_x86::DumpInstructionSet::_GetClangType(VectorElementTypes eElementType)
{
  switch (eElementType)
  {
  case VectorElementTypes::Double:  return _ASTHelper.GetASTContext().DoubleTy;
  case VectorElementTypes::Float:   return _ASTHelper.GetASTContext().FloatTy;
  case VectorElementTypes::Int8:    return _ASTHelper.GetASTContext().CharTy;
  case VectorElementTypes::UInt8:   return _ASTHelper.GetASTContext().UnsignedCharTy;
  case VectorElementTypes::Int16:   return _ASTHelper.GetASTContext().ShortTy;
  case VectorElementTypes::UInt16:  return _ASTHelper.GetASTContext().UnsignedShortTy;
  case VectorElementTypes::Int32:   return _ASTHelper.GetASTContext().IntTy;
  case VectorElementTypes::UInt32:  return _ASTHelper.GetASTContext().UnsignedIntTy;
  case VectorElementTypes::Int64:   return _ASTHelper.GetASTContext().LongLongTy;
  case VectorElementTypes::UInt64:  return _ASTHelper.GetASTContext().UnsignedLongLongTy;
  default:                          throw InternalErrorException( "CPU_x86::DumpInstructionSet::_GetClangType() -> Unsupported vector element type detected!" );
  }
}


FunctionDecl* CPU_x86::DumpInstructionSet::_DumpInstructionSet(Vectorization::InstructionSetBasePtr spInstructionSet, string strFunctionName)
{
  #define DUMP_INSTR(__container, __instr)  try{ __container.push_back(__instr); } catch (exception &e) { __container.push_back( _ASTHelper.CreateStringLiteral(e.what()) ); }

  typedef map< VectorElementTypes, DeclRefExpr* >     VectorDeclRefMapType;
  
  list< VectorElementTypes > lstSupportedElementTypes;
  if (spInstructionSet->IsElementTypeSupported(VectorElementTypes::Double))   lstSupportedElementTypes.push_back(VectorElementTypes::Double);
  if (spInstructionSet->IsElementTypeSupported(VectorElementTypes::Float))    lstSupportedElementTypes.push_back(VectorElementTypes::Float);
  if (spInstructionSet->IsElementTypeSupported(VectorElementTypes::Int8))     lstSupportedElementTypes.push_back(VectorElementTypes::Int8);
  if (spInstructionSet->IsElementTypeSupported(VectorElementTypes::UInt8))    lstSupportedElementTypes.push_back(VectorElementTypes::UInt8);
  if (spInstructionSet->IsElementTypeSupported(VectorElementTypes::Int16))    lstSupportedElementTypes.push_back(VectorElementTypes::Int16);
  if (spInstructionSet->IsElementTypeSupported(VectorElementTypes::UInt16))   lstSupportedElementTypes.push_back(VectorElementTypes::UInt16);
  if (spInstructionSet->IsElementTypeSupported(VectorElementTypes::Int32))    lstSupportedElementTypes.push_back(VectorElementTypes::Int32);
  if (spInstructionSet->IsElementTypeSupported(VectorElementTypes::UInt32))   lstSupportedElementTypes.push_back(VectorElementTypes::UInt32);
  if (spInstructionSet->IsElementTypeSupported(VectorElementTypes::Int64))    lstSupportedElementTypes.push_back(VectorElementTypes::Int64);
  if (spInstructionSet->IsElementTypeSupported(VectorElementTypes::UInt64))   lstSupportedElementTypes.push_back(VectorElementTypes::UInt64);


  FunctionDecl *pFunctionDecl = _ASTHelper.CreateFunctionDeclaration(strFunctionName, _ASTHelper.GetASTContext().VoidTy, ClangASTHelper::StringVectorType(), ClangASTHelper::QualTypeVectorType());

  ClangASTHelper::StatementVectorType vecBody;

  // Create variable declarations
  VectorDeclRefMapType   mapVectorArrayDecls;
  {
    vecBody.push_back( _ASTHelper.CreateStringLiteral("Vector declarations") );

    for (auto itElementType : lstSupportedElementTypes)
    {
      const size_t cszArraySize = std::max( spInstructionSet->GetVectorWidthBytes() / spInstructionSet->GetVectorElementCount(itElementType), static_cast<size_t>(2) );
      QualType qtDeclType       = _ASTHelper.GetConstantArrayType( spInstructionSet->GetVectorType(itElementType), cszArraySize );
      VarDecl *pDecl            = _ASTHelper.CreateVariableDeclaration( pFunctionDecl, string("mm") + TypeInfo::GetTypeString(itElementType), qtDeclType, nullptr );

      mapVectorArrayDecls[itElementType] = _ASTHelper.CreateDeclarationReferenceExpression( pDecl );

      vecBody.push_back( _ASTHelper.CreateDeclarationStatement(pDecl) );
    }

    vecBody.push_back( _ASTHelper.CreateStringLiteral("") );
  }

  // Dump arithmetic operators
  if (_uiDumpFlags & DF_Arithmetic)
  {
    typedef Vectorization::AST::Expressions::ArithmeticOperator   ArithmeticOperator;
    typedef ArithmeticOperator::ArithmeticOperatorType            ArithmeticOperatorType;

    ArithmeticOperatorType aeArithOps[] = { ArithmeticOperatorType::Add,        ArithmeticOperatorType::BitwiseAnd, ArithmeticOperatorType::BitwiseOr,  ArithmeticOperatorType::BitwiseXOr,
                                            ArithmeticOperatorType::Divide,     ArithmeticOperatorType::Modulo,     ArithmeticOperatorType::Multiply,   ArithmeticOperatorType::ShiftLeft,
                                            ArithmeticOperatorType::ShiftRight, ArithmeticOperatorType::Subtract };


    vecBody.push_back( _ASTHelper.CreateStringLiteral("ArithmeticOperator") );

    ClangASTHelper::StatementVectorType vecArithmeticOperators;

    for (auto eCurrentOp : aeArithOps)
    {
      vecArithmeticOperators.push_back( _ASTHelper.CreateStringLiteral( ArithmeticOperator::GetOperatorTypeString(eCurrentOp) ) );

      ClangASTHelper::StatementVectorType vecCurrentOp;

      for (auto itElementType : lstSupportedElementTypes)
      {
        auto itArrayDecl = mapVectorArrayDecls[itElementType];

        vecCurrentOp.push_back( _CreateElementTypeString(itElementType) );
        DUMP_INSTR( vecCurrentOp, spInstructionSet->ArithmeticOperator( itElementType, eCurrentOp, _CreateArraySubscript(itArrayDecl, 0), _CreateArraySubscript(itArrayDecl, 1) ) );
      }

      vecArithmeticOperators.push_back( _ASTHelper.CreateCompoundStatement(vecCurrentOp) );
      vecArithmeticOperators.push_back( _ASTHelper.CreateStringLiteral("") );
    }

    vecBody.push_back( _ASTHelper.CreateCompoundStatement(vecArithmeticOperators) );
    vecBody.push_back( _ASTHelper.CreateStringLiteral("") );
  }

  // Dump blend functions
  if (_uiDumpFlags & DF_Blend)
  {
    vecBody.push_back( _ASTHelper.CreateStringLiteral("BlendVectors") );

    ClangASTHelper::StatementVectorType vecBlendVectors;

    // Create mask declarations
    vecBlendVectors.push_back( _ASTHelper.CreateStringLiteral("MaskDeclarations") );
    VectorDeclRefMapType mapMaskDecls;
    for (auto itElementType : lstSupportedElementTypes)
    {
      VarDecl *pMaskDecl = _ASTHelper.CreateVariableDeclaration( pFunctionDecl, string("mmMask") + TypeInfo::GetTypeString(itElementType), spInstructionSet->GetVectorType(itElementType), nullptr );

      mapMaskDecls[ itElementType ] = _ASTHelper.CreateDeclarationReferenceExpression( pMaskDecl );

      vecBlendVectors.push_back( _ASTHelper.CreateDeclarationStatement(pMaskDecl) );
    }
    vecBlendVectors.push_back( _ASTHelper.CreateStringLiteral("") );

    for (auto itElementType : lstSupportedElementTypes)
    {
      vecBlendVectors.push_back( _CreateElementTypeString(itElementType) );

      auto itArrayDecl = mapVectorArrayDecls[itElementType];

      DUMP_INSTR( vecBlendVectors, spInstructionSet->BlendVectors( itElementType, mapMaskDecls[ itElementType ], _CreateArraySubscript(itArrayDecl, 0), _CreateArraySubscript(itArrayDecl, 1) ) );
    }

    vecBody.push_back( _ASTHelper.CreateCompoundStatement(vecBlendVectors) );
    vecBody.push_back( _ASTHelper.CreateStringLiteral("") );
  }

  // Dump broadcasts
  if (_uiDumpFlags & DF_BroadCast)
  {
    vecBody.push_back( _ASTHelper.CreateStringLiteral("BroadCasts") );

    ClangASTHelper::StatementVectorType vecBroadCasts;

    for (auto itElementType : lstSupportedElementTypes)
    {
      vecBroadCasts.push_back( _CreateElementTypeString(itElementType) );

      DUMP_INSTR( vecBroadCasts, spInstructionSet->BroadCast( itElementType, _ASTHelper.CreateLiteral(1) ) );
    }

    vecBody.push_back( _ASTHelper.CreateCompoundStatement(vecBroadCasts) );
    vecBody.push_back( _ASTHelper.CreateStringLiteral("") );
  }

  // Dump check active elements
  if (_uiDumpFlags & DF_CheckActive)
  {
    typedef Vectorization::AST::VectorSupport::CheckActiveElements    CheckActiveElements;
    typedef CheckActiveElements::CheckType                            CheckType;

    CheckType aeCheckTypes[] = { CheckType::All, CheckType::Any, CheckType::None };

    vecBody.push_back( _ASTHelper.CreateStringLiteral("CheckActiveElements") );

    ClangASTHelper::StatementVectorType vecCheckActiveElements;

    for (auto eCheckType : aeCheckTypes)
    {
      vecCheckActiveElements.push_back( _ASTHelper.CreateStringLiteral( CheckActiveElements::GetCheckTypeString(eCheckType) ) );

      ClangASTHelper::StatementVectorType vecCurrentCheck;

      for (auto itElementType : lstSupportedElementTypes)
      {
        vecCurrentCheck.push_back( _CreateElementTypeString(itElementType) );

        auto itArrayDecl = mapVectorArrayDecls[itElementType];

        DUMP_INSTR( vecCurrentCheck, spInstructionSet->CheckActiveElements( itElementType, eCheckType, _CreateArraySubscript(itArrayDecl, 0) ) );
      }

      vecCheckActiveElements.push_back( _ASTHelper.CreateCompoundStatement(vecCurrentCheck) );
      vecCheckActiveElements.push_back( _ASTHelper.CreateStringLiteral("") );
    }

    vecBody.push_back( _ASTHelper.CreateCompoundStatement(vecCheckActiveElements) );
    vecBody.push_back( _ASTHelper.CreateStringLiteral("") );
  }

  // Dump conversion
  if (_uiDumpFlags & DF_Convert)
  {
    vecBody.push_back( _ASTHelper.CreateStringLiteral("ConvertVector") );

    ClangASTHelper::StatementVectorType vecConvertVector;

    for (int i = 0; i <= 1; ++i)
    {
      const bool cbMaskConversion = (i == 0);

      vecConvertVector.push_back( _ASTHelper.CreateStringLiteral( cbMaskConversion ? "Mask conversion" : "Vector conversion" ) );

      ClangASTHelper::StatementVectorType vecCurrentConvertFrom;

      for (auto itElementTypeFrom : lstSupportedElementTypes)
      {
        vecCurrentConvertFrom.push_back( _ASTHelper.CreateStringLiteral( string("Convert ") + TypeInfo::GetTypeString(itElementTypeFrom) + string(" to ...") ) );

        ClangASTHelper::StatementVectorType vecCurrentConvertTo;

        auto itArrayDecl = mapVectorArrayDecls[itElementTypeFrom];

        for (auto itElementTypeTo : lstSupportedElementTypes)
        {
          vecCurrentConvertTo.push_back( _CreateElementTypeString(itElementTypeTo) );

          const size_t cszSizeFrom  = TypeInfo::GetTypeSize( itElementTypeFrom );
          const size_t cszSizeTo    = TypeInfo::GetTypeSize( itElementTypeTo );

          if (cszSizeFrom == cszSizeTo)
          {
            Expr *pVectorRef = _CreateArraySubscript(itArrayDecl, 0);

            if (cbMaskConversion)
            {
              DUMP_INSTR( vecCurrentConvertTo, spInstructionSet->ConvertMaskSameSize( itElementTypeFrom, itElementTypeTo, pVectorRef ) );
            }
            else
            {
              DUMP_INSTR( vecCurrentConvertTo, spInstructionSet->ConvertVectorSameSize( itElementTypeFrom, itElementTypeTo, pVectorRef ) );
            }
          }
          else if (cszSizeFrom < cszSizeTo)
          {
            Expr *pVectorRef = _CreateArraySubscript(itArrayDecl, 0);

            for (uint32_t uiGroupIndex = 0; uiGroupIndex < static_cast<uint32_t>(cszSizeTo / cszSizeFrom); ++uiGroupIndex)
            {
              if (cbMaskConversion)
              {
                DUMP_INSTR( vecCurrentConvertTo, spInstructionSet->ConvertMaskUp( itElementTypeFrom, itElementTypeTo, pVectorRef, uiGroupIndex ) );
              }
              else
              {
                DUMP_INSTR( vecCurrentConvertTo, spInstructionSet->ConvertVectorUp( itElementTypeFrom, itElementTypeTo, pVectorRef, uiGroupIndex ) );
              }
            }
          }
          else
          {
            ClangASTHelper::ExpressionVectorType vecElements;
            for (int32_t iElem = 0; iElem < static_cast<int32_t>(cszSizeFrom / cszSizeTo); ++iElem)
            {
              vecElements.push_back( _CreateArraySubscript(itArrayDecl, iElem) );
            }

            if (cbMaskConversion)
            {
              DUMP_INSTR( vecCurrentConvertTo, spInstructionSet->ConvertMaskDown( itElementTypeFrom, itElementTypeTo, vecElements ) );
            }
            else
            {
              DUMP_INSTR( vecCurrentConvertTo, spInstructionSet->ConvertVectorDown( itElementTypeFrom, itElementTypeTo, vecElements ) );
            }
          }
        }

        vecCurrentConvertFrom.push_back( _ASTHelper.CreateCompoundStatement(vecCurrentConvertTo) );
        vecCurrentConvertFrom.push_back( _ASTHelper.CreateStringLiteral("") );
      }

      vecConvertVector.push_back( _ASTHelper.CreateCompoundStatement(vecCurrentConvertFrom) );
      vecConvertVector.push_back( _ASTHelper.CreateStringLiteral("") );
    }

    vecBody.push_back( _ASTHelper.CreateCompoundStatement(vecConvertVector) );
    vecBody.push_back( _ASTHelper.CreateStringLiteral("") );
  }

  // Dump create vector
  if (_uiDumpFlags & DF_CreateVector)
  {
    vecBody.push_back(_ASTHelper.CreateStringLiteral("CreateVector"));

    ClangASTHelper::StatementVectorType vecCreateVector;

    const char *apcDumpName[] = { "Normal order", "Reversed order", "Create ones", "Create negative ones", "Create zeros" };

    for (int i = 0; i < 5; ++i)
    {
      vecCreateVector.push_back(_ASTHelper.CreateStringLiteral(apcDumpName[i]));

      ClangASTHelper::StatementVectorType vecCurrentCreate;

      for (auto itElementType : lstSupportedElementTypes)
      {
        vecCurrentCreate.push_back(_CreateElementTypeString(itElementType));

        if (i <= 1)
        {
          ClangASTHelper::ExpressionVectorType vecCreateArgs;
          for (int32_t iArgIdx = 0; iArgIdx < static_cast<int32_t>(spInstructionSet->GetVectorElementCount(itElementType)); ++iArgIdx)
          {
            vecCreateArgs.push_back(_ASTHelper.CreateIntegerLiteral(iArgIdx));
          }

          DUMP_INSTR(vecCurrentCreate, spInstructionSet->CreateVector(itElementType, vecCreateArgs, (i == 1)));
        }
        else if (i == 2)
        {
          DUMP_INSTR(vecCurrentCreate, spInstructionSet->CreateOnesVector(itElementType, false));
        }
        else if (i == 3)
        {
          DUMP_INSTR(vecCurrentCreate, spInstructionSet->CreateOnesVector(itElementType, true));
        }
        else if (i == 4)
        {
          DUMP_INSTR(vecCurrentCreate, spInstructionSet->CreateZeroVector(itElementType));
        }
      }

      vecCreateVector.push_back( _ASTHelper.CreateCompoundStatement(vecCurrentCreate) );
      vecCreateVector.push_back( _ASTHelper.CreateStringLiteral("") );
    }

    vecBody.push_back( _ASTHelper.CreateCompoundStatement(vecCreateVector) );
    vecBody.push_back( _ASTHelper.CreateStringLiteral("") );
  }

  // Dump extract element
  if (_uiDumpFlags & DF_Extract)
  {
    vecBody.push_back(_ASTHelper.CreateStringLiteral("ExtractElement"));

    ClangASTHelper::StatementVectorType vecExtractElement;

    for (auto itElementType : lstSupportedElementTypes)
    {
      vecExtractElement.push_back( _CreateElementTypeString(itElementType) );

      Expr *pVectorRef = _CreateArraySubscript( mapVectorArrayDecls[ itElementType ], 0 );

      for (uint32_t uiIndex = 0; uiIndex < spInstructionSet->GetVectorElementCount(itElementType); ++uiIndex)
      {
        DUMP_INSTR( vecExtractElement, spInstructionSet->ExtractElement( itElementType, pVectorRef, uiIndex ) );
      }
    }

    vecBody.push_back( _ASTHelper.CreateCompoundStatement(vecExtractElement) );
    vecBody.push_back( _ASTHelper.CreateStringLiteral("") );
  }

  // Dump insert element
  if (_uiDumpFlags & DF_Insert)
  {
    vecBody.push_back( _ASTHelper.CreateStringLiteral("InsertElement") );

    ClangASTHelper::StatementVectorType vecInsertElement;

    for (auto itElementType : lstSupportedElementTypes)
    {
      vecInsertElement.push_back( _CreateElementTypeString(itElementType) );

      Expr *pVectorRef    = _CreateArraySubscript( mapVectorArrayDecls[itElementType], 0 );
      Expr *pInsertValue  = _ASTHelper.CreateIntegerLiteral( 2 );

      for (uint32_t uiIndex = 0; uiIndex < spInstructionSet->GetVectorElementCount(itElementType); ++uiIndex)
      {
        DUMP_INSTR( vecInsertElement, spInstructionSet->InsertElement( itElementType, pVectorRef, pInsertValue, uiIndex ) );
      }
    }

    vecBody.push_back( _ASTHelper.CreateCompoundStatement(vecInsertElement) );
    vecBody.push_back( _ASTHelper.CreateStringLiteral("") );
  }

  // Dump memory transfers
  if (_uiDumpFlags & DF_MemoryTransfers)
  {
    vecBody.push_back( _ASTHelper.CreateStringLiteral("MemoryTransfers") );

    ClangASTHelper::StatementVectorType vecMemoryTransfers;

    VectorDeclRefMapType  mapPointerDecls;

    // Create pointer declarations
    vecMemoryTransfers.push_back( _ASTHelper.CreateStringLiteral("Pointer declarations") );
    for (auto itElementType : lstSupportedElementTypes)
    {
      QualType qtPointerType = _ASTHelper.GetPointerType( _GetClangType(itElementType) );

      VarDecl *pPointerDecl = _ASTHelper.CreateVariableDeclaration( pFunctionDecl, string("p") + TypeInfo::GetTypeString(itElementType), qtPointerType, nullptr );

      mapPointerDecls[ itElementType ] = _ASTHelper.CreateDeclarationReferenceExpression( pPointerDecl );

      vecMemoryTransfers.push_back( _ASTHelper.CreateDeclarationStatement(pPointerDecl) );
    }
    vecMemoryTransfers.push_back( _ASTHelper.CreateStringLiteral("") );


    const char *apcDumpName[] = { "LoadVector", "StoreVector", "StoreVectorMasked" };

    for (int i = 0; i <= 2; ++i)
    {
      vecMemoryTransfers.push_back( _ASTHelper.CreateStringLiteral(apcDumpName[i]) );

      ClangASTHelper::StatementVectorType vecCurrentTransfer;
      
      for (auto itElementType : lstSupportedElementTypes)
      {
        vecCurrentTransfer.push_back(_CreateElementTypeString(itElementType));

        Expr *pVectorRef  = _CreateArraySubscript( mapVectorArrayDecls[itElementType], 0 );
        Expr *pPointerRef = mapPointerDecls[itElementType];

        if (i == 0)
        {
          DUMP_INSTR( vecCurrentTransfer, spInstructionSet->LoadVector(itElementType, pPointerRef) );
        }
        else if (i == 1)
        {
          DUMP_INSTR( vecCurrentTransfer, spInstructionSet->StoreVector(itElementType, pPointerRef, pVectorRef) );
        }
        else
        {
          Expr *pMaskRef = _CreateArraySubscript(mapVectorArrayDecls[itElementType], 1);

          DUMP_INSTR( vecCurrentTransfer, spInstructionSet->StoreVectorMasked(itElementType, pPointerRef, pVectorRef, pMaskRef) );
        }
      }

      vecMemoryTransfers.push_back( _ASTHelper.CreateCompoundStatement(vecCurrentTransfer) );
      vecMemoryTransfers.push_back( _ASTHelper.CreateStringLiteral("") );
    }

    vecBody.push_back( _ASTHelper.CreateCompoundStatement(vecMemoryTransfers) );
    vecBody.push_back( _ASTHelper.CreateStringLiteral("") );
  }

  // Dump relational operators
  if (_uiDumpFlags & DF_Relational)
  {
    typedef Vectorization::AST::Expressions::RelationalOperator   RelationalOperator;
    typedef RelationalOperator::RelationalOperatorType            RelationalOperatorType;

    RelationalOperatorType aeRelationalOps[] =  { RelationalOperatorType::Equal,     RelationalOperatorType::Greater,   RelationalOperatorType::GreaterEqual,
                                                  RelationalOperatorType::Less,      RelationalOperatorType::LessEqual, RelationalOperatorType::LogicalAnd,
                                                  RelationalOperatorType::LogicalOr, RelationalOperatorType::NotEqual };


    vecBody.push_back( _ASTHelper.CreateStringLiteral("RelationalOperator") );

    ClangASTHelper::StatementVectorType vecRelationalOperators;

    for (auto eCurrentOp : aeRelationalOps)
    {
      vecRelationalOperators.push_back( _ASTHelper.CreateStringLiteral( RelationalOperator::GetOperatorTypeString(eCurrentOp) ) );

      ClangASTHelper::StatementVectorType vecCurrentOp;

      for (auto itElementType : lstSupportedElementTypes)
      {
        auto itArrayDecl = mapVectorArrayDecls[itElementType];

        vecCurrentOp.push_back( _CreateElementTypeString(itElementType) );
        DUMP_INSTR( vecCurrentOp, spInstructionSet->RelationalOperator( itElementType, eCurrentOp, _CreateArraySubscript(itArrayDecl, 0), _CreateArraySubscript(itArrayDecl, 1) ) );
      }

      vecRelationalOperators.push_back( _ASTHelper.CreateCompoundStatement(vecCurrentOp) );
      vecRelationalOperators.push_back( _ASTHelper.CreateStringLiteral("") );
    }

    vecBody.push_back( _ASTHelper.CreateCompoundStatement(vecRelationalOperators) );
    vecBody.push_back( _ASTHelper.CreateStringLiteral("") );
  }

  // Dump shift elements
  if (_uiDumpFlags & DF_ShiftElements)
  {
    vecBody.push_back( _ASTHelper.CreateStringLiteral("ShiftElements") );

    ClangASTHelper::StatementVectorType vecShiftElements;

    const char *apcDumpName[] = { "Shift left by zero", "Shift left by constant", "Shift right by zero", "Shift left by constant" };

    for (int i = 0; i <= 3; ++i)
    {
      vecShiftElements.push_back( _ASTHelper.CreateStringLiteral(apcDumpName[i]) );

      ClangASTHelper::StatementVectorType vecCurrentShift;
      
      for (auto itElementType : lstSupportedElementTypes)
      {
        vecCurrentShift.push_back( _CreateElementTypeString(itElementType) );

        Expr *pVectorRef  = _CreateArraySubscript( mapVectorArrayDecls[itElementType], 0 );

        DUMP_INSTR( vecCurrentShift, spInstructionSet->ShiftElements( itElementType, pVectorRef, i < 2, (i & 1) ? (i + 1) : 0 ) );
      }

      vecShiftElements.push_back( _ASTHelper.CreateCompoundStatement(vecCurrentShift) );
      vecShiftElements.push_back( _ASTHelper.CreateStringLiteral("") );
    }

    vecBody.push_back( _ASTHelper.CreateCompoundStatement(vecShiftElements) );
    vecBody.push_back( _ASTHelper.CreateStringLiteral("") );
  }

  // Dump unary operators
  if (_uiDumpFlags & DF_Unary)
  {
    typedef Vectorization::AST::Expressions::UnaryOperator    UnaryOperator;
    typedef UnaryOperator::UnaryOperatorType                  UnaryOperatorType;

    UnaryOperatorType aeUnaryOps[] =  { UnaryOperatorType::AddressOf,     UnaryOperatorType::BitwiseNot,    UnaryOperatorType::LogicalNot,
                                        UnaryOperatorType::Minus,         UnaryOperatorType::Plus,          UnaryOperatorType::PostDecrement,
                                        UnaryOperatorType::PostIncrement, UnaryOperatorType::PreDecrement,  UnaryOperatorType::PreIncrement };


    vecBody.push_back( _ASTHelper.CreateStringLiteral("UnaryOperator") );

    ClangASTHelper::StatementVectorType vecUnaryOperators;

    for (auto eCurrentOp : aeUnaryOps)
    {
      vecUnaryOperators.push_back( _ASTHelper.CreateStringLiteral( UnaryOperator::GetOperatorTypeString(eCurrentOp) ) );

      ClangASTHelper::StatementVectorType vecCurrentOp;

      for (auto itElementType : lstSupportedElementTypes)
      {
        auto itArrayDecl = mapVectorArrayDecls[itElementType];

        vecCurrentOp.push_back( _CreateElementTypeString(itElementType) );
        DUMP_INSTR( vecCurrentOp, spInstructionSet->UnaryOperator( itElementType, eCurrentOp, _CreateArraySubscript(itArrayDecl, 0) ) );
      }

      vecUnaryOperators.push_back( _ASTHelper.CreateCompoundStatement(vecCurrentOp) );
      vecUnaryOperators.push_back( _ASTHelper.CreateStringLiteral("") );
    }

    vecBody.push_back( _ASTHelper.CreateCompoundStatement(vecUnaryOperators) );
    vecBody.push_back( _ASTHelper.CreateStringLiteral("") );
  }


  pFunctionDecl->setBody( _ASTHelper.CreateCompoundStatement(vecBody) );
  return pFunctionDecl;

  #undef DUMP_INSTR
}

CPU_x86::DumpInstructionSet::DumpInstructionSet(ASTContext &rASTContext, string strDumpfile, InstructionSetEnum eIntrSet) : _ASTHelper(rASTContext), _uiDumpFlags(0)
{
  typedef pair< string, Vectorization::InstructionSetBasePtr >  InstructionSetInfoPair;
  
  list< InstructionSetInfoPair >  lstInstructionSets;

  switch (eIntrSet)
  {
  case InstructionSetEnum::SSE_4_2:   lstInstructionSets.push_front( InstructionSetInfoPair("DumpSSE_4_2",  Vectorization::InstructionSetBase::Create<Vectorization::InstructionSetSSE4_2>(rASTContext)) );
  case InstructionSetEnum::SSE_4_1:   lstInstructionSets.push_front( InstructionSetInfoPair("DumpSSE_4_1",  Vectorization::InstructionSetBase::Create<Vectorization::InstructionSetSSE4_1>(rASTContext)) );
  case InstructionSetEnum::SSSE_3:    lstInstructionSets.push_front( InstructionSetInfoPair("DumpSSSE_3",   Vectorization::InstructionSetBase::Create<Vectorization::InstructionSetSSSE3 >(rASTContext)) );
  case InstructionSetEnum::SSE_3:     lstInstructionSets.push_front( InstructionSetInfoPair("DumpSSE_3",    Vectorization::InstructionSetBase::Create<Vectorization::InstructionSetSSE3  >(rASTContext)) );
  case InstructionSetEnum::SSE_2:     lstInstructionSets.push_front( InstructionSetInfoPair("DumpSSE_2",    Vectorization::InstructionSetBase::Create<Vectorization::InstructionSetSSE2  >(rASTContext)) );
  case InstructionSetEnum::SSE:       lstInstructionSets.push_front( InstructionSetInfoPair("DumpSSE",      Vectorization::InstructionSetBase::Create<Vectorization::InstructionSetSSE   >(rASTContext)) );
  }

  // Select the requested instruction set parts
  _uiDumpFlags |= DF_Arithmetic;
  _uiDumpFlags |= DF_Blend;
  _uiDumpFlags |= DF_BroadCast;
  _uiDumpFlags |= DF_CheckActive;
  _uiDumpFlags |= DF_Convert;
  _uiDumpFlags |= DF_CreateVector;
  _uiDumpFlags |= DF_Extract;
  _uiDumpFlags |= DF_Insert;
  _uiDumpFlags |= DF_MemoryTransfers;
  _uiDumpFlags |= DF_Relational;
  _uiDumpFlags |= DF_ShiftElements;
  _uiDumpFlags |= DF_Unary;


  ClangASTHelper::FunctionDeclarationVectorType vecFunctionDecls;

  for each (auto itInstrSet in lstInstructionSets)
  {
    vecFunctionDecls.push_back( _DumpInstructionSet(itInstrSet.second, itInstrSet.first) );
  }


  if (! vecFunctionDecls.empty())
  {
    string strErrorInfo;
    llvm::raw_fd_ostream outputStream(strDumpfile.c_str(), strErrorInfo);

    for each (auto itFuncDecl in vecFunctionDecls)
    {
      itFuncDecl->print( outputStream );
      outputStream << "\n\n";
    }

    outputStream.flush();
    outputStream.close();
  }
}



// Implementation of class CPU_x86::HipaccHelper
int CPU_x86::HipaccHelper::_FindKernelParamIndex(const string &crstrParamName)
{
  for (unsigned int i = 0; i < _pKernelFunction->getNumParams(); ++i)
  {
    if (_pKernelFunction->getParamDecl(i)->getNameAsString() == crstrParamName)
    {
      return static_cast<int>( i );
    }
  }

  return -1;
}

MemoryAccess CPU_x86::HipaccHelper::GetImageAccess(const string &crstrParamName)
{
  int iParamIndex = _FindKernelParamIndex(crstrParamName);

  if (iParamIndex < 0)        // Parameter not found
  {
    return UNDEFINED;
  }
  else if (iParamIndex == 0)  // First parameter is always the output image
  {
    return WRITE_ONLY;
  }
  else                        // Parameter found
  {
    ::FieldDecl*  pFieldDescriptor = _pKernel->getDeviceArgFields()[iParamIndex];

    if (_pKernel->getImgFromMapping(pFieldDescriptor) == nullptr)   // Parameter is not an image
    {
      return UNDEFINED;
    }
    else
    {
      return _pKernel->getKernelClass()->getImgAccess(pFieldDescriptor);
    }
  }
}

HipaccAccessor* CPU_x86::HipaccHelper::GetImageFromMapping(const string &crstrParamName)
{
  int iParamIndex = _FindKernelParamIndex(crstrParamName);

  if (iParamIndex < 0)        // Parameter not found
  {
    return nullptr;
  }
  else if (iParamIndex == 0)  // First parameter is always the output image
  {
    return _pKernel->getIterationSpace()->getAccessor();
  }
  else                        // Parameter found
  {
    return _pKernel->getImgFromMapping(_pKernel->getDeviceArgFields()[iParamIndex] );
  }
}

HipaccMask* CPU_x86::HipaccHelper::GetMaskFromMapping(const string &crstrParamName)
{
  int iParamIndex = _FindKernelParamIndex(crstrParamName);

  if (iParamIndex < 0)        // Parameter not found
  {
    return nullptr;
  }
  else if (iParamIndex == 0)  // First parameter is always the output image not a mask
  {
    return nullptr;
  }
  else                        // Parameter found
  {
    return _pKernel->getMaskFromMapping(_pKernel->getDeviceArgFields()[iParamIndex]);
  }
}

::clang::DeclRefExpr* CPU_x86::HipaccHelper::GetImageParameterDecl(const std::string &crstrImageName, ImageParamType eParamType)
{
  HipaccAccessor* pAccessor = GetImageFromMapping(crstrImageName);
  if (pAccessor == nullptr)
  {
    return nullptr;
  }

  switch (eParamType)
  {
  case ImageParamType::Buffer:
    {
      int iParamIndex = _FindKernelParamIndex(crstrImageName);
      ::clang::ParmVarDecl *pParamDecl = GetKernelFunction()->getParamDecl(static_cast<unsigned int>(iParamIndex));
      return ClangASTHelper(_GetASTContext()).CreateDeclarationReferenceExpression(pParamDecl);
    }
  case ImageParamType::Width:   return pAccessor->getWidthDecl();
  case ImageParamType::Height:  return pAccessor->getHeightDecl();
  case ImageParamType::Stride:  return pAccessor->getStrideDecl();
  default:                      throw RuntimeErrorException("Unknwon image parameter type!");
  }
}

::clang::Expr* CPU_x86::HipaccHelper::GetIterationSpaceLimitX()
{
  ::clang::Expr *pUpperX = _pKernel->getIterationSpace()->getAccessor()->getWidthDecl();

  if (::clang::DeclRefExpr *pOffsetX = _pKernel->getIterationSpace()->getAccessor()->getOffsetXDecl())
  {
    pUpperX = ASTNode::createBinaryOperator(_GetASTContext(), pUpperX, pOffsetX, BO_Add, _GetASTContext().IntTy);
  }

  return pUpperX;
}

::clang::Expr* CPU_x86::HipaccHelper::GetIterationSpaceLimitY()
{
  ::clang::Expr *pUpperY = _pKernel->getIterationSpace()->getAccessor()->getHeightDecl();

  if (::clang::DeclRefExpr *pOffsetY = _pKernel->getIterationSpace()->getAccessor()->getOffsetYDecl())
  {
    pUpperY = ASTNode::createBinaryOperator(_GetASTContext(), pUpperY, pOffsetY, BO_Add, _GetASTContext().IntTy);
  }

  return pUpperY;
}



// Implementation of class CPU_x86::CodeGenerator::Descriptor
CPU_x86::CodeGenerator::Descriptor::Descriptor()
{
  SetTargetCode(::clang::hipacc::TARGET_C);
  SetName("CPU-x86");
  SetEmissionKey("cpu");
  SetDescription("Emit C++ code for x86-CPUs");
}


// Implementation of class CPU_x86::CodeGenerator::KernelSubFunctionBuilder
void CPU_x86::CodeGenerator::KernelSubFunctionBuilder::AddCallParameter(::clang::DeclRefExpr *pCallParam, bool bForceConstDecl)
{
  ::clang::ValueDecl  *pValueDecl = pCallParam->getDecl();
  ::clang::QualType   qtParamType = pValueDecl->getType();

  if (bForceConstDecl)
  {
    qtParamType.addConst();
  }

  _vecArgumentTypes.push_back(qtParamType);
  _vecArgumentNames.push_back(pValueDecl->getNameAsString());
  _vecCallParams.push_back(pCallParam);
}

CPU_x86::CodeGenerator::KernelSubFunctionBuilder::DeclCallPairType  CPU_x86::CodeGenerator::KernelSubFunctionBuilder::CreateFuntionDeclarationAndCall(string strFunctionName, const ::clang::QualType &crResultType)
{
  DeclCallPairType pairDeclAndCall;

  pairDeclAndCall.first  = _ASTHelper.CreateFunctionDeclaration( strFunctionName, crResultType, _vecArgumentNames, _vecArgumentTypes );
  pairDeclAndCall.second = _ASTHelper.CreateFunctionCall( pairDeclAndCall.first, _vecCallParams );

  return pairDeclAndCall;
}

void CPU_x86::CodeGenerator::KernelSubFunctionBuilder::ImportUsedParameters(::clang::FunctionDecl *pRootFunctionDecl, ::clang::Stmt *pSubFunctionBody)
{
  for (unsigned int i = 0; i < pRootFunctionDecl->getNumParams(); ++i)
  {
    ParmVarDecl *pParamVarDecl = pRootFunctionDecl->getParamDecl(i);

    if (IsVariableUsed(pParamVarDecl->getNameAsString(), pSubFunctionBody))
    {
      AddCallParameter( _ASTHelper.CreateDeclarationReferenceExpression(pParamVarDecl) );
    }
  }
}

bool CPU_x86::CodeGenerator::KernelSubFunctionBuilder::IsVariableUsed(const string &crstrVariableName, ::clang::Stmt *pStatement)
{
  return (ClangASTHelper::CountNumberOfReferences(pStatement, crstrVariableName) > 0);
}


// Implementation of class CPU_x86::CodeGenerator::ImageAccessTranslator
CPU_x86::CodeGenerator::ImageAccessTranslator::ImageAccessTranslator(HipaccHelper &rHipaccHelper) : _rHipaccHelper(rHipaccHelper), _ASTHelper(_rHipaccHelper.GetKernelFunction()->getASTContext())
{
  ::clang::FunctionDecl *pKernelFunction = rHipaccHelper.GetKernelFunction();

  _pDRGidX = _ASTHelper.FindDeclaration(pKernelFunction, _rHipaccHelper.GlobalIdX());
  _pDRGidY = _ASTHelper.FindDeclaration(pKernelFunction, _rHipaccHelper.GlobalIdY());
}

list< ::clang::ArraySubscriptExpr* > CPU_x86::CodeGenerator::ImageAccessTranslator::_FindImageAccesses(const string &crstrImageName, ::clang::Stmt *pStatement)
{
  list< ::clang::ArraySubscriptExpr* > lstImageAccesses;

  if (pStatement == nullptr)
  {
    return lstImageAccesses;
  }
  else if (isa<::clang::ArraySubscriptExpr>(pStatement))
  {
    // Found an array subscript expression => Check if the structure corresponds to an image access
    ::clang::ArraySubscriptExpr *pRootArraySubscript = dyn_cast<::clang::ArraySubscriptExpr>(pStatement);

    // Look through implicit cast expressions
    ::clang::Expr *pLhs = pRootArraySubscript->getLHS();
    while (isa<::clang::ImplicitCastExpr>(pLhs))
    {
      pLhs = dyn_cast<::clang::ImplicitCastExpr>(pLhs)->getSubExpr();
    }

    if (isa<::clang::ArraySubscriptExpr>(pLhs))
    {
      // At least 2-dimensional array found => Look for the declaration reference to the image
      ::clang::ArraySubscriptExpr *pChildArraySubscript = dyn_cast<::clang::ArraySubscriptExpr>(pLhs);

      // Look through implicit cast expressions
      pLhs = pChildArraySubscript->getLHS();
      while (isa<::clang::ImplicitCastExpr>(pLhs))
      {
        pLhs = dyn_cast<::clang::ImplicitCastExpr>(pLhs)->getSubExpr();
      }

      if (isa<::clang::DeclRefExpr>(pLhs))
      {
        // Found a 2-dimensional array access => check if the array if the specified image
        ::clang::DeclRefExpr* pArrayDeclRef = dyn_cast<::clang::DeclRefExpr>(pLhs);

        if (pArrayDeclRef->getNameInfo().getAsString() == crstrImageName)
        {
          lstImageAccesses.push_back(pRootArraySubscript);
        }
      }
    }
  }

  // Parse all children everytime (in case an image access is used as an index expression for another image access)
  for (auto itChild = pStatement->child_begin(); itChild != pStatement->child_end(); itChild++)
  {
    list< ::clang::ArraySubscriptExpr* > lstImageAccessesInternal = _FindImageAccesses(crstrImageName, *itChild);

    if (!lstImageAccessesInternal.empty())
    {
      lstImageAccesses.insert(lstImageAccesses.end(), lstImageAccessesInternal.begin(), lstImageAccessesInternal.end());
    }
  }

  return lstImageAccesses;
}

void CPU_x86::CodeGenerator::ImageAccessTranslator::_LinearizeImageAccess(const string &crstrImageName, ::clang::ArraySubscriptExpr *pImageAccessRoot)
{
  // Find the horizontal and vertical index expression of the 2-dimensional image access
  ::clang::Expr *pIndexExprX = pImageAccessRoot->getRHS();
  ::clang::Expr *pIndexExprY = pImageAccessRoot->getLHS();
  {
    while (!isa<::clang::ArraySubscriptExpr>(pIndexExprY))
    {
      pIndexExprY = dyn_cast<::clang::Expr>(*pIndexExprY->child_begin());
    }

    pIndexExprY = dyn_cast<::clang::ArraySubscriptExpr>(pIndexExprY)->getRHS();
  }

  // The image pointer have been re-routed to the current pixel => strip the reference to the global pixel ID
  pIndexExprX = _SubtractReference(pIndexExprX, _pDRGidX);
  pIndexExprY = _SubtractReference(pIndexExprY, _pDRGidY);


  // Build the final 1-dimensional index expression
  ::clang::Expr *pFinalIndexExpression = nullptr;
  if (pIndexExprY == nullptr)     // No vertical index expression required
  {
    if (pIndexExprX == nullptr)     // Neither horizontal nor vertical index required => access to the current pixel
    {
      pFinalIndexExpression = _ASTHelper.CreateIntegerLiteral(0);
    }
    else                            // Only horizontal index required => Access inside the current row
    {
      pFinalIndexExpression = pIndexExprX;
    }
  }
  else                            // Vertical index required
  {
    // Account for the row offset necessary for linear memory indexing
    ::clang::DeclRefExpr *pDRImageStride = _rHipaccHelper.GetImageParameterDecl(crstrImageName, HipaccHelper::ImageParamType::Stride);
    _rHipaccHelper.MarkParamUsed(pDRImageStride->getNameInfo().getAsString());

    pIndexExprY = _ASTHelper.CreateBinaryOperator(pIndexExprY, pDRImageStride, BO_Mul, pIndexExprY->getType());


    if (pIndexExprX == nullptr)     // Only vertical index required => Access inside the current column
    {
      pFinalIndexExpression = pIndexExprY;
    }
    else                            // Both horizontal and vertical index required => Add up both index expressions
    {
      pFinalIndexExpression = _ASTHelper.CreateBinaryOperator(pIndexExprY, pIndexExprX, BO_Add, pIndexExprY->getType());
    }
  }

  // Create the final 1-dimensional array subscript expression
  pImageAccessRoot->setLHS( _rHipaccHelper.GetImageParameterDecl(crstrImageName, HipaccHelper::ImageParamType::Buffer) );
  pImageAccessRoot->setRHS( pFinalIndexExpression );
}

::clang::Expr* CPU_x86::CodeGenerator::ImageAccessTranslator::_SubtractReference(::clang::Expr *pExpression, ::clang::DeclRefExpr *pDRSubtrahend)
{
  string        strStripVarName   = pDRSubtrahend->getNameInfo().getAsString();
  unsigned int  uiReferenceCount  = _ASTHelper.CountNumberOfReferences(pExpression, strStripVarName);

  ::clang::Expr *pReturnExpr = nullptr;

  if (uiReferenceCount == 1)    // One reference to stripping variable found => Try to remove it completely
  {
    if (_ASTHelper.IsSingleBranchStatement(pExpression))        // The expression refers only to the stripping variable => Entire expression is obsolete
    {
      return nullptr;
    }
    else if (_TryRemoveReference(pExpression, strStripVarName)) // Try to remove the stripping variable from the expression
    {
      pReturnExpr = pExpression;
    }
    else                                                        // The stripping variable could not be removed => subtract it to ensure the correct result
    {
      pReturnExpr = _ASTHelper.CreateBinaryOperator(pExpression, pDRSubtrahend, ::clang::BO_Sub, pExpression->getType());
    }
  }
  else                          // Either none or more than one reference found => subtract the stripping variable to ensure the correct result
  {
    pReturnExpr = _ASTHelper.CreateBinaryOperator(pExpression, pDRSubtrahend, ::clang::BO_Sub, pExpression->getType());
  }

  return _ASTHelper.CreateParenthesisExpression(pReturnExpr);
}

bool CPU_x86::CodeGenerator::ImageAccessTranslator::_TryRemoveReference(::clang::Expr *pExpression, string strStripVarName)
{
  // Try to find the bottom-most operator referencing the stripping variable
  ::clang::Expr           *pCurrentExpression = pExpression;
  ::clang::BinaryOperator *pBottomOperator    = nullptr;

  while (true)
  {
    if (isa<::clang::CastExpr>(pCurrentExpression))             // Current node is a cast expression => Step to the subexpression
    {
      pCurrentExpression = dyn_cast<::clang::CastExpr>(pCurrentExpression)->getSubExpr();
    }
    else if (isa<::clang::ParenExpr>(pCurrentExpression))       // Current node is a parenthesis expression => Step to the subexpression
    {
      pCurrentExpression = dyn_cast<::clang::ParenExpr>(pCurrentExpression)->getSubExpr();
    }
    else if (isa<::clang::DeclRefExpr>(pCurrentExpression))     // Current node is a declaration reference => Check for the stripping variable
    {
      if (dyn_cast<::clang::DeclRefExpr>(pCurrentExpression)->getNameInfo().getAsString() == strStripVarName)
      {
        // Found the reference to the stripping variable => Break here and remove it
        break;
      }
      else  // Found a wrong declaration reference (something went wrong) => Stripping failed
      {
        return false;
      }
    }
    else if (isa<::clang::BinaryOperator>(pCurrentExpression))  // Found a binary operator => Step into its correct branch
    {
      ::clang::BinaryOperator *pCurrentOperator = dyn_cast<::clang::BinaryOperator>(pCurrentExpression);

      if (pCurrentOperator->getOpcode() == ::clang::BO_Add)       // Found an addition => Both branches are supported
      {
        if (_ASTHelper.CountNumberOfReferences(pCurrentOperator->getLHS(), strStripVarName) == 1)
        {
          // Found the reference in the left-hand-branch => Step into it
          pCurrentExpression = pCurrentOperator->getLHS();
        }
        else if (_ASTHelper.CountNumberOfReferences(pCurrentOperator->getRHS(), strStripVarName) == 1)
        {
          // Found the reference in the right-hand-branch => Step into it
          pCurrentExpression = pCurrentOperator->getRHS();
        }
        else  // Something went wrong => Stripping failed
        {
          return false;
        }
      }
      else if (pCurrentOperator->getOpcode() == ::clang::BO_Sub)  // Found a substraction => Only the left-hand-branch is supported for the stripping
      {
        if (_ASTHelper.CountNumberOfReferences(pCurrentOperator->getLHS(), strStripVarName) == 1)
        {
          // Found the reference in the left-hand-branch => Step into it
          pCurrentExpression = pCurrentOperator->getLHS();
        }
        else  // Reference is not in the left-hand-branch => Stripping failed
        {
          return false;
        }
      }
      else  // The type of the binary operator is not supported => Stripping failed
      {
        return false;
      }

      // Set the current operator as the bottom-most operator
      pBottomOperator = pCurrentOperator;
    }
    else                                                        // Found an unsupported expression => Stripping failed
    {
      return false;
    }
  }


  if (pBottomOperator != nullptr)   // Found the bottom-most binary operator referencing the stripping variable
  {
    // Replace the operator branch containing the stripping variable with zero (note: this operator could be reduced, but then its parent would need to be changed)
    ::clang::Expr *pZeroLiteral = _ASTHelper.CreateIntegerLiteral(0);

    if (_ASTHelper.CountNumberOfReferences(pBottomOperator->getLHS(), strStripVarName) == 1)
    {
      pBottomOperator->setLHS(pZeroLiteral);
    }
    else if (_ASTHelper.CountNumberOfReferences(pBottomOperator->getRHS(), strStripVarName) == 1)
    {
      pBottomOperator->setRHS(pZeroLiteral);
    }

    return true;
  }
  else                              // Could not find the bottom-most reference => Stripping failed
  {
    return false;
  }
}

CPU_x86::CodeGenerator::ImageAccessTranslator::ImageLinePosDeclPairType CPU_x86::CodeGenerator::ImageAccessTranslator::CreateImageLineAndPosDecl(std::string strImageName)
{
  ImageLinePosDeclPairType LinePosDeclPair;

  ::clang::DeclRefExpr  *pImageDeclRef    = _rHipaccHelper.GetImageParameterDecl(strImageName, HipaccHelper::ImageParamType::Buffer);
  ::clang::FunctionDecl *pKernelFunction  = _rHipaccHelper.GetKernelFunction();

  // Fetch the required image access and pointer types
  QualType qtArrayAccessType, qtImagePointerType;
  {
    qtArrayAccessType = pImageDeclRef->getType()->getPointeeType();
    QualType qtElementType = qtArrayAccessType->getAsArrayTypeUnsafe()->getElementType();

    if (_rHipaccHelper.GetImageAccess(strImageName) == READ_ONLY)
    {
      qtElementType.addConst();
    }

    qtImagePointerType = _ASTHelper.GetPointerType(qtElementType);
  }


  // Create the declaration of the currently processed image line, e.g. "float *Output_CurrentLine = Output[gid_y];"
  {
    ::clang::ArraySubscriptExpr *pArrayAccess   = _ASTHelper.CreateArraySubscriptExpression(pImageDeclRef, _pDRGidY, qtArrayAccessType, false);
    ::clang::ImplicitCastExpr   *pCastToPointer = _ASTHelper.CreateImplicitCastExpression(pArrayAccess, qtImagePointerType, CK_ArrayToPointerDecay, false);
    LinePosDeclPair.first                       = _ASTHelper.CreateVariableDeclaration(pKernelFunction, strImageName + string("_CurrentLine"), qtImagePointerType, pCastToPointer);
  }

  ::clang::DeclRefExpr *pImageLineDeclRef = _ASTHelper.CreateDeclarationReferenceExpression(LinePosDeclPair.first);


  // Create the declaration of the currently processed image pixel, e.g. "float *Output_CurrentPos = Output_CurrentLine + gid_x;"
  ::clang::BinaryOperator *pPointerAddition = _ASTHelper.CreateBinaryOperator(pImageLineDeclRef, _pDRGidX, BO_Add, qtImagePointerType);
  LinePosDeclPair.second                    = _ASTHelper.CreateVariableDeclaration(pKernelFunction, strImageName + string("_CurrentPos"), qtImagePointerType, pPointerAddition);

  return LinePosDeclPair;
}

void CPU_x86::CodeGenerator::ImageAccessTranslator::TranslateImageAccesses(::clang::Stmt *pStatement)
{
  ::clang::FunctionDecl *pKernelFunction = _rHipaccHelper.GetKernelFunction();

  // Parse through all kernel images
  for (unsigned int i = 0; i < pKernelFunction->getNumParams(); ++i)
  {
    ::clang::ParmVarDecl  *pParamDecl   = pKernelFunction->getParamDecl(i);
    string                strParamName  = pParamDecl->getNameAsString();

    // Skip all kernel function parameters, which are unused or do not refer to an HIPAcc image
    if ((!_rHipaccHelper.IsParamUsed(strParamName)) || (_rHipaccHelper.GetImageFromMapping(strParamName) == nullptr))
    {
      continue;
    }

    // Find all access to the current image
    list< ::clang::ArraySubscriptExpr* >  lstImageAccesses = _FindImageAccesses(strParamName, pStatement);

    // Linearize all found image accesses
    for each (auto itImageAccess in lstImageAccesses)
    {
      _LinearizeImageAccess(strParamName, itImageAccess);
    }
  }
}

void CPU_x86::CodeGenerator::ImageAccessTranslator::TranslateImageDeclarations(::clang::FunctionDecl *pFunctionDecl, ImageDeclarationTypes eDeclType)
{
  // Parse through all kernel images
  for (unsigned int i = 0; i < pFunctionDecl->getNumParams(); ++i)
  {
    ::clang::ParmVarDecl  *pParamDecl = pFunctionDecl->getParamDecl(i);
    string                strParamName = pParamDecl->getNameAsString();

    // Skip all kernel function parameters, which are unused or do not refer to an HIPAcc image
    if ((!_rHipaccHelper.IsParamUsed(strParamName)) || (_rHipaccHelper.GetImageFromMapping(strParamName) == nullptr))
    {
      continue;
    }

    ::clang::DeclRefExpr  *pImageDeclRef = _rHipaccHelper.GetImageParameterDecl(strParamName, HipaccHelper::ImageParamType::Buffer);

    // Get the actual pixel type
    QualType qtElementType;
    {
      QualType qtArrayAccessType = pImageDeclRef->getType()->getPointeeType();
      qtElementType = qtArrayAccessType->getAsArrayTypeUnsafe()->getElementType();

      if (_rHipaccHelper.GetImageAccess(strParamName) == READ_ONLY)
      {
        qtElementType.addConst();
      }
    }

    // Build the desired new declaration type
    QualType qtFinalImageType;
    switch (eDeclType)
    {
    case ImageDeclarationTypes::NativePointer:
      {
        qtFinalImageType = _ASTHelper.GetPointerType(qtElementType);
      }
      break;
    case ImageDeclarationTypes::ConstantArray:
      {
        HipaccImage *pImage = _rHipaccHelper.GetImageFromMapping(strParamName)->getImage();

        QualType qtArrayType  = _ASTHelper.GetConstantArrayType( qtElementType, static_cast< size_t >(pImage->getSizeX()) );
        qtFinalImageType      = _ASTHelper.GetConstantArrayType( qtArrayType,   static_cast< size_t >(pImage->getSizeY()) );
      }
      break;
    default:    throw InternalErrorException("Unknown image declaration type");
    }

    // Replace the delcaration type of the image
    pParamDecl->setType(qtFinalImageType);
  }
}



// Implementation of class CPU_x86::CodeGenerator
CPU_x86::CodeGenerator::CodeGenerator(::clang::hipacc::CompilerOptions *pCompilerOptions) : BaseType(pCompilerOptions, Descriptor())
{
  _InitSwitch< KnownSwitches::InstructionSet    >( CompilerSwitchTypeEnum::InstructionSet );
  _InitSwitch< KnownSwitches::UnrollVectorLoops >( CompilerSwitchTypeEnum::UnrollVectorLoops );
  _InitSwitch< KnownSwitches::VectorizeKernel   >( CompilerSwitchTypeEnum::VectorizeKernel );
  _InitSwitch< KnownSwitches::VectorWidth       >( CompilerSwitchTypeEnum::VectorWidth );

  _eInstructionSet    = InstructionSetEnum::Array;
  _bUnrollVectorLoops = true;
  _bVectorizeKernel   = false;
  _szVectorWidth      = static_cast< size_t >(0);
}

size_t CPU_x86::CodeGenerator::_HandleSwitch(CompilerSwitchTypeEnum eSwitch, CommonDefines::ArgumentVectorType &rvecArguments, size_t szCurrentIndex)
{
  string  strCurrentSwitch  = rvecArguments[szCurrentIndex];
  size_t  szReturnIndex     = szCurrentIndex;

  switch (eSwitch)
  {
  case CompilerSwitchTypeEnum::InstructionSet:
    _eInstructionSet = _ParseOption< KnownSwitches::InstructionSet >(rvecArguments, szCurrentIndex);
    ++szReturnIndex;
    break;
  case CompilerSwitchTypeEnum::UnrollVectorLoops:
    {
      ::clang::hipacc::CompilerOption eOption = _ParseOption< KnownSwitches::UnrollVectorLoops >(rvecArguments, szCurrentIndex);

      _bUnrollVectorLoops = (eOption == USER_ON);

      ++szReturnIndex;
    }
    break;
  case CompilerSwitchTypeEnum::VectorizeKernel:
    _bVectorizeKernel = true;
    break;
  case CompilerSwitchTypeEnum::VectorWidth:
    {
      int iRequestedVecWidth = _ParseOption< KnownSwitches::VectorWidth >(rvecArguments, szCurrentIndex);
      if (iRequestedVecWidth <= 0)
      {
        throw RuntimeErrors::InvalidOptionException(KnownSwitches::VectorWidth::Key(), rvecArguments[szCurrentIndex + 1]);
      }

      _szVectorWidth = static_cast< size_t >( iRequestedVecWidth );
      ++szReturnIndex;
    }
    break;
  default:  throw InternalErrors::UnhandledSwitchException(strCurrentSwitch, GetName());
  }

  return szReturnIndex;
}


::clang::ForStmt* CPU_x86::CodeGenerator::_CreateIterationSpaceLoop(ClangASTHelper &rAstHelper, ::clang::DeclRefExpr *pLoopCounter, ::clang::Expr *pUpperLimit, ::clang::Stmt *pLoopBody)
{
  ::clang::Stmt* pFinalLoopBody = pLoopBody;
  if (! isa<::clang::CompoundStmt>(pFinalLoopBody))
  {
    pFinalLoopBody = rAstHelper.CreateCompoundStatement(pLoopBody);
  }

  ::clang::DeclStmt   *pInitStatement = rAstHelper.CreateDeclarationStatement(pLoopCounter);
  ::clang::Expr       *pCondition     = rAstHelper.CreateBinaryOperatorLessThan(pLoopCounter, pUpperLimit);
  ::clang::Expr       *pIncrement     = rAstHelper.CreatePostIncrementOperator(pLoopCounter);

  return ASTNode::createForStmt(rAstHelper.GetASTContext(), pInitStatement, pCondition, pIncrement, pFinalLoopBody);
}


string CPU_x86::CodeGenerator::_GetImageDeclarationString(string strName, HipaccMemory *pHipaccMemoryObject, bool bConstPointer)
{
  stringstream FormatStream;

  if (bConstPointer)
  {
    FormatStream << "const ";
  }

  FormatStream << pHipaccMemoryObject->getTypeStr() << " " << strName;
  FormatStream << "[" << pHipaccMemoryObject->getSizeYStr() << "]";
  FormatStream << "[" << pHipaccMemoryObject->getSizeXStr() << "]";

  return FormatStream.str();
}


string CPU_x86::CodeGenerator::_FormatFunctionHeader(FunctionDecl *pFunctionDecl, HipaccHelper &rHipaccHelper, bool bCheckUsage, bool bPrintActualImageType)
{
  vector< string > vecParamStrings;

  // Translate function parameters to declaration strings
  for (size_t i = 0; i < pFunctionDecl->getNumParams(); ++i)
  {
    ::clang::ParmVarDecl  *pParamDecl = pFunctionDecl->getParamDecl(i);
    std::string strName(pParamDecl->getNameAsString());

    if ( bCheckUsage && (!rHipaccHelper.IsParamUsed(strName)) )
    {
      continue;
    }

    // Translate argument, dependent on its type
    if (HipaccMask *pMask = rHipaccHelper.GetMaskFromMapping(strName))                // check if we have a Mask or Domain
    {
      if (!pMask->isConstant())
      {
        vecParamStrings.push_back( _GetImageDeclarationString(pMask->getName(), pMask, true) );
      }
    }
    else if (HipaccAccessor *pAccessor = rHipaccHelper.GetImageFromMapping(strName))  // check if we have an Accessor
    {
      if (bPrintActualImageType)
      {
        pParamDecl->getType().getAsStringInternal(strName, GetPrintingPolicy());
        vecParamStrings.push_back( strName );
      }
      else
      {
        vecParamStrings.push_back( _GetImageDeclarationString(strName, pAccessor->getImage(), rHipaccHelper.GetImageAccess(strName) == READ_ONLY) );
      }
    }
    else                                                                              // normal arguments
    {
      string strParamBuffer;
      llvm::raw_string_ostream ParamStream(strParamBuffer);

      pParamDecl->getType().getAsStringInternal(strName, GetPrintingPolicy());
      ParamStream << strName;

      // default arguments ...
      if (Expr *Init = pParamDecl->getInit())
      {
        CXXConstructExpr *CCE = dyn_cast<CXXConstructExpr>(Init);

        if (!CCE || CCE->getConstructor()->isCopyConstructor())
        {
          ParamStream << " = ";
        }

        Init->printPretty(ParamStream, 0, GetPrintingPolicy(), 0);
      }

      vecParamStrings.push_back( ParamStream.str() );
    }
  }


  stringstream OutputStream;

  // Write function name and qualifiers
  OutputStream << pFunctionDecl->getResultType().getAsString(GetPrintingPolicy()) << " " << pFunctionDecl->getNameAsString() << "(";

  // Write all parameters with comma delimiters
  if (! vecParamStrings.empty())
  {
    OutputStream << vecParamStrings[0];

    for (size_t i = static_cast<size_t>(1); i < vecParamStrings.size(); ++i)
    {
      OutputStream << ", " << vecParamStrings[i];
    }
  }

  OutputStream << ") ";

  return OutputStream.str();
}

string CPU_x86::CodeGenerator::_GetInstructionSetIncludeFile()
{
  switch (_eInstructionSet)
  {
  case InstructionSetEnum::SSE:       return "xmmintrin.h";
  case InstructionSetEnum::SSE_2:     return "emmintrin.h";
  case InstructionSetEnum::SSE_3:     return "pmmintrin.h";
  case InstructionSetEnum::SSSE_3:    return "tmmintrin.h";
  case InstructionSetEnum::SSE_4_1:   return "smmintrin.h";
  case InstructionSetEnum::SSE_4_2:   return "nmmintrin.h";
  default:  return "";
  }
}

size_t CPU_x86::CodeGenerator::_GetVectorWidth(Vectorization::AST::FunctionDeclarationPtr spVecFunction)
{
  if (_eInstructionSet == InstructionSetEnum::Array)
  {
    if (_szVectorWidth == static_cast<size_t>(0))
    {
      llvm::errs() << "\nNOTE: No vector width for array export selected => Set default value \"4\"\n\n";
      return static_cast< size_t >( 4 );
    }
    else
    {
      return _szVectorWidth;
    }
  }
  else
  {
    const size_t cszMaxVecWidth = static_cast< size_t >( 16 );

    if (_szVectorWidth > cszMaxVecWidth)
    {
      llvm::errs() << "\nWARNING: Selected vector width exceeds the maximum width for the SSE instruction set => Clipping vector width to \"" << cszMaxVecWidth << "\"\n\n";
      return cszMaxVecWidth;
    }

    // Compute the minimum width dependend on the size of the image element types
    size_t szMinVecWidth = static_cast<size_t>(1);
    {
      size_t szMinTypeSize = cszMaxVecWidth;

      for (Vectorization::AST::IndexType iParamIdx = static_cast<Vectorization::AST::IndexType>(0); iParamIdx < spVecFunction->GetParameterCount(); ++iParamIdx)
      {
        Vectorization::AST::BaseClasses::VariableInfoPtr spParamInfo = spVecFunction->GetParameter(iParamIdx)->LookupVariableInfo();

        if (spParamInfo->GetVectorize() && spParamInfo->GetTypeInfo().IsDereferencable())
        {
          size_t szCurrentTypeSize  = Vectorization::AST::BaseClasses::TypeInfo::GetTypeSize( spParamInfo->GetTypeInfo().GetType() );
          szMinTypeSize             = std::min( szMinTypeSize, szCurrentTypeSize );
        }
      }

      szMinVecWidth = cszMaxVecWidth / szMinTypeSize;
    }

    if (_szVectorWidth == static_cast<size_t>(0))
    {
      llvm::errs() << "\nNOTE: No vector width for SSE instruction set selected => Set kernel-based minimum value \"" << szMinVecWidth << "\"\n\n";
      return szMinVecWidth;
    }
    else if (_szVectorWidth < szMinVecWidth)
    {
      llvm::errs() << "\nWARNING: Selected vector width is below the minimum width for the SSE instruction set => Set kernel-based minimum value \"" << szMinVecWidth << "\"\n\n";
      return szMinVecWidth;
    }
    else
    {
      for (size_t szCurWidth = szMinVecWidth; szCurWidth <= cszMaxVecWidth; szCurWidth <<= 1)
      {
        if (_szVectorWidth == szCurWidth)
        {
          break;
        }
        else if (_szVectorWidth < szCurWidth)
        {
          llvm::errs() << "\nWARNING: The selected vector width for the SSE instruction set must be a power of 2 => Promote width \"" << _szVectorWidth << "\" to \"" << szCurWidth << "\"\n\n";
          _szVectorWidth = szCurWidth;
          break;
        }
      }

      return _szVectorWidth;
    }
  }
}

::clang::FunctionDecl* CPU_x86::CodeGenerator::_VectorizeKernelSubFunction(FunctionDecl *pSubFunction, HipaccHelper &rHipaccHelper)
{
  try
  {
    Vectorization::Vectorizer Vectorizer;

    Vectorization::AST::FunctionDeclarationPtr spVecFunction = Vectorizer.ConvertClangFunctionDecl(pSubFunction);

    spVecFunction->SetName( rHipaccHelper.GetKernelFunction()->getNameAsString() + string("_Vectorized") );

    Vectorizer.DumpVASTNodeToXML(spVecFunction, "Dump_1.xml");


    Vectorizer.RemoveUnnecessaryConversions(spVecFunction);
    Vectorizer.DumpVASTNodeToXML(spVecFunction, "Dump_2.xml");


    Vectorizer.FlattenScopeTrees(spVecFunction);
    Vectorizer.DumpVASTNodeToXML(spVecFunction, "Dump_3.xml");


    // Vectorize the kernel sub-function
    {
      // Mark all HIPAcc images as vectorized variables
      for (unsigned int uiParam = 0; uiParam < pSubFunction->getNumParams(); ++uiParam)
      {
        string strParamName = pSubFunction->getParamDecl(uiParam)->getNameAsString();

        if (rHipaccHelper.GetImageFromMapping(strParamName) != nullptr)
        {
          Vectorization::AST::BaseClasses::VariableInfoPtr spVariableInfo = spVecFunction->GetVariableInfo(strParamName);
          if (! spVariableInfo)
          {
            throw InternalErrorException(string("Missing vectorization parameter: ") + strParamName);
          }

          spVariableInfo->SetVectorize(true);
        }
      }

      // Mark the horizontal global ID as vectorized variable if it used by the kernel sub-function
      Vectorization::AST::BaseClasses::VariableInfoPtr spGidXInfo = spVecFunction->GetVariableInfo(rHipaccHelper.GlobalIdX());
      if (spGidXInfo)
      {
        spGidXInfo->SetVectorize(true);
      }
    }

    Vectorizer.VectorizeFunction(spVecFunction);
    Vectorizer.DumpVASTNodeToXML(spVecFunction, "Dump_4.xml");


    Vectorizer.RebuildControlFlow(spVecFunction);
    Vectorizer.DumpVASTNodeToXML(spVecFunction, "Dump_5.xml");


    Vectorizer.FlattenMemoryAccesses(spVecFunction);
    Vectorizer.DumpVASTNodeToXML(spVecFunction, "Dump_6.xml");


    // Convert vectorized function parameters
    {
      Vectorization::AST::ScopePtr spVecFunctionBody = spVecFunction->GetBody();

      for (Vectorization::AST::IndexType iParamIdx = static_cast<Vectorization::AST::IndexType>(0); iParamIdx < spVecFunction->GetParameterCount(); ++iParamIdx)
      {
        Vectorization::AST::Expressions::IdentifierPtr    spParam     = spVecFunction->GetParameter( iParamIdx );
        Vectorization::AST::BaseClasses::VariableInfoPtr  spParamInfo = spParam->LookupVariableInfo();

        // Each vectorized single value parameter will be converted into a scalar parameter and a new vectorized variable will be created
        if (spParamInfo->GetVectorize() && spParamInfo->GetTypeInfo().IsSingleValue())
        {
          string strParamName = spParam->GetName();

          // Replace parameter
          Vectorization::AST::BaseClasses::VariableInfoPtr spNewParamInfo = Vectorization::AST::BaseClasses::VariableInfo::Create( spParamInfo->GetName() + string("_base"), spParamInfo->GetTypeInfo(), false );
          Vectorization::AST::Expressions::IdentifierPtr   spNewParam     = Vectorization::AST::Expressions::Identifier::Create( spNewParamInfo->GetName() );

          spVecFunction->SetParameter(iParamIdx, spNewParamInfo);


          // Create the assignment expression for the new variable
          Vectorization::AST::Expressions::AssignmentOperatorPtr  spAssignment    = Vectorization::AST::Expressions::AssignmentOperator::Create( spParam );
          Vectorization::AST::VectorSupport::BroadCastPtr         spBaseBroadCast = Vectorization::AST::VectorSupport::BroadCast::Create( spNewParam );

          if (strParamName == HipaccHelper::GlobalIdX())
          {
            typedef Vectorization::AST::Expressions::ArithmeticOperator::ArithmeticOperatorType   OperatorType;

            // The horizontal global id must be incremental vector
            Vectorization::AST::VectorSupport::VectorIndexPtr       spVectorIndex = Vectorization::AST::VectorSupport::VectorIndex::Create( spParamInfo->GetTypeInfo().GetType() );
            Vectorization::AST::Expressions::ArithmeticOperatorPtr  spAddOperator = Vectorization::AST::Expressions::ArithmeticOperator::Create( OperatorType::Add, spBaseBroadCast, spVectorIndex );

            spAssignment->SetRHS(spAddOperator);
          }
          else
          {
            // Every other variable will be initialized with the base value
            spAssignment->SetRHS(spBaseBroadCast);
          }

          // Add the declaration and the assignment statement
          spVecFunctionBody->AddVariableDeclaration(spParamInfo);
          spVecFunctionBody->InsertChild(0, spAssignment);
        }
      }
    }
    Vectorizer.DumpVASTNodeToXML(spVecFunction, "Dump_7.xml");

    Vectorizer.RebuildDataFlow(spVecFunction, _eInstructionSet != InstructionSetEnum::Array);
    Vectorizer.DumpVASTNodeToXML(spVecFunction, "Dump_8.xml");


    DumpInstructionSet(rHipaccHelper.GetKernelFunction()->getASTContext(), "Dump_IS.cpp", _eInstructionSet);

    return Vectorizer.ConvertVASTFunctionDecl(spVecFunction, _GetVectorWidth(spVecFunction), pSubFunction->getASTContext(), _bUnrollVectorLoops);
  }
  catch (std::exception &e)
  {
    llvm::errs() << "\n\nERROR: " << e.what() << "\n\n";
    exit(EXIT_FAILURE);
  }
}


CommonDefines::ArgumentVectorType CPU_x86::CodeGenerator::GetAdditionalClangArguments() const
{
  CommonDefines::ArgumentVectorType vecArguments;

  // Add required macro definition which toggle the correct include files
  switch (_eInstructionSet)   // The case-fall-through is intenden here
  {
  case InstructionSetEnum::SSE_4_2:   vecArguments.push_back("-D __SSE4_2__");
  case InstructionSetEnum::SSE_4_1:   vecArguments.push_back("-D __SSE4_1__");
  case InstructionSetEnum::SSSE_3:    vecArguments.push_back("-D __SSSE3__");
  case InstructionSetEnum::SSE_3:     vecArguments.push_back("-D __SSE3__");
  case InstructionSetEnum::SSE_2:     vecArguments.push_back("-D __SSE2__");
  case InstructionSetEnum::SSE:
    {
      vecArguments.push_back("-D __SSE__");
      vecArguments.push_back("-D __MMX__");

      // Add the common intrinsics header
      vecArguments.push_back("-includeimmintrin.h");  // FIXME: Due to a bug in the clang command arguments parser the space between option and switch is missing

      // Enable 64bit extensions of the SSE instruction sets
      vecArguments.push_back("-D __x86_64__");

      break;
    }
  }

  return std::move( vecArguments );
}

bool CPU_x86::CodeGenerator::PrintKernelFunction(FunctionDecl *pKernelFunction, HipaccKernel *pKernel, llvm::raw_ostream &rOutputStream)
{
  HipaccHelper          hipaccHelper(pKernelFunction, pKernel);
  ImageAccessTranslator ImgAccessTranslator(hipaccHelper);

  // Print the instruction set include directive
  if (_bVectorizeKernel)
  {
    string strIncludeFile = _GetInstructionSetIncludeFile();

    if (! strIncludeFile.empty())
    {
      rOutputStream << "\n#include <" << strIncludeFile << ">\n\n";
    }
  }


  // Add the iteration space loops
  {
    ClangASTHelper  ASTHelper(pKernelFunction->getASTContext());

    DeclRefExpr *gid_x_ref = ASTHelper.FindDeclaration(pKernelFunction, HipaccHelper::GlobalIdX());
    DeclRefExpr *gid_y_ref = ASTHelper.FindDeclaration(pKernelFunction, HipaccHelper::GlobalIdY());

    ::clang::CallExpr *pSubFuncCallScalar     = nullptr;
    ::clang::CallExpr *pSubFuncCallVectorized = nullptr;

    // If vectorization is enabled, split the kernel function into the "iteration space"-part and the "pixel-wise processing"-part
    if (_bVectorizeKernel)
    {
      ImgAccessTranslator.TranslateImageAccesses(pKernelFunction->getBody());

      // Push loop body to own function
      ::clang::Stmt *pKernelBody = pKernelFunction->getBody();

      KernelSubFunctionBuilder SubFuncBuilder(ASTHelper.GetASTContext());

      SubFuncBuilder.ImportUsedParameters(pKernelFunction, pKernelBody);

      if ( SubFuncBuilder.IsVariableUsed(HipaccHelper::GlobalIdY(), pKernelBody) )
      {
        SubFuncBuilder.AddCallParameter(gid_y_ref, true);
      }

      if ( SubFuncBuilder.IsVariableUsed(HipaccHelper::GlobalIdX(), pKernelBody) )
      {
        SubFuncBuilder.AddCallParameter(gid_x_ref, true);
      }


      KernelSubFunctionBuilder::DeclCallPairType  DeclCallPair = SubFuncBuilder.CreateFuntionDeclarationAndCall(pKernelFunction->getNameAsString() + string("_Scalar"), pKernelFunction->getResultType());
      ImgAccessTranslator.TranslateImageDeclarations(DeclCallPair.first, ImageAccessTranslator::ImageDeclarationTypes::NativePointer);
      DeclCallPair.first->setBody(pKernelBody);


      // Create function call reference for the scalar sub-functiuon
      pSubFuncCallScalar = DeclCallPair.second;


      // Print the new kernel body sub-function
      rOutputStream << "inline " << _FormatFunctionHeader(DeclCallPair.first, hipaccHelper, false, true);
      DeclCallPair.first->getBody()->printPretty(rOutputStream, 0, GetPrintingPolicy(), 0);
      rOutputStream << "\n\n";


      // Vectorize the kernel sub-function and print it
      ::clang::FunctionDecl *pVecSubFunction = _VectorizeKernelSubFunction(DeclCallPair.first, hipaccHelper);
      pSubFuncCallVectorized = ASTHelper.CreateFunctionCall(pVecSubFunction, SubFuncBuilder.GetCallParameters());

      rOutputStream << "inline " << _FormatFunctionHeader(pVecSubFunction, hipaccHelper, false, true);
      pVecSubFunction->getBody()->printPretty(rOutputStream, 0, GetPrintingPolicy(), 0);
      rOutputStream << "\n\n";
    }


    // Create the iteration space
    ClangASTHelper::StatementVectorType vecKernelFunctionBody;
    ClangASTHelper::StatementVectorType vecOuterLoopBody;

    {
      ClangASTHelper::StatementVectorType vecInnerLoopBody;

      if (_bVectorizeKernel)
      {
        // If vectorization is enabled, redirect all image pointers for the internal kernel function to the currently processed pixel
        for (unsigned int i = 0; i < pKernelFunction->getNumParams(); ++i)
        {
          ::clang::ParmVarDecl  *pParamDecl   = pKernelFunction->getParamDecl(i);
          string                strParamName  = pParamDecl->getNameAsString();

          // Skip all kernel function parameters, which are unused or to not refer to an HIPAcc image
          if ((! hipaccHelper.IsParamUsed(strParamName)) || (hipaccHelper.GetImageFromMapping(strParamName) == nullptr))
          {
            continue;
          }

          // Create declaration for "current line" and "current pixel" image pointers and add them to the iteration space loops
          ImageAccessTranslator::ImageLinePosDeclPairType LinePosDeclPair = ImgAccessTranslator.CreateImageLineAndPosDecl(strParamName);

          vecOuterLoopBody.push_back( ASTHelper.CreateDeclarationStatement(LinePosDeclPair.first) );
          vecInnerLoopBody.push_back( ASTHelper.CreateDeclarationStatement(LinePosDeclPair.second) );


          // Replace all references to the HIPAcc image by the "current pixel" pointer
          ASTHelper.ReplaceDeclarationReferences(pKernelFunction->getBody(), strParamName, LinePosDeclPair.second);
        }

        // Compute the iteration space range, which must be handled by the scalar sub-function
        ::clang::DeclRefExpr  *pScalarRangeRef = nullptr;
        {
          ::clang::VarDecl *pGidXDecl = dyn_cast<::clang::VarDecl>(gid_x_ref->getDecl());

          ::clang::DeclRefExpr  *pIterationSpaceWidth = hipaccHelper.GetImageParameterDecl(pKernelFunction->getParamDecl(0)->getNameAsString(), HipaccHelper::ImageParamType::Width);
          ::clang::QualType     qtReturnType          = pIterationSpaceWidth->getType();

          ::clang::IntegerLiteral *pVectorWidth = ASTHelper.CreateIntegerLiteral(static_cast<int>(_szVectorWidth));
          ::clang::Expr           *pInitExpr    = ASTHelper.CreateBinaryOperator(pIterationSpaceWidth, pVectorWidth, ::clang::BO_Rem, qtReturnType);

          pInitExpr = ASTHelper.CreateParenthesisExpression(pInitExpr);
          pInitExpr = ASTHelper.CreateBinaryOperator(pGidXDecl->getInit(), pInitExpr, ::clang::BO_Add, qtReturnType);

          qtReturnType.addConst();

          ::clang::ValueDecl *pScalarRangeDecl = ASTHelper.CreateVariableDeclaration(pKernelFunction, "is_scalar_range", qtReturnType, pInitExpr);
          vecKernelFunctionBody.push_back(ASTHelper.CreateDeclarationStatement(pScalarRangeDecl));

          pScalarRangeRef = ASTHelper.CreateDeclarationReferenceExpression(pScalarRangeDecl);
        }

        // Create the horizontal iteration space loops and push them to the vertical loop body
        {
          ::clang::ValueDecl    *pNewGidXDecl = ASTHelper.CreateVariableDeclaration(pKernelFunction, HipaccHelper::GlobalIdX(), gid_x_ref->getType(), pScalarRangeRef);
          ::clang::DeclRefExpr  *pNewGidXRef = ASTHelper.CreateDeclarationReferenceExpression(pNewGidXDecl);

          // Add the loop for the scalar function call
          vecInnerLoopBody.push_back(pSubFuncCallScalar);
          vecOuterLoopBody.push_back( _CreateIterationSpaceLoop(ASTHelper, gid_x_ref, pScalarRangeRef, ASTHelper.CreateCompoundStatement(vecInnerLoopBody)) );

          // Add the loop for the vectorized function call
          vecInnerLoopBody.pop_back();
          vecInnerLoopBody.push_back(pSubFuncCallVectorized);
          vecOuterLoopBody.push_back( _CreateIterationSpaceLoop(ASTHelper, pNewGidXRef, hipaccHelper.GetIterationSpaceLimitX(), ASTHelper.CreateCompoundStatement(vecInnerLoopBody)) );
        }
      }
      else
      {
        // Create the horizontal iteration space loop and push it to the vertical loop body
        ForStmt *pInnerLoop = _CreateIterationSpaceLoop(ASTHelper, gid_x_ref, hipaccHelper.GetIterationSpaceLimitX(), pKernelFunction->getBody());
        vecOuterLoopBody.push_back(pInnerLoop);
      }
    }

    // Create the vertical iteration space loop and set it as kernel function body
    vecKernelFunctionBody.push_back( _CreateIterationSpaceLoop(ASTHelper, gid_y_ref, hipaccHelper.GetIterationSpaceLimitY(), ASTHelper.CreateCompoundStatement(vecOuterLoopBody)) );
    pKernelFunction->setBody( ASTHelper.CreateCompoundStatement(vecKernelFunctionBody) );
  }


  rOutputStream << _FormatFunctionHeader(pKernelFunction, hipaccHelper, true, false);

  // print kernel body
  pKernelFunction->getBody()->printPretty(rOutputStream, 0, GetPrintingPolicy(), 0);

  return true;
}


// vim: set ts=2 sw=2 sts=2 et ai:

