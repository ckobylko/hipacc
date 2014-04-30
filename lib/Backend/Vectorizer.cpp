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

//===--- Vectorizer.cpp - Implements a vectorizing component for clang's syntax trees. -===//
//
// This file implements a vectorizing component for clang's syntax trees
//
//===-----------------------------------------------------------------------------------===//

#include "hipacc/Backend/Vectorizer.h"
#include <cstdint>
#include <fstream>
#include <string>
using namespace clang::hipacc::Backend::Vectorization;
using namespace std;


AST::Expressions::BinaryOperatorPtr Vectorizer::VASTBuilder::_BuildBinaryOperatorExpression(::clang::Expr *pExprLHS, ::clang::Expr *pExprRHS, ::clang::BinaryOperatorKind eOpKind)
{
  AST::Expressions::BinaryOperatorPtr spReturnOperator(nullptr);

  if (eOpKind == ::clang::BO_Assign)
  {
    spReturnOperator = AST::CreateNode<AST::Expressions::AssignmentOperator>();
  }
  else if (::clang::BinaryOperator::isComparisonOp(eOpKind) || ::clang::BinaryOperator::isLogicalOp(eOpKind))
  {
    typedef AST::Expressions::RelationalOperator::RelationalOperatorType  OperatorType;

    AST::Expressions::RelationalOperatorPtr spRelationalOp = AST::CreateNode< AST::Expressions::RelationalOperator >();
    spReturnOperator = spRelationalOp;

    switch (eOpKind)
    {
    case BO_EQ:     spRelationalOp->SetOperatorType(OperatorType::Equal);         break;
    case BO_GT:     spRelationalOp->SetOperatorType(OperatorType::Greater);       break;
    case BO_GE:     spRelationalOp->SetOperatorType(OperatorType::GreaterEqual);  break;
    case BO_LT:     spRelationalOp->SetOperatorType(OperatorType::Less);          break;
    case BO_LE:     spRelationalOp->SetOperatorType(OperatorType::LessEqual);     break;
    case BO_LAnd:   spRelationalOp->SetOperatorType(OperatorType::LogicalAnd);    break;
    case BO_LOr:    spRelationalOp->SetOperatorType(OperatorType::LogicalOr);     break;
    case BO_NE:     spRelationalOp->SetOperatorType(OperatorType::NotEqual);      break;
    default:        throw RuntimeErrorException("Invalid relational operator type!");
    }
  }
  else
  {
    typedef AST::Expressions::ArithmeticOperator::ArithmeticOperatorType  OperatorType;

    AST::Expressions::ArithmeticOperatorPtr spArithmeticOp = AST::CreateNode< AST::Expressions::ArithmeticOperator >();
    spReturnOperator = spArithmeticOp;

    switch (eOpKind)
    {
    case BO_Add:    spArithmeticOp->SetOperatorType(OperatorType::Add);         break;
    case BO_And:    spArithmeticOp->SetOperatorType(OperatorType::BitwiseAnd);  break;
    case BO_Or:     spArithmeticOp->SetOperatorType(OperatorType::BitwiseOr);   break;
    case BO_Xor:    spArithmeticOp->SetOperatorType(OperatorType::BitwiseXOr);  break;
    case BO_Div:    spArithmeticOp->SetOperatorType(OperatorType::Divide);      break;
    case BO_Rem:    spArithmeticOp->SetOperatorType(OperatorType::Modulo);      break;
    case BO_Mul:    spArithmeticOp->SetOperatorType(OperatorType::Multiply);    break;
    case BO_Shl:    spArithmeticOp->SetOperatorType(OperatorType::ShiftLeft);   break;
    case BO_Shr:    spArithmeticOp->SetOperatorType(OperatorType::ShiftRight);  break;
    case BO_Sub:    spArithmeticOp->SetOperatorType(OperatorType::Subtract);    break;
    default:        throw RuntimeErrorException("Invalid arithmetic operator type!");
    }
  }


  spReturnOperator->SetLHS( _BuildExpression(pExprLHS) );
  spReturnOperator->SetRHS( _BuildExpression(pExprRHS) );

  return spReturnOperator;
}


AST::Expressions::ConstantPtr Vectorizer::VASTBuilder::_BuildConstantExpression(::clang::Expr *pExpression)
{
  if (isa<::clang::IntegerLiteral>(pExpression))
  {
    ::clang::IntegerLiteral *pIntLiteral  = dyn_cast<::clang::IntegerLiteral>(pExpression);
    llvm::APInt             llvmIntValue  = pIntLiteral->getValue();

    bool          bSigned     = pIntLiteral->getType()->isSignedIntegerType();
    unsigned int  uiBitWidth  = llvmIntValue.getBitWidth();

    uint64_t ui64Value = *llvmIntValue.getRawData();

    if (uiBitWidth <= 8)
    {
      if (bSigned)  return _CreateConstant( static_cast<int8_t >(ui64Value) );
      else          return _CreateConstant( static_cast<uint8_t>(ui64Value) );
    }
    else if (uiBitWidth <= 16)
    {
      if (bSigned)  return _CreateConstant( static_cast<int16_t >(ui64Value) );
      else          return _CreateConstant( static_cast<uint16_t>(ui64Value) );
    }
    else if (uiBitWidth <= 32)
    {
      if (bSigned)  return _CreateConstant( static_cast<int32_t >(ui64Value) );
      else          return _CreateConstant( static_cast<uint32_t>(ui64Value) );
    }
    else
    {
      if (bSigned)  return _CreateConstant( static_cast<int64_t >(ui64Value) );
      else          return _CreateConstant( static_cast<uint64_t>(ui64Value) );
    }
  }
  else if (isa<::clang::FloatingLiteral>(pExpression))
  {
    llvm::APFloat llvmFloatValue = dyn_cast<::clang::FloatingLiteral>(pExpression)->getValue();

    if ( (llvm::APFloat::semanticsPrecision(llvmFloatValue.getSemantics()) == llvm::APFloat::semanticsPrecision(llvm::APFloat::IEEEhalf)) ||
         (llvm::APFloat::semanticsPrecision(llvmFloatValue.getSemantics()) == llvm::APFloat::semanticsPrecision(llvm::APFloat::IEEEsingle)) )
    {
      return _CreateConstant( llvmFloatValue.convertToFloat() );
    }
    else
    {
      return _CreateConstant( llvmFloatValue.convertToDouble() );
    }
  }
  else if (isa<::clang::CXXBoolLiteralExpr>(pExpression))
  {
    return _CreateConstant( dyn_cast<::clang::CXXBoolLiteralExpr>(pExpression)->getValue() );
  }
  else
  {
    throw InternalErrorException("Unknown literal expression!");
  }
}

AST::Expressions::ConversionPtr Vectorizer::VASTBuilder::_BuildConversionExpression(::clang::CastExpr *pCastExpr)
{
  AST::Expressions::ConversionPtr spConversion = AST::CreateNode< AST::Expressions::Conversion >();

  AST::BaseClasses::TypeInfo CastType;
  _ConvertTypeInfo( CastType, pCastExpr->getType() );

  spConversion->SetConvertType( CastType );
  spConversion->SetSubExpression( _BuildExpression(pCastExpr->getSubExpr()) );

  return spConversion;
}

AST::BaseClasses::ExpressionPtr Vectorizer::VASTBuilder::_BuildExpression(::clang::Expr *pExpression)
{
  AST::BaseClasses::ExpressionPtr spReturnExpression(nullptr);

  if (isa<::clang::IntegerLiteral>(pExpression) || isa<::clang::FloatingLiteral>(pExpression) || isa<::clang::CXXBoolLiteralExpr>(pExpression))
  {
    spReturnExpression = _BuildConstantExpression(pExpression);
  }
  else if (isa<::clang::DeclRefExpr>(pExpression))
  {
    AST::Expressions::IdentifierPtr spIdentifier = AST::CreateNode<AST::Expressions::Identifier>();
    spReturnExpression = spIdentifier;

    spIdentifier->SetName( dyn_cast<::clang::DeclRefExpr>(pExpression)->getNameInfo().getAsString() );
  }
  else if (isa<::clang::CompoundAssignOperator>(pExpression))
  {
    ::clang::CompoundAssignOperator *pCompoundAssignment  = dyn_cast<::clang::CompoundAssignOperator>(pExpression);
    ::clang::Expr                   *pExprLHS             = pCompoundAssignment->getLHS();
    ::clang::Expr                   *pExprRHS             = pCompoundAssignment->getRHS();
    ::clang::BinaryOperatorKind     eOpKind               = pCompoundAssignment->getOpcode();

    switch (eOpKind)
    {
    case BO_AddAssign:  eOpKind = BO_Add;   break;
    case BO_AndAssign:  eOpKind = BO_And;   break;
    case BO_DivAssign:  eOpKind = BO_Div;   break;
    case BO_MulAssign:  eOpKind = BO_Mul;   break;
    case BO_OrAssign:   eOpKind = BO_Or;    break;
    case BO_RemAssign:  eOpKind = BO_Rem;   break;
    case BO_ShlAssign:  eOpKind = BO_Shl;   break;
    case BO_ShrAssign:  eOpKind = BO_Shr;   break;
    case BO_SubAssign:  eOpKind = BO_Sub;   break;
    case BO_XorAssign:  eOpKind = BO_Xor;   break;
    }

    AST::Expressions::AssignmentOperatorPtr spAssignment = AST::CreateNode<AST::Expressions::AssignmentOperator>();
    spReturnExpression = spAssignment;

    spAssignment->SetLHS( _BuildExpression(pExprLHS) );
    spAssignment->SetRHS( _BuildBinaryOperatorExpression(pExprLHS, pExprRHS, eOpKind) );
  }
  else if (isa<::clang::BinaryOperator>(pExpression))
  {
    ::clang::BinaryOperator *pBinOp = dyn_cast<::clang::BinaryOperator>(pExpression);

    spReturnExpression = _BuildBinaryOperatorExpression( pBinOp->getLHS(), pBinOp->getRHS(), pBinOp->getOpcode() );
  }
  else if (isa<::clang::CastExpr>(pExpression))
  {
    spReturnExpression = _BuildConversionExpression(dyn_cast<::clang::CastExpr>(pExpression));
  }
  else if (isa<::clang::ParenExpr>(pExpression))
  {
    AST::Expressions::ParenthesisPtr  spParenthesis = AST::CreateNode< AST::Expressions::Parenthesis >();
    spReturnExpression = spParenthesis;

    spParenthesis->SetSubExpression( _BuildExpression(dyn_cast<::clang::ParenExpr>(pExpression)->getSubExpr()) );
  }
  else if (isa<::clang::ArraySubscriptExpr>(pExpression))
  {
    ::clang::ArraySubscriptExpr *pArraySubscript = dyn_cast<::clang::ArraySubscriptExpr>(pExpression);

    AST::Expressions::MemoryAccessPtr spMemoryAccess = AST::CreateNode< AST::Expressions::MemoryAccess >();
    spReturnExpression = spMemoryAccess;

    spMemoryAccess->SetMemoryReference( _BuildExpression(pArraySubscript->getLHS()) );
    spMemoryAccess->SetIndexExpression( _BuildExpression(pArraySubscript->getRHS()) );
  }
  else if (isa<::clang::UnaryOperator>(pExpression))
  {
    ::clang::UnaryOperator      *pUnaryOp = dyn_cast<::clang::UnaryOperator>(pExpression);
    ::clang::Expr               *pSubExpr = pUnaryOp->getSubExpr();
    ::clang::UnaryOperatorKind  eOpCode   = pUnaryOp->getOpcode();

    if (eOpCode == ::clang::UO_Deref)
    {
      AST::Expressions::MemoryAccessPtr spMemoryAccess = AST::CreateNode< AST::Expressions::MemoryAccess >();
      spReturnExpression = spMemoryAccess;

      spMemoryAccess->SetMemoryReference( _BuildExpression(pSubExpr) );
      spMemoryAccess->SetIndexExpression( _CreateConstant< int32_t >( 0 ) );
    }
    else
    {
      spReturnExpression = _BuildUnaryOperatorExpression(pSubExpr, eOpCode);
    }
  }
  else
  {
    throw ASTExceptions::UnknownExpressionClass( pExpression->getStmtClassName() );
  }

  return spReturnExpression;
}

void Vectorizer::VASTBuilder::_BuildLoop(::clang::Stmt *pLoopStatement, AST::ScopePtr spEnclosingScope)
{
  AST::ControlFlow::LoopPtr spLoop = AST::CreateNode< AST::ControlFlow::Loop >();

  ::clang::Stmt   *pLoopBody  = nullptr;
  ::clang::Expr   *pCondition = nullptr;

  if (isa<::clang::ForStmt>(pLoopStatement))
  {
    spLoop->SetLoopType(AST::ControlFlow::Loop::LoopType::TopControlled);

    ::clang::ForStmt *pForLoop = dyn_cast<::clang::ForStmt>(pLoopStatement);

    // If we have an init statement, create a container scope around the loop and add the init statement
    if (pForLoop->getInit())
    {
      AST::ScopePtr spLoopHolderScope = AST::CreateNode< AST::Scope >();
      spEnclosingScope->AddChild(spLoopHolderScope);

      AST::BaseClasses::NodePtr spInitStatement = _BuildStatement(pForLoop->getInit(), spLoopHolderScope);
      if (spInitStatement)
      {
        spLoopHolderScope->AddChild(spInitStatement);
      }

      spLoopHolderScope->AddChild(spLoop);
    }
    else
    {
      spEnclosingScope->AddChild(spLoop);
    }

    // Build increment expression if it is present
    if (pForLoop->getInc())
    {
      spLoop->SetIncrement( _BuildExpression(pForLoop->getInc()) );
    }

    pCondition  = pForLoop->getCond();
    pLoopBody   = pForLoop->getBody();
  }
  else
  {
    spEnclosingScope->AddChild(spLoop);

    if (isa<::clang::DoStmt>(pLoopStatement))
    {
      spLoop->SetLoopType(AST::ControlFlow::Loop::LoopType::BottomControlled);

      ::clang::DoStmt *pDoWhileLoop = dyn_cast<::clang::DoStmt>(pLoopStatement);
      pCondition  = pDoWhileLoop->getCond();
      pLoopBody   = pDoWhileLoop->getBody();
    }
    else if (isa<::clang::WhileStmt>(pLoopStatement))
    {
      spLoop->SetLoopType(AST::ControlFlow::Loop::LoopType::TopControlled);

      ::clang::WhileStmt *pWhileLoop = dyn_cast<::clang::WhileStmt>(pLoopStatement);
      pCondition  = pWhileLoop->getCond();
      pLoopBody   = pWhileLoop->getBody();
    }
    else
    {
      throw ASTExceptions::UnknownStatementClass(pLoopStatement->getStmtClassName());
    }
  }


  // Build condition expression
  spLoop->SetCondition( _BuildExpression(pCondition) );

  // Build loop body if it is present
  AST::ScopePtr spLoopBody = spLoop->GetBody();  // Must be called in every case, so that the loop body node is created
  if (pLoopBody != nullptr)
  {
    if (isa<::clang::CompoundStmt>(pLoopBody))
    {
      _ConvertScope(spLoopBody, dyn_cast<::clang::CompoundStmt>(pLoopBody));
    }
    else
    {
      AST::BaseClasses::NodePtr spChild = _BuildStatement(pLoopBody, spLoopBody);
      if (spChild)
      {
        spLoopBody->AddChild(spChild);
      }
    }
  }
}

AST::BaseClasses::NodePtr Vectorizer::VASTBuilder::_BuildStatement(::clang::Stmt *pStatement, AST::ScopePtr spEnclosingScope)
{
  AST::BaseClasses::NodePtr spStatement(nullptr);

  if (isa<::clang::CompoundStmt>(pStatement))
  {
    ::clang::CompoundStmt *pCurrentCompound = dyn_cast<::clang::CompoundStmt>(pStatement);

    AST::ScopePtr spChildScope = AST::CreateNode<AST::Scope>();
    spStatement = spChildScope;

    _ConvertScope(spChildScope, pCurrentCompound);
  }
  else if (isa<::clang::DeclStmt>(pStatement))
  {
    ::clang::DeclStmt     *pDeclStatement = dyn_cast<::clang::DeclStmt>(pStatement);
    ::clang::DeclGroupRef  DeclGroup      = pDeclStatement->getDeclGroup();

    for (auto itDecl = DeclGroup.begin(); itDecl != DeclGroup.end(); itDecl++)
    {
      ::clang::Decl *pDecl = *itDecl;
      if (pDecl == nullptr)
      {
        continue;
      }
      else if (! isa<::clang::VarDecl>(pDecl))
      {
        continue;
      }

      ::clang::VarDecl  *pVarDecl = dyn_cast<::clang::VarDecl>(pDecl);
      spEnclosingScope->AddVariable( _BuildVariableInfo(pVarDecl) );

      ::clang::Expr     *pInitExpr = pVarDecl->getInit();
      if (pInitExpr == nullptr)
      {
        continue;
      }

      AST::Expressions::AssignmentOperatorPtr spAssignment = AST::CreateNode< AST::Expressions::AssignmentOperator >();

      AST::Expressions::IdentifierPtr spVariable = AST::CreateNode< AST::Expressions::Identifier >();
      spVariable->SetName(pVarDecl->getNameAsString());

      spAssignment->SetLHS(spVariable);
      spAssignment->SetRHS(_BuildExpression(pInitExpr));

      spEnclosingScope->AddChild(spAssignment);
    }

    spStatement = nullptr;
  }
  else if (isa<::clang::Expr>(pStatement))
  {
    AST::BaseClasses::ExpressionPtr spExpression = _BuildExpression(dyn_cast<::clang::Expr>(pStatement));
    if (!spExpression)
    {
      throw InternalErrors::NullPointerException("spStatement");
    }

    spStatement = spExpression;
  }
  else if ( isa<::clang::DoStmt>(pStatement) || isa<::clang::ForStmt>(pStatement) || isa<::clang::WhileStmt>(pStatement) )
  {
    _BuildLoop(pStatement, spEnclosingScope);
  }
  else
  {
    throw ASTExceptions::UnknownStatementClass(pStatement->getStmtClassName());
  }

  return spStatement;
}

AST::Expressions::UnaryOperatorPtr Vectorizer::VASTBuilder::_BuildUnaryOperatorExpression(::clang::Expr *pSubExpr, ::clang::UnaryOperatorKind eOpKind)
{
  typedef AST::Expressions::UnaryOperator::UnaryOperatorType OperatorType;

  AST::Expressions::UnaryOperatorPtr spReturnOperator = AST::CreateNode< AST::Expressions::UnaryOperator >();

  switch (eOpKind)
  {
  case UO_AddrOf:   spReturnOperator->SetOperatorType(OperatorType::AddressOf);       break;
  case UO_Not:      spReturnOperator->SetOperatorType(OperatorType::BitwiseNot);      break;
  case UO_LNot:     spReturnOperator->SetOperatorType(OperatorType::LogicalNot);      break;
  case UO_Minus:    spReturnOperator->SetOperatorType(OperatorType::Minus);           break;
  case UO_Plus:     spReturnOperator->SetOperatorType(OperatorType::Plus);            break;
  case UO_PostDec:  spReturnOperator->SetOperatorType(OperatorType::PostDecrement);   break;
  case UO_PostInc:  spReturnOperator->SetOperatorType(OperatorType::PostIncrement);   break;
  case UO_PreDec:   spReturnOperator->SetOperatorType(OperatorType::PreDecrement);    break;
  case UO_PreInc:   spReturnOperator->SetOperatorType(OperatorType::PreIncrement);    break;
  default:          throw RuntimeErrorException("Invalid unary operator type!");
  }

  spReturnOperator->SetSubExpression( _BuildExpression(pSubExpr) );

  return spReturnOperator;
}

AST::BaseClasses::VariableInfoPtr Vectorizer::VASTBuilder::_BuildVariableInfo(::clang::VarDecl *pVarDecl)
{
  AST::BaseClasses::VariableInfoPtr spVariableInfo = std::make_shared< AST::BaseClasses::VariableInfo >();
  spVariableInfo->SetName(pVarDecl->getNameAsString());

  _ConvertTypeInfo( spVariableInfo->GetTypeInfo(), pVarDecl->getType() );

  return spVariableInfo;
}

void Vectorizer::VASTBuilder::_ConvertScope(AST::ScopePtr spScope, ::clang::CompoundStmt *pCompoundStatement)
{
  for (auto itChild = pCompoundStatement->child_begin(); itChild != pCompoundStatement->child_end(); itChild++)
  {
    ::clang::Stmt *pChildStatement = *itChild;
    if (pChildStatement == nullptr)
    {
      continue;
    }

    AST::BaseClasses::NodePtr spChild = _BuildStatement(pChildStatement, spScope);
    if (spChild)
    {
      spScope->AddChild(spChild);
    }
  }
}

void Vectorizer::VASTBuilder::_ConvertTypeInfo(AST::BaseClasses::TypeInfo &rTypeInfo, ::clang::QualType qtSourceType)
{
  while (qtSourceType->isArrayType())
  {
    const ::clang::ArrayType *pArrayType = qtSourceType->getAsArrayTypeUnsafe();

    if (pArrayType->isConstantArrayType())
    {
      const ::clang::ConstantArrayType *pConstArrayType = dyn_cast<::clang::ConstantArrayType>(pArrayType);

      rTypeInfo.GetArrayDimensions().push_back( static_cast< size_t >( *(pConstArrayType->getSize().getRawData()) ) );
    }
    else
    {
      throw RuntimeErrorException("Only constant size array types allowed!");
    }

    qtSourceType = pArrayType->getElementType();
  }

  if (qtSourceType->isPointerType())
  {
    rTypeInfo.SetPointer(true);
    qtSourceType = qtSourceType->getPointeeType();

    if (qtSourceType->isPointerType())
    {
      throw RuntimeErrorException("Only one level of indirection is allowed for pointer types!");
    }
  }
  else
  {
    rTypeInfo.SetPointer(false);
  }

  rTypeInfo.SetConst(qtSourceType.isConstQualified());

  if (qtSourceType->isScalarType())
  {
    qtSourceType = qtSourceType->getCanonicalTypeInternal();

    if (qtSourceType->isBuiltinType())
    {
      typedef ::clang::BuiltinType                    ClangTypes;
      typedef AST::BaseClasses::TypeInfo::KnownTypes  KnownTypes;

      const ::clang::BuiltinType *pBuiltInType = qtSourceType->getAs<::clang::BuiltinType>();

      KnownTypes eType;

      switch (pBuiltInType->getKind())
      {
      case ClangTypes::Bool:                              eType = KnownTypes::Bool;     break;
      case ClangTypes::Char_S: case ClangTypes::SChar:    eType = KnownTypes::Int8;     break;
      case ClangTypes::Char_U: case ClangTypes::UChar:    eType = KnownTypes::UInt8;    break;
      case ClangTypes::Short:                             eType = KnownTypes::Int16;    break;
      case ClangTypes::UShort:                            eType = KnownTypes::UInt16;   break;
      case ClangTypes::Int:                               eType = KnownTypes::Int32;    break;
      case ClangTypes::UInt:                              eType = KnownTypes::UInt32;   break;
      case ClangTypes::Long:                              eType = KnownTypes::Int64;    break;
      case ClangTypes::ULong:                             eType = KnownTypes::UInt64;   break;
      case ClangTypes::Float:                             eType = KnownTypes::Float;    break;
      case ClangTypes::Double:                            eType = KnownTypes::Double;   break;
      default:                                            throw RuntimeErrorException("Unsupported built-in type detected!");
      }

      rTypeInfo.SetType(eType);
    }
    else
    {
      throw RuntimeErrorException("Expected a built-in type!");
    }
  }
  else
  {
    throw RuntimeErrorException("Only scalar types, pointers to scalar types, or arrays of scalar or pointers to scalar types allowed!");
  }
}



AST::FunctionDeclarationPtr Vectorizer::VASTBuilder::BuildFunctionDecl(::clang::FunctionDecl *pFunctionDeclaration)
{
  AST::FunctionDeclarationPtr spFunctionDecl = AST::CreateNode< AST::FunctionDeclaration >();

  spFunctionDecl->SetName(pFunctionDeclaration->getName());

  for (size_t i = 0; i < pFunctionDeclaration->getNumParams(); ++i)
  {
    AST::BaseClasses::VariableInfoPtr spVariable = _BuildVariableInfo( pFunctionDeclaration->getParamDecl(i) );

    spFunctionDecl->AddParameter(spVariable);
  }

  ::clang::Stmt* pBody = pFunctionDeclaration->getBody();

  if ((pBody == nullptr) || (!isa<::clang::CompoundStmt>(pBody)))
  {
    throw RuntimeErrorException("Invalid function body");
  }

  _ConvertScope(spFunctionDecl->GetBody(), dyn_cast<::clang::CompoundStmt>(pBody));

  return spFunctionDecl;
}



AST::FunctionDeclarationPtr Vectorizer::ConvertClangFunctionDecl(::clang::FunctionDecl *pFunctionDeclaration)
{
  try
  {
//VASTBuilder().Import(pFunctionDeclaration);

    AST::FunctionDeclarationPtr spFunctionDecl = VASTBuilder::BuildFunctionDecl(pFunctionDeclaration);

    std::ofstream XmlStream("Dump.xml");

    XmlStream << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
    XmlStream << spFunctionDecl->DumpToXML(0);

    XmlStream.flush();
    XmlStream.close();

    return spFunctionDecl;
  }
  catch (BackendException &e)
  {
    llvm::errs() << "\n\nERROR: " << e.what() << "\n\n";
    exit(EXIT_FAILURE);
  }
}


// vim: set ts=2 sw=2 sts=2 et ai:

