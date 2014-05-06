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
#include <sstream>
using namespace clang::hipacc::Backend::Vectorization;
using namespace std;


void  Vectorizer::VASTBuilder::VariableNameTranslator::AddRenameEntry(string strOriginalName, string strNewName)
{
  if (_lstRenameStack.empty())
  {
    throw InternalErrorException("Rename stack is empty");
  }
  else if (strOriginalName == strNewName)
  {
    throw InternalErrorException("The original and new name cannot be identical!");
  }

  RenameMapType &rCurrentMap = _lstRenameStack.front();
  if (rCurrentMap.find(strOriginalName) != rCurrentMap.end())
  {
    throw InternalErrorException(string("The variable name \"") + strOriginalName + string("\" is already known at this layer!"));
  }

  rCurrentMap[strOriginalName] = strNewName;
}

string Vectorizer::VASTBuilder::VariableNameTranslator::TranslateName(string strOriginalName) const
{
  for (auto itMap = _lstRenameStack.begin(); itMap != _lstRenameStack.end(); itMap++)
  {
    auto itEntry = itMap->find(strOriginalName);

    if (itEntry != itMap->end())
    {
      return itEntry->second;
    }
  }

  return strOriginalName;
}



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

void Vectorizer::VASTBuilder::_BuildBranchingStatement(::clang::IfStmt *pIfStmt, AST::ScopePtr spEnclosingScope)
{
  AST::ControlFlow::BranchingStatementPtr spBranchingStmt = AST::CreateNode<AST::ControlFlow::BranchingStatement>();
  spEnclosingScope->AddChild(spBranchingStmt);

  // Unroll the "if-else"-cascade in the clang AST
  ::clang::Stmt *pCurrentStatement = pIfStmt;
  while (isa<::clang::IfStmt>(pCurrentStatement))
  {
    pCurrentStatement = _BuildConditionalBranch(dyn_cast<::clang::IfStmt>(pCurrentStatement), spBranchingStmt);
    if (pCurrentStatement == nullptr)
    {
      break;
    }
  }

  // Build default branch
  AST::ScopePtr spDefaultBranch = spBranchingStmt->GetDefaultBranch();
  if (pCurrentStatement != nullptr)
  {
    if (isa<::clang::CompoundStmt>(pCurrentStatement))
    {
      _ConvertScope(spDefaultBranch, dyn_cast<::clang::CompoundStmt>(pCurrentStatement));
    }
    else
    {
      AST::BaseClasses::NodePtr spChild = _BuildStatement(pCurrentStatement, spDefaultBranch);
      if (spChild)
      {
        spDefaultBranch->AddChild(spChild);
      }
    }
  }
}

::clang::Stmt* Vectorizer::VASTBuilder::_BuildConditionalBranch(::clang::IfStmt *pIfStmt, AST::ControlFlow::BranchingStatementPtr spBranchingStatement)
{
  AST::ControlFlow::ConditionalBranchPtr spBranch = AST::CreateNode< AST::ControlFlow::ConditionalBranch >();
  spBranchingStatement->AddConditionalBranch(spBranch);

  spBranch->SetCondition( _BuildExpression(pIfStmt->getCond()) );

  AST::ScopePtr spBranchBody  = spBranch->GetBody();
  ::clang::Stmt *pIfBody      = pIfStmt->getThen();
  if (pIfBody != nullptr)
  {
    if (isa<::clang::CompoundStmt>(pIfBody))
    {
      _ConvertScope(spBranchBody, dyn_cast<::clang::CompoundStmt>(pIfBody));
    }
    else
    {
      AST::BaseClasses::NodePtr spChild = _BuildStatement(pIfBody, spBranchBody);
      if (spChild)
      {
        spBranchBody->AddChild(spChild);
      }
    }
  }

  return pIfStmt->getElse();
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
    spReturnExpression = _BuildIdentifier( dyn_cast< ::clang::DeclRefExpr >(pExpression)->getNameInfo().getAsString() );
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

    spReturnExpression = CreateAssignmentOperator( _BuildExpression(pExprLHS), _BuildBinaryOperatorExpression(pExprLHS, pExprRHS, eOpKind) );
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
  else if (isa<::clang::CallExpr>(pExpression))
  {
    ::clang::CallExpr *pCallExpr  = dyn_cast<::clang::CallExpr>(pExpression);

    AST::Expressions::FunctionCallPtr spFunctionCall = AST::CreateNode< AST::Expressions::FunctionCall >();
    spReturnExpression = spFunctionCall;

    // Set the function name
    {
      ::clang::FunctionDecl *pCalleeDecl = pCallExpr->getDirectCallee();
      if (pCalleeDecl == nullptr)
      {
        throw InternalErrors::NullPointerException("pCalleeDecl");
      }

      spFunctionCall->SetName(pCalleeDecl->getNameAsString());
    }

    // Set the return type
    {
      AST::BaseClasses::TypeInfo  ReturnType = _ConvertTypeInfo(pCallExpr->getCallReturnType());
      if (ReturnType.IsSingleValue())
      {
        ReturnType.SetConst(true);
      }

      spFunctionCall->SetReturnType(ReturnType);
    }


    // Build the call parameter expressions
    for (unsigned int i = 0; i < pCallExpr->getNumArgs(); ++i)
    {
      ::clang::Expr *pArg = pCallExpr->getArg(i);
      if (pArg == nullptr)
      {
        throw InternalErrors::NullPointerException("pArg");
      }

      spFunctionCall->AddCallParameter( _BuildExpression(pArg) );
    }
  }
  else
  {
    throw ASTExceptions::UnknownExpressionClass( pExpression->getStmtClassName() );
  }

  return spReturnExpression;
}

AST::Expressions::IdentifierPtr Vectorizer::VASTBuilder::_BuildIdentifier(string strIdentifierName)
{
  return CreateIdentifier( _VarTranslator.TranslateName(strIdentifierName) );
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
      spEnclosingScope->AddVariableDeclaration( _BuildVariableInfo(pVarDecl, spEnclosingScope) );

      ::clang::Expr     *pInitExpr = pVarDecl->getInit();
      if (pInitExpr == nullptr)
      {
        continue;
      }

      AST::Expressions::AssignmentOperatorPtr spAssignment = AST::CreateNode< AST::Expressions::AssignmentOperator >();

      spAssignment->SetLHS( _BuildIdentifier(pVarDecl->getNameAsString()) );
      spAssignment->SetRHS( _BuildExpression(pInitExpr) );

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
  else if (isa<::clang::IfStmt>(pStatement))
  {
    _BuildBranchingStatement(dyn_cast<::clang::IfStmt>(pStatement), spEnclosingScope);
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

AST::BaseClasses::VariableInfoPtr Vectorizer::VASTBuilder::_BuildVariableInfo(::clang::VarDecl *pVarDecl, AST::IVariableContainerPtr spVariableContainer)
{
  AST::BaseClasses::VariableInfoPtr spVariableInfo = std::make_shared< AST::BaseClasses::VariableInfo >();

  string strVariableName = pVarDecl->getNameAsString();

  if ( spVariableContainer->IsVariableUsed(strVariableName) )
  {
    string strNewName = GetNextFreeVariableName( spVariableContainer, strVariableName );

    _VarTranslator.AddRenameEntry(strVariableName, strNewName);

    strVariableName = strNewName;
  }


  spVariableInfo->SetName( strVariableName );

  _ConvertTypeInfo( spVariableInfo->GetTypeInfo(), pVarDecl->getType() );

  return spVariableInfo;
}

void Vectorizer::VASTBuilder::_ConvertScope(AST::ScopePtr spScope, ::clang::CompoundStmt *pCompoundStatement)
{
  _VarTranslator.AddLayer();

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

  _VarTranslator.PopLayer();
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
    AST::BaseClasses::VariableInfoPtr spVariable = _BuildVariableInfo( pFunctionDeclaration->getParamDecl(i), spFunctionDecl );

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

AST::Expressions::AssignmentOperatorPtr Vectorizer::VASTBuilder::CreateAssignmentOperator(AST::BaseClasses::ExpressionPtr spLHS, AST::BaseClasses::ExpressionPtr spRHS)
{
  AST::Expressions::AssignmentOperatorPtr spAssignment = AST::CreateNode< AST::Expressions::AssignmentOperator >();

  spAssignment->SetLHS( spLHS );
  spAssignment->SetRHS( spRHS );

  return spAssignment;
}

AST::Expressions::IdentifierPtr Vectorizer::VASTBuilder::CreateIdentifier(string strIdentifierName)
{
  AST::Expressions::IdentifierPtr spIdentifier = AST::CreateNode<AST::Expressions::Identifier>();

  spIdentifier->SetName(strIdentifierName);

  return spIdentifier;
}

string Vectorizer::VASTBuilder::GetNextFreeVariableName(AST::IVariableContainerPtr spVariableContainer, string strRootName)
{
  if (!spVariableContainer)
  {
    throw InternalErrors::NullPointerException("spVariableContainer");
  }
  else if (!spVariableContainer->IsVariableUsed(strRootName))
  {
    return strRootName;
  }

  for (int iVarSuffix = 0; true; ++iVarSuffix)
  {
    stringstream VarNameStream;

    VarNameStream << strRootName << "_" << iVarSuffix;

    string strCurrentName = VarNameStream.str();

    if (!spVariableContainer->IsVariableUsed(strCurrentName))
    {
      return strCurrentName;
    }
  }
}




void Vectorizer::Transformations::CheckInternalDeclaration::Execute(AST::ScopePtr spScope)
{
  if (spScope->HasVariableDeclaration(_strDeclName))
  {
    _bFound = true;
  }
}

void Vectorizer::Transformations::FindBranchingInternalAssignments::Execute(AST::ControlFlow::BranchingStatementPtr spBranchingStmt)
{
  list< AST::BaseClasses::ExpressionPtr > lstConditions;

  // Find all assignments in every conditional branch => each branch depends on its condition as well as on the conditions of the preceding branches
  for (IndexType iBranchIdx = static_cast<IndexType>(0); iBranchIdx < spBranchingStmt->GetConditionalBranchesCount(); ++iBranchIdx)
  {
    AST::ControlFlow::ConditionalBranchPtr spBranch = spBranchingStmt->GetConditionalBranch(iBranchIdx);
    if (! spBranch)
    {
      throw InternalErrors::NullPointerException("spBranch");
    }

    lstConditions.push_back(spBranch->GetCondition());

    Transformations::FindAssignments AssignmentFinder;

    _RunVASTTransformation(spBranch->GetBody(), AssignmentFinder);

    for each (auto itAssignment in AssignmentFinder.lstAssignments)
    {
      // Check if the assignment is done to a branch internal variable (these variables do not depend on the conditions)
      CheckInternalDeclaration DeclChecker( _GetAssigneeInfo(itAssignment)->GetName() );
      _RunVASTTransformation( spBranch, DeclChecker );

      // Only add assignments to external variables to the result map
      if (! DeclChecker.Found())
      {
        for each (auto itCondition in lstConditions)
        {
          mapConditionalAssignments[itAssignment].push_back(itCondition);
        }
      }
    }
  }

  // Find the assignments in the default branch => this branch depends on the conditions of ALL conditional branches
  {
    Transformations::FindAssignments AssignmentFinder;

    _RunVASTTransformation(spBranchingStmt->GetDefaultBranch(), AssignmentFinder);

    for each (auto itAssignment in AssignmentFinder.lstAssignments)
    {
      // Check if the assignment is done to a branch internal variable (these variables do not depend on the conditions)
      CheckInternalDeclaration DeclChecker( _GetAssigneeInfo(itAssignment)->GetName() );
      _RunVASTTransformation( spBranchingStmt->GetDefaultBranch(), DeclChecker );

      // Only add assignments to external variables to the result map
      if (! DeclChecker.Found())
      {
        for each (auto itCondition in lstConditions)
        {
          mapConditionalAssignments[itAssignment].push_back(itCondition);
        }
      }
    }
  }
}

void Vectorizer::Transformations::FindLoopInternalAssignments::Execute(AST::ControlFlow::LoopPtr spLoop)
{
  // Find all assignments inside the loop body
  Transformations::FindAssignments AssignmentFinder;

  _RunVASTTransformation(spLoop->GetBody(), AssignmentFinder);

  for each (auto itAssignment in AssignmentFinder.lstAssignments)
  {
    // Check if the assignment is done to a loop internal variable (these variables do not depend on the loop condition)
    CheckInternalDeclaration DeclChecker( _GetAssigneeInfo(itAssignment)->GetName() );
    _RunVASTTransformation(spLoop->GetBody(), DeclChecker);

    // Only add assignments to external variables to the result map
    if (! DeclChecker.Found())
    {
      mapConditionalAssignments[itAssignment].push_back(spLoop->GetCondition());
    }
  }
}

void Vectorizer::Transformations::FlattenMemoryAccesses::Execute(AST::Expressions::MemoryAccessPtr spMemoryAccess)
{
  if (spMemoryAccess->IsVectorized())
  {
    AST::BaseClasses::ExpressionPtr spIndexExpr = spMemoryAccess->GetIndexExpression();

    bool bIsSingleValue = spIndexExpr->IsType<AST::Expressions::Identifier>() || spIndexExpr->IsType<AST::Expressions::Constant>();

    if (! bIsSingleValue)
    {
      AST::ScopePosition  ScopePos        = spMemoryAccess->GetScopePosition();
      AST::ScopePtr       spCurrentScope  = ScopePos.GetScope();

      // Create a name for a new tempory index variable
      string strIndexVariableName = VASTBuilder::GetNextFreeVariableName(spCurrentScope, VASTBuilder::GetTemporaryNamePrefix() + string("_index"));

      // Create the new index variable declaration
      {
        AST::BaseClasses::VariableInfoPtr spVariableInfo = std::make_shared< AST::BaseClasses::VariableInfo >();
        spVariableInfo->GetTypeInfo() = spIndexExpr->GetResultType();
        spVariableInfo->SetName(strIndexVariableName);
        spVariableInfo->SetVectorize(spIndexExpr->IsVectorized());

        spCurrentScope->AddVariableDeclaration(spVariableInfo);
      }

      // Create the assignment expression for the new index variable
      spCurrentScope->InsertChild( ScopePos.GetChildIndex(), VASTBuilder::CreateAssignmentOperator( VASTBuilder::CreateIdentifier(strIndexVariableName), spIndexExpr ) );

      // Set the new index variable as index expression for the memory access
      spMemoryAccess->SetIndexExpression( VASTBuilder::CreateIdentifier( strIndexVariableName ) );
    }
  }
}

Vectorizer::IndexType Vectorizer::Transformations::FlattenScopes::ProcessChild(AST::ScopePtr spParentScope, IndexType iChildIndex, AST::ScopePtr spChildScope)
{
  bool      bRemoved    = false;
  IndexType ChildCount  = spChildScope->GetChildCount();

  if (ChildCount == static_cast<IndexType>(0))
  {
    spParentScope->RemoveChild(iChildIndex);
    bRemoved = true;
    --iChildIndex;
  }
  else if (ChildCount == static_cast<IndexType>(1))
  {
    spParentScope->SetChild(iChildIndex, spChildScope->GetChild(0));
    bRemoved = true;
  }

  if (bRemoved)
  {
    spParentScope->ImportVariableDeclarations( spChildScope );
  }

  return iChildIndex;
}

Vectorizer::IndexType Vectorizer::Transformations::RemoveUnnecessaryConversions::ProcessChild(AST::BaseClasses::ExpressionPtr spParentExpression, IndexType iChildIndex, AST::Expressions::ConversionPtr spConversion)
{
  AST::BaseClasses::ExpressionPtr spSubExpression = spConversion->GetSubExpression();
  AST::BaseClasses::TypeInfo      ConvertType     = spConversion->GetResultType();

  if (spSubExpression)
  {
    AST::BaseClasses::TypeInfo  ChildType = spConversion->GetSubExpression()->GetResultType();
    bool bRemoveConversion = false;

    if (spSubExpression->IsType<AST::Expressions::Constant>())
    {
      AST::Expressions::ConstantPtr spConstant = spSubExpression->CastToType<AST::Expressions::Constant>();
      spConstant->ChangeType(ConvertType.GetType());
      bRemoveConversion = true;
    }
    else
    {
      bRemoveConversion = ConvertType.IsEqual(ChildType, true);
    }

    if (bRemoveConversion)
    {
      spParentExpression->SetSubExpression(iChildIndex, spConversion->GetSubExpression());
    }
  }

  return iChildIndex;
}

void Vectorizer::Transformations::SeparateBranchingStatements::Execute(AST::ControlFlow::BranchingStatementPtr spBranchingStmt)
{
  if (spBranchingStmt->IsVectorized())
  {
    IndexType iFirstVecBranch = static_cast< IndexType >( 0 );
    for (IndexType iBranchIdx = static_cast<IndexType>(0); iBranchIdx < spBranchingStmt->GetConditionalBranchesCount(); ++iBranchIdx)
    {
      if (spBranchingStmt->GetConditionalBranch(iBranchIdx)->IsVectorized())
      {
        iFirstVecBranch = iBranchIdx;
        break;
      }
    }

    if (iFirstVecBranch == static_cast<IndexType>(0))
    {
      return;
    }


    AST::ControlFlow::BranchingStatementPtr spVecBranchStatement = AST::CreateNode< AST::ControlFlow::BranchingStatement >();

    while (iFirstVecBranch < spBranchingStmt->GetConditionalBranchesCount())
    {
      spVecBranchStatement->AddConditionalBranch( spBranchingStmt->GetConditionalBranch(iFirstVecBranch) );

      spBranchingStmt->RemoveConditionalBranch(iFirstVecBranch);
    }

    AST::ScopePtr spDefaultBranch     = spBranchingStmt->GetDefaultBranch();
    AST::ScopePtr spDefaultBranchNew  = spVecBranchStatement->GetDefaultBranch();

    while (spDefaultBranch->GetChildCount() > static_cast< IndexType >(0))
    {
      spDefaultBranchNew->AddChild( spDefaultBranch->GetChild(0) );

      spDefaultBranch->RemoveChild(0);
    }

    spDefaultBranchNew->ImportVariableDeclarations( spDefaultBranch );

    spDefaultBranch->AddChild(spVecBranchStatement);
  }
}




AST::BaseClasses::VariableInfoPtr Vectorizer::_GetAssigneeInfo(AST::Expressions::AssignmentOperatorPtr spAssignment)
{
  if (! spAssignment)
  {
    throw InternalErrors::NullPointerException("spAssignment");
  }

  // Step through to the leaf node of the left hand side of the assignment expression
  AST::BaseClasses::ExpressionPtr spValueExpression = spAssignment;
  while (spValueExpression->GetSubExpressionCount() != static_cast<IndexType>(0))
  {
    spValueExpression = spValueExpression->GetSubExpression(0);
    if (! spValueExpression)
    {
      throw InternalErrors::NullPointerException("spValueExpression");
    }
  }

  // Check if the assignee is an identifier
  if (spValueExpression->IsType<AST::Expressions::Identifier>())
  {
    // Fetch the variable info object which belongs to this identifier
    AST::Expressions::IdentifierPtr   spIdentifier    = spValueExpression->CastToType<AST::Expressions::Identifier>();
    AST::BaseClasses::VariableInfoPtr spVariableInfo  = spIdentifier->LookupVariableInfo();

    if (! spVariableInfo)
    {
      throw InternalErrorException(string("Could not find variable info for identifier: ") + spIdentifier->GetName());
    }

    return spVariableInfo;
  }
  else
  {
    throw InternalErrorException("Expected an identifier expression!");
  }
}



AST::FunctionDeclarationPtr Vectorizer::ConvertClangFunctionDecl(::clang::FunctionDecl *pFunctionDeclaration)
{
//VASTBuilder().Import(pFunctionDeclaration);

  return VASTBuilder().BuildFunctionDecl(pFunctionDeclaration);
}



void Vectorizer::DumpVASTNodeToXML(AST::BaseClasses::NodePtr spVastNode, string strXmlFilename)
{
  if (! spVastNode)
  {
    throw InternalErrors::NullPointerException("spVastNode");
  }

  std::ofstream XmlStream(strXmlFilename);

  XmlStream << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
  XmlStream << spVastNode->DumpToXML(0);

  XmlStream.flush();
  XmlStream.close();
}


void Vectorizer::VectorizeFunction(AST::FunctionDeclarationPtr spFunction)
{
  typedef map< AST::BaseClasses::VariableInfoPtr, std::list< AST::BaseClasses::ExpressionPtr > >   VariableDependencyMapType;

  if (! spFunction)
  {
    throw InternalErrors::NullPointerException("spFunction");
  }

  VariableDependencyMapType mapVariableDependencies;

  // Find all asignment expression (they express direct variable dependencies)
  {
    Transformations::FindAssignments AssignmentFinder;

    _RunVASTTransformation(spFunction, AssignmentFinder);

    for each (auto itAssignment in AssignmentFinder.lstAssignments)
    {
      mapVariableDependencies[ _GetAssigneeInfo(itAssignment) ].push_back( itAssignment->GetRHS() );
    }
  }

  // Find all loop internal assignments (the vectorization of these assignments also depends on the loop condition)
  {
    Transformations::FindLoopInternalAssignments LoopAssignmentFinder;

    _RunVASTTransformation(spFunction, LoopAssignmentFinder);

    for each (auto itCondAssignment in LoopAssignmentFinder.mapConditionalAssignments)
    {
      AST::BaseClasses::VariableInfoPtr spVariableInfo = _GetAssigneeInfo(itCondAssignment.first);

      for each (auto itCondition in itCondAssignment.second)
      {
        mapVariableDependencies[spVariableInfo].push_back(itCondition);
      }
    }
  }

  // Find all assignments inside all conditional branching statement (the vectorization of these assignments also depends on the branching condition cascade)
  {
    Transformations::FindBranchingInternalAssignments BranchAssignmentFinder;

    _RunVASTTransformation(spFunction, BranchAssignmentFinder);

    for each (auto itCondAssignment in BranchAssignmentFinder.mapConditionalAssignments)
    {
      AST::BaseClasses::VariableInfoPtr spVariableInfo = _GetAssigneeInfo(itCondAssignment.first);

      for each (auto itCondition in itCondAssignment.second)
      {
        mapVariableDependencies[spVariableInfo].push_back(itCondition);
      }
    }
  }


  // Continue to mark dependent variables as vectorized until nothing is changing anymore
  bool bChanged = true;
  while (bChanged)
  {
    bChanged = false;

    for each (auto itEntry in mapVariableDependencies)
    {
      if (! itEntry.first->GetVectorize())
      {
        for each (auto itExpression in itEntry.second)
        {
          if (itExpression->IsVectorized())
          {
            itEntry.first->SetVectorize(true);
            bChanged = true;
            break;
          }
        }
      }
    }
  }

}



// vim: set ts=2 sw=2 sts=2 et ai:

