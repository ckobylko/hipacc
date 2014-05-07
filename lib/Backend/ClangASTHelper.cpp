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

//===--- ClangASTHelper.cpp - Implements helper class for easy clang AST handling. ---===//
//
// This file implements a helper class which contains a few methods for easy clang AST handling.
//
//===---------------------------------------------------------------------------------===//

#include "hipacc/Backend/ClangASTHelper.h"

using namespace clang::hipacc::Backend;
using namespace clang;
using namespace std;


unsigned int ClangASTHelper::CountNumberOfReferences(Stmt *pStatement, const string &crstrReferenceName)
{
  if (pStatement == nullptr)
  {
    return 0;
  }
  else if (isa<::clang::DeclRefExpr>(pStatement))
  {
    DeclRefExpr *pCurrentDeclRef = dyn_cast<DeclRefExpr>(pStatement);

    if (pCurrentDeclRef->getNameInfo().getAsString() == crstrReferenceName)
    {
      return 1;
    }
    else
    {
      return 0;
    }
  }
  else
  {
    unsigned int uiChildRefCount = 0;

    for (auto itChild = pStatement->child_begin(); itChild != pStatement->child_end(); itChild++)
    {
      uiChildRefCount += CountNumberOfReferences(*itChild, crstrReferenceName);
    }

    return uiChildRefCount;
  }
}


ArraySubscriptExpr* ClangASTHelper::CreateArraySubscriptExpression(Expr *pArrayRef, Expr *pIndexExpression, const QualType &crReturnType, bool bIsLValue)
{
  ExprValueKind  eValueKind = bIsLValue ? VK_LValue : VK_RValue;

  return new (GetASTContext()) ArraySubscriptExpr(pArrayRef, pIndexExpression, crReturnType, eValueKind, OK_Ordinary, SourceLocation());
}

BinaryOperator* ClangASTHelper::CreateBinaryOperator(Expr *pLhs, Expr *pRhs, BinaryOperatorKind eOperatorKind, const QualType &crReturnType)
{
  return ASTNode::createBinaryOperator(GetASTContext(), pLhs, pRhs, eOperatorKind, crReturnType);
}

BinaryOperator* ClangASTHelper::CreateBinaryOperatorLessThan(Expr *pLhs, Expr *pRhs)
{
  return CreateBinaryOperator(pLhs, pRhs, BO_LT, GetASTContext().BoolTy);
}

CXXBoolLiteralExpr* ClangASTHelper::CreateBoolLiteral(bool bValue)
{
  return new (GetASTContext()) CXXBoolLiteralExpr(bValue, GetASTContext().BoolTy, SourceLocation());
}

CompoundStmt* ClangASTHelper::CreateCompoundStatement(Stmt *pStatement)
{
  StatementVectorType vecStatements;

  vecStatements.push_back(pStatement);

  return CreateCompoundStatement(vecStatements);
}

CompoundStmt* ClangASTHelper::CreateCompoundStatement(const StatementVectorType &crvecStatements)
{
  return ASTNode::createCompoundStmt(GetASTContext(), crvecStatements);
}

DeclRefExpr* ClangASTHelper::CreateDeclarationReferenceExpression(ValueDecl *pValueDecl)
{
  return ASTNode::createDeclRefExpr(GetASTContext(), pValueDecl);
}

DeclStmt* ClangASTHelper::CreateDeclarationStatement(DeclRefExpr *pDeclRef)
{
  return CreateDeclarationStatement(pDeclRef->getDecl());
}

DeclStmt* ClangASTHelper::CreateDeclarationStatement(ValueDecl *pValueDecl)
{
  return ASTNode::createDeclStmt(GetASTContext(), pValueDecl);
}

CallExpr* ClangASTHelper::CreateFunctionCall(FunctionDecl *pFunctionDecl, const ExpressionVectorType &crvecArguments)
{
  return ASTNode::createFunctionCall(GetASTContext(), pFunctionDecl, crvecArguments);
}

FunctionDecl* ClangASTHelper::CreateFunctionDeclaration(string strFunctionName, const QualType &crReturnType, const StringVectorType &crvecArgumentNames, const QualTypeVectorType &crvecArgumentTypes)
{
  return ASTNode::createFunctionDecl( GetASTContext(), GetASTContext().getTranslationUnitDecl(), strFunctionName, crReturnType,
                                      ArrayRef< ::clang::QualType >(crvecArgumentTypes), ArrayRef< string >(crvecArgumentNames) );
}

ImplicitCastExpr* ClangASTHelper::CreateImplicitCastExpression(Expr *pOperandExpression, const QualType &crReturnType, CastKind eCastKind, bool bIsLValue)
{
  ExprValueKind  eValueKind = bIsLValue ? VK_LValue : VK_RValue;

  return ASTNode::createImplicitCastExpr(GetASTContext(), crReturnType, eCastKind, pOperandExpression, nullptr, eValueKind);
}

InitListExpr* ClangASTHelper::CreateInitListExpression(const ExpressionVectorType &crvecExpressions)
{
  return new (GetASTContext()) InitListExpr( GetASTContext(), SourceLocation(), ArrayRef< Expr* >(crvecExpressions), SourceLocation() );
}

ParenExpr* ClangASTHelper::CreateParenthesisExpression(Expr *pSubExpression)
{
  return ASTNode::createParenExpr(GetASTContext(), pSubExpression);
}

UnaryOperator* ClangASTHelper::CreatePostIncrementOperator(DeclRefExpr *pDeclRef)
{
  return CreateUnaryOperator(pDeclRef, UO_PostInc, pDeclRef->getType());
}

CXXReinterpretCastExpr* ClangASTHelper::CreateReinterpretCast(Expr *pOperandExpression, const QualType &crReturnType, CastKind eCastKind, bool bIsLValue)
{
  ExprValueKind  eValueKind = true ? VK_LValue : VK_RValue;

  CXXCastPath CastPath;

  return CXXReinterpretCastExpr::Create(GetASTContext(), crReturnType, eValueKind, eCastKind, pOperandExpression, &CastPath, GetASTContext().getTrivialTypeSourceInfo(crReturnType), SourceLocation(), SourceLocation(), SourceRange());
}

CXXStaticCastExpr* ClangASTHelper::CreateStaticCast(Expr *pOperandExpression, const QualType &crReturnType, CastKind eCastKind, bool bIsLValue)
{
  ExprValueKind  eValueKind = true ? VK_LValue : VK_RValue;
  
  CXXCastPath CastPath;

  return CXXStaticCastExpr::Create(GetASTContext(), crReturnType, eValueKind, eCastKind, pOperandExpression, &CastPath, GetASTContext().getTrivialTypeSourceInfo(crReturnType), SourceLocation(), SourceLocation(), SourceRange());
}

UnaryOperator* ClangASTHelper::CreateUnaryOperator(Expr *pSubExpression, UnaryOperatorKind eOperatorKind, const QualType &crResultType)
{
  return ASTNode::createUnaryOperator(GetASTContext(), pSubExpression, eOperatorKind, crResultType);
}

VarDecl* ClangASTHelper::CreateVariableDeclaration(DeclContext *pDeclContext, const string &crstrVariableName, const QualType &crVariableType, Expr *pInitExpression)
{
  VarDecl *pVarDecl = ASTNode::createVarDecl(GetASTContext(), pDeclContext, crstrVariableName, crVariableType, pInitExpression);
  pDeclContext->addDecl(pVarDecl);

  return pVarDecl;
}

VarDecl* ClangASTHelper::CreateVariableDeclaration(FunctionDecl *pParentFunction, const string &crstrVariableName, const QualType &crVariableType, Expr *pInitExpression)
{
  return CreateVariableDeclaration(FunctionDecl::castToDeclContext(pParentFunction), crstrVariableName, crVariableType, pInitExpression);
}


QualType ClangASTHelper::GetConstantArrayType(const QualType &crElementType, const size_t cszDimension)
{
  return GetASTContext().getConstantArrayType( crElementType, llvm::APInt(32, static_cast< uint64_t >(cszDimension), false), ArrayType::Normal, 0 );
}


DeclRefExpr* ClangASTHelper::FindDeclaration(FunctionDecl *pFunction, const string &crstrDeclName)
{
  DeclContext *pDeclContext = FunctionDecl::castToDeclContext(pFunction);

  for (auto itDecl = pDeclContext->decls_begin(); itDecl != pDeclContext->decls_end(); itDecl++)
  {
    Decl *pDecl = *itDecl;

    if ((pDecl == nullptr) || (!isa<ValueDecl>(pDecl)))
    {
      continue;
    }

    ValueDecl* pValueDecl = dyn_cast<ValueDecl>(pDecl);

    if (pValueDecl->getNameAsString() == crstrDeclName)
    {
      return CreateDeclarationReferenceExpression(pValueDecl);
    }
  }

  return nullptr;
}

bool ClangASTHelper::IsSingleBranchStatement(Stmt *pStatement)
{
  if (pStatement == nullptr)                                      // Empty statements have no children
  {
    return true;
  }
  else if (pStatement->child_begin() == pStatement->child_end())  // No children
  {
    return true;
  }
  else                                                            // Statement has children
  {
    // Check if more than one child is present
    auto itChild = pStatement->child_begin();
    itChild++;

    if (itChild == pStatement->child_end())   // Only one child => Check if the child is a single branch
    {
      return IsSingleBranchStatement(*(pStatement->child_begin()));
    }
    else                                      // More than one child => Statement is not a single branch statement
    {
      return false;
    }
  }
}

void ClangASTHelper::ReplaceDeclarationReferences(Stmt* pStatement, const string &crstrDeclRefName, ValueDecl *pNewDecl)
{
  if (pStatement == nullptr)
  {
    return;
  }
  else if (isa<::clang::DeclRefExpr>(pStatement))
  {
    DeclRefExpr *pDeclRef = dyn_cast<DeclRefExpr>(pStatement);

    if (pDeclRef->getNameInfo().getName().getAsString() == crstrDeclRefName)
    {
      pDeclRef->setDecl(pNewDecl);
    }
  }
  else
  {
    for (auto itChild = pStatement->child_begin(); itChild != pStatement->child_end(); itChild++)
    {
      ReplaceDeclarationReferences(*itChild, crstrDeclRefName, pNewDecl);
    }
  }
}


// vim: set ts=2 sw=2 sts=2 et ai:

