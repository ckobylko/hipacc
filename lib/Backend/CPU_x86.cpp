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
#include <map>
#include <sstream>

using namespace clang::hipacc::Backend;
using namespace clang::hipacc;
using namespace clang;
using namespace std;


// Implementation of class CPU_x86::ClangASTHelper
unsigned int CPU_x86::ClangASTHelper::CountNumberOfReferences(::clang::Stmt *pStatement, const string &crstrReferenceName)
{
  if (pStatement == nullptr)
  {
    return 0;
  }
  else if (isa<::clang::DeclRefExpr>(pStatement))
  {
    ::clang::DeclRefExpr *pCurrentDeclRef = dyn_cast<::clang::DeclRefExpr>(pStatement);

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

::clang::ArraySubscriptExpr* CPU_x86::ClangASTHelper::CreateArraySubscriptExpression(::clang::DeclRefExpr *pArrayRef, ::clang::Expr *pIndexExpression, const ::clang::QualType &crReturnType, bool bIsLValue)
{
  ::clang::ExprValueKind  eValueKind = bIsLValue ? VK_LValue : VK_RValue;

  return new (GetASTContext()) ArraySubscriptExpr(pArrayRef, pIndexExpression, crReturnType, eValueKind, OK_Ordinary, SourceLocation());
}

::clang::BinaryOperator* CPU_x86::ClangASTHelper::CreateBinaryOperator(::clang::Expr *pLhs, ::clang::Expr *pRhs, ::clang::BinaryOperatorKind eOperatorKind, const ::clang::QualType &crReturnType)
{
  return ASTNode::createBinaryOperator(GetASTContext(), pLhs, pRhs, eOperatorKind, crReturnType);
}

::clang::BinaryOperator* CPU_x86::ClangASTHelper::CreateBinaryOperatorLessThan(::clang::Expr *pLhs, ::clang::Expr *pRhs)
{
  return CreateBinaryOperator(pLhs, pRhs, BO_LT, GetASTContext().BoolTy);
}

::clang::CompoundStmt* CPU_x86::ClangASTHelper::CreateCompoundStatement(::clang::Stmt *pStatement)
{
  StatementVectorType vecStatements;

  vecStatements.push_back(pStatement);

  return CreateCompoundStatement(vecStatements);
}

::clang::CompoundStmt* CPU_x86::ClangASTHelper::CreateCompoundStatement(const StatementVectorType &crvecStatements)
{
  return ASTNode::createCompoundStmt(_rCtx, crvecStatements);
}

::clang::DeclRefExpr* CPU_x86::ClangASTHelper::CreateDeclarationReferenceExpression(::clang::ValueDecl *pValueDecl)
{
  return ASTNode::createDeclRefExpr(GetASTContext(), pValueDecl);
}

::clang::DeclStmt* CPU_x86::ClangASTHelper::CreateDeclarationStatement(::clang::DeclRefExpr *pDeclRef)
{
  return CreateDeclarationStatement(pDeclRef->getDecl());
}

::clang::DeclStmt* CPU_x86::ClangASTHelper::CreateDeclarationStatement(::clang::ValueDecl *pValueDecl)
{
  return ASTNode::createDeclStmt(GetASTContext(), pValueDecl);
}

::clang::ImplicitCastExpr* CPU_x86::ClangASTHelper::CreateImplicitCastExpression(::clang::Expr *pOperandExpression, const ::clang::QualType &crReturnType, ::clang::CastKind eCastKind, bool bIsLValue)
{
  ::clang::ExprValueKind  eValueKind = bIsLValue ? VK_LValue : VK_RValue;

  return ASTNode::createImplicitCastExpr(GetASTContext(), crReturnType, eCastKind, pOperandExpression, nullptr, eValueKind);
}

::clang::IntegerLiteral* CPU_x86::ClangASTHelper::CreateIntegerLiteral(int32_t iValue)
{
  return ASTNode::createIntegerLiteral(GetASTContext(), iValue);
}

::clang::ParenExpr* CPU_x86::ClangASTHelper::CreateParenthesisExpression(::clang::Expr *pSubExpression)
{
  return ASTNode::createParenExpr(GetASTContext(), pSubExpression);
}

::clang::UnaryOperator* CPU_x86::ClangASTHelper::CreatePostIncrementOperator(::clang::DeclRefExpr *pDeclRef)
{
  return ASTNode::createUnaryOperator(_rCtx, pDeclRef, ::clang::UO_PostInc, pDeclRef->getType());
}

::clang::VarDecl*  CPU_x86::ClangASTHelper::CreateVariableDeclaration(::clang::FunctionDecl *pParentFunction, const string &crstrVariableName, const ::clang::QualType &crVariableType, ::clang::Expr *pInitExpression)
{
  ::clang::DeclContext *pDeclContext = ::clang::FunctionDecl::castToDeclContext(pParentFunction);

  ::clang::VarDecl *pVarDecl = ASTNode::createVarDecl(GetASTContext(), pDeclContext, crstrVariableName, crVariableType, pInitExpression);
  pDeclContext->addDecl(pVarDecl);

  return pVarDecl;
}

::clang::DeclRefExpr* CPU_x86::ClangASTHelper::FindDeclaration(::clang::FunctionDecl *pFunction, const string &crstrDeclName)
{
  ::clang::DeclContext *pDeclContext = ::clang::FunctionDecl::castToDeclContext(pFunction);

  for (auto itDecl = pDeclContext->decls_begin(); itDecl != pDeclContext->decls_end(); itDecl++)
  {
    ::clang::Decl *pDecl = *itDecl;

    if ((pDecl == nullptr) || (!isa<ValueDecl>(pDecl)))
    {
      continue;
    }

    ::clang::ValueDecl* pValueDecl = dyn_cast<ValueDecl>(pDecl);

    if (pValueDecl->getNameAsString() == crstrDeclName)
    {
      return CreateDeclarationReferenceExpression(pValueDecl);
    }
  }

  return nullptr;
}

bool CPU_x86::ClangASTHelper::IsSingleBranchStatement(::clang::Stmt *pStatement)
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

void CPU_x86::ClangASTHelper::ReplaceDeclarationReferences(::clang::Stmt* pStatement, const string &crstrDeclRefName, ::clang::ValueDecl *pNewDecl)
{
  if (pStatement == nullptr)
  {
    return;
  }
  else if (isa<::clang::DeclRefExpr>(pStatement))
  {
    ::clang::DeclRefExpr *pDeclRef = dyn_cast<::clang::DeclRefExpr>(pStatement);

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
bool CPU_x86::CodeGenerator::KernelSubFunctionBuilder::_IsVariableUsed(const string &crstrVariableName, ::clang::Stmt *pStatement)
{
  if (pStatement == nullptr)
  {
    // Break for invalid statements
    return false;
  }
  else if (isa<DeclRefExpr>(pStatement))
  {
    // Found a declaration reference expression => Check if it refers to specified variable
    if (dyn_cast<DeclRefExpr>(pStatement)->getNameInfo().getAsString() == crstrVariableName)
    {
      return true;
    }
  }
  else
  {
    // Check all child statements for references to the specified variable
    for (auto itChild = pStatement->child_begin(); itChild != pStatement->child_end(); itChild++)
    {
      if (_IsVariableUsed(crstrVariableName, *itChild))
      {
        return true;
      }
    }

  }

  return false;
}

void CPU_x86::CodeGenerator::KernelSubFunctionBuilder::AddCallParameter(::clang::DeclRefExpr *pCallParam)
{
  _vecArgumentTypes.push_back(pCallParam->getDecl()->getType());
  _vecArgumentNames.push_back(pCallParam->getDecl()->getNameAsString());
  _vecCallParams.push_back(pCallParam);
}

void CPU_x86::CodeGenerator::KernelSubFunctionBuilder::ImportUsedParameters(::clang::FunctionDecl *pRootFunctionDecl, ::clang::Stmt *pSubFunctionBody)
{
  for (unsigned int i = 0; i < pRootFunctionDecl->getNumParams(); ++i)
  {
    ParmVarDecl *pParamVarDecl = pRootFunctionDecl->getParamDecl(i);

    if (_IsVariableUsed(pParamVarDecl->getNameAsString(), pSubFunctionBody))
    {
      AddCallParameter(ASTNode::createDeclRefExpr(_rASTContext, pParamVarDecl));
    }
  }
}

CPU_x86::CodeGenerator::KernelSubFunctionBuilder::DeclCallPairType  CPU_x86::CodeGenerator::KernelSubFunctionBuilder::CreateFuntionDeclarationAndCall(string strFunctionName, const ::clang::QualType &crResultType)
{
  DeclCallPairType pairDeclAndCall;

  pairDeclAndCall.first  = ASTNode::createFunctionDecl( _rASTContext, _rASTContext.getTranslationUnitDecl(), strFunctionName, crResultType,
                                                        ArrayRef< ::clang::QualType >(_vecArgumentTypes.data(), _vecArgumentTypes.size()),
                                                        ArrayRef< string >(_vecArgumentNames.data(), _vecArgumentNames.size()) );

  pairDeclAndCall.second = ASTNode::createFunctionCall( _rASTContext, pairDeclAndCall.first, _vecCallParams );

  return pairDeclAndCall;
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

void CPU_x86::CodeGenerator::ImageAccessTranslator::TranslateImageAccesses()
{
  ::clang::FunctionDecl *pKernelFunction = _rHipaccHelper.GetKernelFunction();

  // Parse through all kernel images
  for (size_t i = 0; i < pKernelFunction->getNumParams(); ++i)
  {
    ::clang::ParmVarDecl  *pParamDecl = pKernelFunction->getParamDecl(i);
    string                strParamName = pParamDecl->getNameAsString();

    // Skip all kernel function parameters, which are unused or do not refer to an HIPAcc image
    if ((!_rHipaccHelper.IsParamUsed(strParamName)) || (_rHipaccHelper.GetImageFromMapping(strParamName) == nullptr))
    {
      continue;
    }

    // Find all access to the current image
    list< ::clang::ArraySubscriptExpr* >  lstImageAccesses = _FindImageAccesses(strParamName, pKernelFunction->getBody());

    // Linearize all found image accesses
    for each (auto itImageAccess in lstImageAccesses)
    {
      _LinearizeImageAccess(strParamName, itImageAccess);
    }
  }
}



// Implementation of class CPU_x86::CodeGenerator
CPU_x86::CodeGenerator::CodeGenerator(::clang::hipacc::CompilerOptions *pCompilerOptions) : BaseType(pCompilerOptions, Descriptor())
{
  _InitSwitch< KnownSwitches::VectorizeKernel >(CompilerSwitchTypeEnum::VectorizeKernel);

  _bVectorizeKernel = false;
}

size_t CPU_x86::CodeGenerator::_HandleSwitch(CompilerSwitchTypeEnum eSwitch, CommonDefines::ArgumentVectorType &rvecArguments, size_t szCurrentIndex)
{
  string  strCurrentSwitch  = rvecArguments[szCurrentIndex];
  size_t  szReturnIndex     = szCurrentIndex;

  switch (eSwitch)
  {
  case CompilerSwitchTypeEnum::VectorizeKernel:
    _bVectorizeKernel = true;
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


string CPU_x86::CodeGenerator::_GetImageDeclarationString(string strName, HipaccMemory *pHipaccMemoryObject, bool bConstPointer, bool bTranslate)
{
  stringstream FormatStream;

  if (bConstPointer)
  {
    FormatStream << "const ";
  }

  FormatStream << pHipaccMemoryObject->getTypeStr() << " ";
  
  if (bTranslate)
  {
    FormatStream << "*" << strName;
  }
  else
  {
    FormatStream << strName;
    FormatStream << "[" << pHipaccMemoryObject->getSizeYStr() << "]";
    FormatStream << "[" << pHipaccMemoryObject->getSizeXStr() << "]";
  }

  return FormatStream.str();
}


string CPU_x86::CodeGenerator::_FormatFunctionHeader(FunctionDecl *pFunctionDecl, HipaccHelper &rHipaccHelper, bool bCheckUsage, bool bTranslateImageDecls)
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
        vecParamStrings.push_back( _GetImageDeclarationString(pMask->getName(), pMask, true, false) );
      }
    }
    else if (HipaccAccessor *pAccessor = rHipaccHelper.GetImageFromMapping(strName))  // check if we have an Accessor
    {
      vecParamStrings.push_back( _GetImageDeclarationString(strName, pAccessor->getImage(), rHipaccHelper.GetImageAccess(strName) == READ_ONLY, bTranslateImageDecls) );
    }
    else                                                                              // normal arguments
    {
      string strParamBuffer;
      llvm::raw_string_ostream ParamStream(strParamBuffer);

      QualType T = pParamDecl->getType();
      T.removeLocalConst();
      T.removeLocalRestrict();

      T.getAsStringInternal(strName, GetPrintingPolicy());
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


bool CPU_x86::CodeGenerator::PrintKernelFunction(FunctionDecl *pKernelFunction, HipaccKernel *pKernel, llvm::raw_ostream &rOutputStream)
{
  HipaccHelper          hipaccHelper(pKernelFunction, pKernel);
  ImageAccessTranslator ImgAccessTranslator(hipaccHelper);

  // Add the iteration space loops
  {
    ClangASTHelper  ASTHelper(pKernelFunction->getASTContext());

    DeclRefExpr *gid_x_ref = ASTHelper.FindDeclaration(pKernelFunction, HipaccHelper::GlobalIdX());
    DeclRefExpr *gid_y_ref = ASTHelper.FindDeclaration(pKernelFunction, HipaccHelper::GlobalIdY());


    // If vectorization is enabled, split the kernel function into the "iteration space"-part and the "pixel-wise processing"-part
    if (_bVectorizeKernel)
    {
      ImgAccessTranslator.TranslateImageAccesses();

      // Push loop body to own function
      ::clang::Stmt *pKernelBody = pKernelFunction->getBody();

      KernelSubFunctionBuilder SubFuncBuilder(ASTHelper.GetASTContext());

      SubFuncBuilder.ImportUsedParameters(pKernelFunction, pKernelBody);
      SubFuncBuilder.AddCallParameter(gid_y_ref);
      SubFuncBuilder.AddCallParameter(gid_x_ref);

      KernelSubFunctionBuilder::DeclCallPairType  DeclCallPair = SubFuncBuilder.CreateFuntionDeclarationAndCall(pKernelFunction->getNameAsString() + string("_Scalar"), pKernelFunction->getResultType());
      DeclCallPair.first->setBody(pKernelBody);


      // Create function call reference for kernel loop body
      pKernelFunction->setBody(ASTHelper.CreateCompoundStatement(DeclCallPair.second));


      // Print the new kernel body sub-function
      rOutputStream << "inline " << _FormatFunctionHeader(DeclCallPair.first, hipaccHelper, false, true);
      DeclCallPair.first->getBody()->printPretty(rOutputStream, 0, GetPrintingPolicy(), 0);
      rOutputStream << "\n\n";
    }


    // Create the iteration space
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
      }

      // Create the horizontal iteration space loop and push it to the vertical loop body
      vecInnerLoopBody.push_back(pKernelFunction->getBody());

      ForStmt *pInnerLoop = _CreateIterationSpaceLoop(ASTHelper, gid_x_ref, hipaccHelper.GetIterationSpaceLimitX(), ASTHelper.CreateCompoundStatement(vecInnerLoopBody));
      vecOuterLoopBody.push_back( pInnerLoop );
    }

    // Create the vertical iteration space loop and set it as kernel function body
    ForStmt *pOuterLoop = _CreateIterationSpaceLoop(ASTHelper, gid_y_ref, hipaccHelper.GetIterationSpaceLimitY(), ASTHelper.CreateCompoundStatement(vecOuterLoopBody));
    pKernelFunction->setBody( ASTHelper.CreateCompoundStatement(pOuterLoop) );
  }


  rOutputStream << _FormatFunctionHeader(pKernelFunction, hipaccHelper, true, false);

  // print kernel body
  pKernelFunction->getBody()->printPretty(rOutputStream, 0, GetPrintingPolicy(), 0);

  return true;
}


// vim: set ts=2 sw=2 sts=2 et ai:

