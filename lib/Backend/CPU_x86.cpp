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
      return ASTNode::createDeclRefExpr(_rCtx, pValueDecl);
    }
  }

  return nullptr;
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


string CPU_x86::CodeGenerator::_FormatFunctionHeader(FunctionDecl *pFunctionDecl, HipaccHelper &rHipaccHelper, bool bCheckUsage)
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
      vecParamStrings.push_back(_GetImageDeclarationString(strName, pAccessor->getImage(), rHipaccHelper.GetImageAccess(strName) == READ_ONLY));
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
  HipaccHelper    hipaccHelper(pKernelFunction, pKernel);

  // Add the iteration space loops
  {
    ClangASTHelper  ASTHelper(pKernelFunction->getASTContext());

    DeclRefExpr *gid_x_ref = ASTHelper.FindDeclaration(pKernelFunction, HipaccHelper::GlobalIdX());
    DeclRefExpr *gid_y_ref = ASTHelper.FindDeclaration(pKernelFunction, HipaccHelper::GlobalIdY());


    // If vectorization is enabled, split the kernel function into the "iteration space"-part and the "pixel-wise processing"-part
    if (_bVectorizeKernel)
    {
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
      rOutputStream << "inline " << _FormatFunctionHeader(DeclCallPair.first, hipaccHelper, false);
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

          // Fetch the required image access and pointer types
          QualType qtArrayAccessType, qtImagePointerType;
          {
            qtArrayAccessType       = pParamDecl->getType()->getPointeeType();
            QualType qtElementType  = qtArrayAccessType->getAsArrayTypeUnsafe()->getElementType();

            if (hipaccHelper.GetImageAccess(strParamName) == READ_ONLY)
            {
              qtElementType.addConst();
            }

            qtImagePointerType = ASTHelper.GetPointerType(qtElementType);
          }


          // Create the declaration of the currently processed image line, e.g. "float *Output_CurrentLine = Output[gid_y];"
          ::clang::DeclRefExpr *pImageLineDeclRef = nullptr;
          {
            ::clang::DeclRefExpr        *pImageDeclRef  = ASTHelper.CreateDeclarationReferenceExpression(pParamDecl);
            ::clang::ArraySubscriptExpr *pArrayAccess   = ASTHelper.CreateArraySubscriptExpression(pImageDeclRef, gid_y_ref, qtArrayAccessType, false);
            ::clang::ImplicitCastExpr   *pCastToPointer = ASTHelper.CreateImplicitCastExpression(pArrayAccess, qtImagePointerType, CK_ArrayToPointerDecay, false);
            ::clang::VarDecl            *pImageLineDecl = ASTHelper.CreateVariableDeclaration(pKernelFunction, strParamName + string("_CurrentLine"), qtImagePointerType, pCastToPointer);

            pImageLineDeclRef = ASTHelper.CreateDeclarationReferenceExpression(pImageLineDecl);
          }

          vecOuterLoopBody.push_back( ASTHelper.CreateDeclarationStatement(pImageLineDeclRef) );


          // Create the declaration of the currently processed image pixel, e.g. "float *Output_CurrentPos = Output_CurrentLine + gid_x;"
          ::clang::BinaryOperator *pPointerAddition = ASTHelper.CreateBinaryOperator(pImageLineDeclRef, gid_x_ref, BO_Add, qtImagePointerType);
          ::clang::VarDecl        *pImagePosDecl    = ASTHelper.CreateVariableDeclaration(pKernelFunction, strParamName + string("_CurrentPos"), qtImagePointerType, pPointerAddition);

          vecInnerLoopBody.push_back( ASTHelper.CreateDeclarationStatement(pImagePosDecl) );
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


  rOutputStream << _FormatFunctionHeader(pKernelFunction, hipaccHelper, true);

  // print kernel body
  pKernelFunction->getBody()->printPretty(rOutputStream, 0, GetPrintingPolicy(), 0);

  return true;
}


// vim: set ts=2 sw=2 sts=2 et ai:

