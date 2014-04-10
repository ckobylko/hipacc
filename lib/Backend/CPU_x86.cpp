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
#include <sstream>

using namespace clang::hipacc::Backend;
using namespace clang::hipacc;
using namespace clang;
using namespace std;

CPU_x86::CodeGenerator::Descriptor::Descriptor()
{
  SetTargetCode(::clang::hipacc::TARGET_C);
  SetName("CPU-x86");
  SetEmissionKey("cpu");
  SetDescription("Emit C++ code for x86-CPUs");
}

CPU_x86::CodeGenerator::CodeGenerator(::clang::hipacc::CompilerOptions *pCompilerOptions) : BaseType(pCompilerOptions, Descriptor())
{
}

size_t CPU_x86::CodeGenerator::_HandleSwitch(CompilerSwitchTypeEnum eSwitch, CommonDefines::ArgumentVectorType &rvecArguments, size_t szCurrentIndex)
{
  string  strCurrentSwitch  = rvecArguments[szCurrentIndex];
  size_t  szReturnIndex     = szCurrentIndex;

  switch (eSwitch)
  {
  default:  throw InternalErrors::UnhandledSwitchException(strCurrentSwitch, GetName());
  }

  return szReturnIndex;
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


bool CPU_x86::CodeGenerator::PrintKernelFunction(FunctionDecl *pKernelFunction, HipaccKernelClass *pKernelClass, HipaccKernel *pKernel, llvm::raw_ostream &rOutputStream)
{
  // write kernel name and qualifiers
  rOutputStream << "void " << pKernel->getKernelName() << "(";

  // write kernel parameters
  size_t comma = 0;
  for (size_t i = 0, e = pKernelFunction->getNumParams(); i != e; ++i)
  {
    std::string Name(pKernelFunction->getParamDecl(i)->getNameAsString());
    FieldDecl *FD = pKernel->getDeviceArgFields()[i];

    if (!pKernel->getUsed(Name))
    {
      continue;
    }

    QualType T = pKernelFunction->getParamDecl(i)->getType();
    T.removeLocalConst();
    T.removeLocalRestrict();

    // check if we have a Mask or Domain
    HipaccMask *Mask = pKernel->getMaskFromMapping(FD);
    if (Mask)
    {
      if (!Mask->isConstant())
      {
        if (comma++) rOutputStream << ", ";

        rOutputStream << _GetImageDeclarationString(Mask->getName(), Mask, true);
      }

      continue;
    }

    // check if we have an Accessor
    HipaccAccessor *Acc = pKernel->getImgFromMapping(FD);
    MemoryAccess memAcc = UNDEFINED;
    if (i == 0)
    { // first argument is always the output image
      Acc = pKernel->getIterationSpace()->getAccessor();
      memAcc = WRITE_ONLY;
    }
    else if (Acc)
    {
      memAcc = pKernelClass->getImgAccess(FD);
    }

    if (Acc)
    {
      if (comma++) rOutputStream << ", ";

      rOutputStream << _GetImageDeclarationString(Name, Acc->getImage(), memAcc == READ_ONLY);

      continue;
    }

    // normal arguments
    if (comma++) rOutputStream << ", ";
    T.getAsStringInternal(Name, GetPrintingPolicy());
    rOutputStream << Name;

    // default arguments ...
    if (Expr *Init = pKernelFunction->getParamDecl(i)->getInit())
    {
      CXXConstructExpr *CCE = dyn_cast<CXXConstructExpr>(Init);
      if (!CCE || CCE->getConstructor()->isCopyConstructor()) {
        rOutputStream << " = ";
      }
      Init->printPretty(rOutputStream, 0, GetPrintingPolicy(), 0);
    }
  }
  rOutputStream << ") ";


  // Add the iteration space loops
  {
    ::clang::ASTContext &Ctx = pKernelFunction->getASTContext();

    ::clang::Expr *upper_x = pKernel->getIterationSpace()->getAccessor()->getWidthDecl();
    ::clang::Expr *upper_y = pKernel->getIterationSpace()->getAccessor()->getHeightDecl();

    if (pKernel->getIterationSpace()->getAccessor()->getOffsetXDecl())
    {
      upper_x = ASTNode::createBinaryOperator(Ctx, upper_x,
        pKernel->getIterationSpace()->getAccessor()->getOffsetXDecl(), BO_Add,
        Ctx.IntTy);
    }

    if (pKernel->getIterationSpace()->getAccessor()->getOffsetYDecl())
    {
      upper_y = ASTNode::createBinaryOperator(Ctx, upper_y,
        pKernel->getIterationSpace()->getAccessor()->getOffsetYDecl(), BO_Add,
        Ctx.IntTy);
    }

    DeclContext *DC = FunctionDecl::castToDeclContext(pKernelFunction);
    DeclStmt    *gid_x_stmt = nullptr, *gid_y_stmt = nullptr;
    DeclRefExpr *gid_x_ref = nullptr,  *gid_y_ref = nullptr;

    for (auto itDecl = DC->decls_begin(); itDecl != DC->decls_end(); itDecl++)
    {
      ::clang::Decl *pDecl = *itDecl;

      if (pDecl == nullptr)
      {
        continue;
      }
      else if (!isa<ValueDecl>(pDecl))
      {
        continue;
      }

      std::string strDeclName = dyn_cast<ValueDecl>(pDecl)->getNameAsString();

      if (strDeclName == "gid_x")
      {
        gid_x_stmt  = ASTNode::createDeclStmt(Ctx, pDecl);
        gid_x_ref = ASTNode::createDeclRefExpr(Ctx, dyn_cast<ValueDecl>(pDecl));
      }
      else if (strDeclName == "gid_y")
      {
        gid_y_stmt  = ASTNode::createDeclStmt(Ctx, pDecl);
        gid_y_ref = ASTNode::createDeclRefExpr(Ctx, dyn_cast<ValueDecl>(pDecl));
      }
    }

    ForStmt *innerLoop = ASTNode::createForStmt(Ctx, gid_x_stmt, ASTNode::createBinaryOperator(Ctx,
      gid_x_ref, upper_x, BO_LT, Ctx.BoolTy),
      ASTNode::createUnaryOperator(Ctx, gid_x_ref, ::clang::UO_PostInc,
      gid_x_ref->getType()), pKernelFunction->getBody());


    llvm::SmallVector< ::clang::Stmt*, 16 > vecInnerLoopBody;
    vecInnerLoopBody.push_back(innerLoop);

    ForStmt *outerLoop = ASTNode::createForStmt(Ctx, gid_y_stmt, ASTNode::createBinaryOperator(Ctx,
      gid_y_ref, upper_y, BO_LT, Ctx.BoolTy),
      ASTNode::createUnaryOperator(Ctx, gid_y_ref, ::clang::UO_PostInc,
      gid_y_ref->getType()), ASTNode::createCompoundStmt(Ctx, vecInnerLoopBody));


    llvm::SmallVector< ::clang::Stmt*, 16 > vecNewBody;
    vecNewBody.push_back( outerLoop );

    pKernelFunction->setBody(ASTNode::createCompoundStmt(Ctx, vecNewBody));
  }


  // print kernel body
  pKernelFunction->getBody()->printPretty(rOutputStream, 0, GetPrintingPolicy(), 0);

  return true;
}


// vim: set ts=2 sw=2 sts=2 et ai:

