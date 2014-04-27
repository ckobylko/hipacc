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
#include <fstream>
#include <string>
using namespace clang::hipacc::Backend::Vectorization;
using namespace std;


AST::BaseClasses::ExpressionPtr Vectorizer::VASTBuilder::_BuildExpression(::clang::Expr *pExpression)
{
  return nullptr;
}

AST::BaseClasses::VariableInfoPtr Vectorizer::VASTBuilder::_BuildVariableInfo(::clang::VarDecl *pVarDecl)
{
  AST::BaseClasses::VariableInfoPtr spVariableInfo = std::make_shared< AST::BaseClasses::VariableInfo >();

  spVariableInfo->SetName(pVarDecl->getNameAsString());

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

    AST::BaseClasses::NodePtr spChild(nullptr);

    if (isa<::clang::CompoundStmt>(pChildStatement))
    {
      ::clang::CompoundStmt *pCurrentCompound = dyn_cast<::clang::CompoundStmt>(pChildStatement);

      AST::ScopePtr spChildScope = AST::CreateNode<AST::Scope>();
      spChild = spChildScope;

      _ConvertScope(spChildScope, pCurrentCompound);
    }
    else if (isa<::clang::DeclStmt>(pChildStatement))
    {
      ::clang::DeclStmt     *pDeclStatement = dyn_cast<::clang::DeclStmt>(pChildStatement);
      ::clang::DeclGroupRef  DeclGroup      = pDeclStatement->getDeclGroup();

      for (auto itDecl = DeclGroup.begin(); itDecl != DeclGroup.end(); itDecl++)
      {
        ::clang::Decl *pDecl = *itDecl;
        if (pDecl == nullptr)
        {
          continue;
        }
        else if (!isa<::clang::VarDecl>(pDecl))
        {
          continue;
        }

        ::clang::VarDecl  *pVarDecl   = dyn_cast<::clang::VarDecl>(pDecl);

        AST::BaseClasses::VariableInfoPtr spVariableInfo = _BuildVariableInfo(pVarDecl);
        spScope->AddVariable(spVariableInfo);


        ::clang::Expr     *pInitExpr  = pVarDecl->getInit();

        if (pInitExpr == nullptr)
        {
          continue;
        }

        AST::Expressions::AssignmentOperatorPtr spAssignment = AST::CreateNode< AST::Expressions::AssignmentOperator >();

        AST::Expressions::IdentifierPtr spVariable = AST::CreateNode< AST::Expressions::Identifier >();
        spVariable->SetName(pVarDecl->getNameAsString());

        spAssignment->SetLHS(spVariable);
        spAssignment->SetRHS(_BuildExpression(pInitExpr));

        spScope->AddChild(spAssignment);
      }

      continue;
    }
    else if (isa<::clang::Expr>(pChildStatement))
    {
      ::clang::Expr *pExpression = dyn_cast<::clang::Expr>(pChildStatement);

//      spChild = _BuildExpression(pExpression);
continue;  // TODO: Remove this
    }
    else
    {
//      throw RuntimeErrorException(string("Unknown statement type \"") + string(pChildStatement->getStmtClassName()) + string("\" !"));
continue;  // TODO: Remove this
    }

    spScope->AddChild(spChild);
  }
}


AST::FunctionDeclarationPtr Vectorizer::VASTBuilder::BuildFunctionDecl(::clang::FunctionDecl *pFunctionDeclaration)
{
  AST::FunctionDeclarationPtr spFunctionDecl = AST::CreateNode< AST::FunctionDeclaration >();

  spFunctionDecl->SetName(pFunctionDeclaration->getName());

  for (size_t i = 0; i < pFunctionDeclaration->getNumParams(); ++i)
  {
    AST::BaseClasses::VariableInfoPtr spVariable = std::make_shared<AST::BaseClasses::VariableInfo>();

    spVariable->SetName(pFunctionDeclaration->getParamDecl(i)->getNameAsString());

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

