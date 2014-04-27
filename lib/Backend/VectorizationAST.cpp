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

//===--- VectorizationAST.cpp - Implements a vectorizable syntax tree. ---------------===//
//
// This file implements the internally used vectorizable syntax tree (a simplification to clang's AST)
//
//===---------------------------------------------------------------------------------===//

#include "hipacc/Backend/VectorizationAST.h"

using namespace clang::hipacc::Backend::Vectorization;
using namespace std;


#define CHECK_NULL_POINTER(ptr)   if (ptr == nullptr)   { throw InternalErrors::NullPointerException(#ptr); }


string AST::BaseClasses::VariableInfo::DumpToXML(size_t szIntend)
{
  string strPadString("");
  strPadString.resize(szIntend, ' ');

  string strXmlString = strPadString + string("<Variable name =\"") + GetName() + string("\">\n");

  strXmlString += strPadString + string("</Variable>\n");

  return strXmlString;
}


AST::BaseClasses::NodePtr AST::BaseClasses::Node::GetParent()
{
  if (_wpParent.expired())
  {
    return nullptr;
  }
  else
  {
    return _wpParent.lock();
  }
}


void AST::BaseClasses::Node::_RemoveParentFromChild(NodePtr spChild)
{
  CHECK_NULL_POINTER(spChild);

  spChild->_SetParent(nullptr);
}

void AST::BaseClasses::Node::_SetParentToChild(NodePtr spChild)
{
  CHECK_NULL_POINTER(spChild);

  spChild->_SetParent(_wpThis.lock());
}



AST::BaseClasses::ExpressionPtr AST::Expressions::Value::GetSubExpression(IndexType SubExprIndex)
{
  throw ASTExceptions::ChildIndexOutOfRange();
}


string AST::Expressions::Constant::DumpToXML(size_t szIntend)
{
  string strXmlString;
  strXmlString.resize(szIntend, ' ');

  strXmlString += string("<Constant value=\"") + GetAsString() + string("\" />\n");

  return strXmlString;
}

string AST::Expressions::Identifier::DumpToXML(size_t szIntend)
{
  string strXmlString;
  strXmlString.resize(szIntend, ' ');

  strXmlString += string("<Identifier name=\"") + GetAsString() + string("\" />\n");

  return strXmlString;
}


string AST::Expressions::BinaryOperator::_DumpSubExpressionsToXML(size_t szIntend)
{
  string strPadString("");
  strPadString.resize(szIntend, ' ');

  string strXmlString = strPadString + string("<LHS>\n");

  if (GetLHS())
  {
    strXmlString += GetLHS()->DumpToXML(szIntend + 2);
  }

  strXmlString += strPadString + string("</LHS>\n");

  strXmlString += strPadString + string("<RHS>\n");

  if (GetRHS())
  {
    strXmlString += GetRHS()->DumpToXML(szIntend + 2);
  }

  strXmlString += strPadString + string("</RHS>\n");

  return strXmlString;
}

void AST::Expressions::BinaryOperator::SetLHS(ExpressionPtr spNewLHS)
{
  if (_spLHS)
  {
    _RemoveParentFromChild(_spLHS);
  }

  _spLHS = spNewLHS;

  if (_spLHS)
  {
    _SetParentToChild(_spLHS);
  }
}

void AST::Expressions::BinaryOperator::SetRHS(ExpressionPtr spNewRHS)
{
  if (_spRHS)
  {
    _RemoveParentFromChild(_spRHS);
  }

  _spRHS = spNewRHS;

  if (_spRHS)
  {
    _SetParentToChild(_spRHS);
  }
}


AST::BaseClasses::ExpressionPtr AST::Expressions::BinaryOperator::GetSubExpression(IndexType SubExprIndex)
{
  switch (SubExprIndex)
  {
  case 0:   return GetLHS();
  case 1:   return GetRHS();
  default:  throw ASTExceptions::ChildIndexOutOfRange();
  }
}


string AST::Expressions::AssignmentOperator::DumpToXML(size_t szIntend)
{
  string strPadString("");
  strPadString.resize(szIntend, ' ');

  string strXmlString = strPadString + string("<AssignmentOperator>\n");

  strXmlString += _DumpSubExpressionsToXML(szIntend + 2);

  strXmlString += strPadString + string("</AssignmentOperator>\n");

  return strXmlString;
}


void AST::Scope::AddChild(NodePtr spChild)
{
  CHECK_NULL_POINTER(spChild);

  _SetParentToChild(spChild);
  _Children.push_back(spChild);
}

void AST::Scope::AddVariable(BaseClasses::VariableInfoPtr spVariableInfo)
{
  CHECK_NULL_POINTER(spVariableInfo);

  BaseClasses::NodePtr spParent = GetThis();

  while (true)
  {
    spParent = spParent->GetParent();
    CHECK_NULL_POINTER(spParent);

    if (spParent->GetNodeType() == BaseClasses::Node::NodeType::Scope)
    {
      spParent->CastToType<AST::Scope>()->AddVariable(spVariableInfo);
      break;
    }
    else if (spParent->GetNodeType() == BaseClasses::Node::NodeType::FunctionDeclaration)
    {
      spParent->CastToType<AST::FunctionDeclaration>()->AddVariable(spVariableInfo);
      break;
    }
  }
}

AST::BaseClasses::NodePtr AST::Scope::GetChild(IndexType ChildIndex)
{
  if (ChildIndex >= GetChildCount())
  {
    throw ASTExceptions::ChildIndexOutOfRange();
  }
  else
  {
    return _Children[ChildIndex];
  }
}

string AST::Scope::DumpToXML(size_t szIntend)
{
  string strPadString("");
  strPadString.resize(szIntend, ' ');

  string strXmlString = strPadString + string("<Scope>\n");

  for each (auto itNode in _Children)
  {
    strXmlString += itNode->DumpToXML(szIntend + 2);
  }

  strXmlString += strPadString + string("</Scope>\n");

  return strXmlString;
}


AST::FunctionDeclaration::FunctionDeclaration() : BaseType(Node::NodeType::FunctionDeclaration), _spBody(nullptr)
{
}


void AST::FunctionDeclaration::AddParameter(BaseClasses::VariableInfoPtr spVariableInfo)
{
  CHECK_NULL_POINTER(spVariableInfo);

  // TODO: Quick and dirty => fix this
  _mapKnownVariables[spVariableInfo->GetName()] = spVariableInfo;

  Expressions::IdentifierPtr spParameter = AST::CreateNode<Expressions::Identifier>();
  _SetParentToChild(spParameter);

  spParameter->SetName(spVariableInfo->GetName());

  _Parameters.push_back(spParameter);
}

void AST::FunctionDeclaration::AddVariable(BaseClasses::VariableInfoPtr spVariableInfo)
{
  CHECK_NULL_POINTER(spVariableInfo);

  // TODO: Quick and dirty => fix this
  _mapKnownVariables[spVariableInfo->GetName()] = spVariableInfo;
}


AST::ScopePtr AST::FunctionDeclaration::GetBody()
{
  if (!_spBody)
  {
    _spBody = AST::CreateNode< Scope >();
    _SetParentToChild(_spBody);
  }

  return _spBody;
}

AST::BaseClasses::NodePtr AST::FunctionDeclaration::GetChild(IndexType ChildIndex)
{
  if (ChildIndex == static_cast<IndexType>(0))
  {
    return GetBody();
  }
  else
  {
    throw ASTExceptions::ChildIndexOutOfRange();
  }
}

string AST::FunctionDeclaration::DumpToXML(size_t szIntend)
{
  string strPadString("");
  strPadString.resize(szIntend, ' ');

  string strXmlString = strPadString + string("<FunctionDeclaration name=\"") + GetName() + ("\">\n");

  // Dump known variables
  {
    strXmlString += strPadString + string("  ") + string("<KnownVariables>\n");

    for each (auto itVariable in _mapKnownVariables)
    {
      strXmlString += itVariable.second->DumpToXML(szIntend + 4);
    }

    strXmlString += strPadString + string("  ") + string("</KnownVariables>\n");
  }

  // Dump parameters
  {
    strXmlString += strPadString + string("  ") + string("<Parameters>\n");

    for each (auto itParameter in _Parameters)
    {
      strXmlString += itParameter->DumpToXML(szIntend + 4);
    }

    strXmlString += strPadString + string("  ") + string("</Parameters>\n");
  }

  // Dump body
  {
    strXmlString += strPadString + string("  ") + string("<Body>\n");

    strXmlString += GetBody()->DumpToXML(szIntend + 4);

    strXmlString += strPadString + string("  ") + string("</Body>\n");
  }

  strXmlString += strPadString + string("</FunctionDeclaration>\n");

  return strXmlString;
}




// vim: set ts=2 sw=2 sts=2 et ai:

