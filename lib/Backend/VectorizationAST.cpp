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
#include <algorithm>
#include <sstream>

using namespace clang::hipacc::Backend::Vectorization;
using namespace std;


#define CHECK_NULL_POINTER(ptr)   if (ptr == nullptr)   { throw InternalErrors::NullPointerException(#ptr); }


AST::BaseClasses::TypeInfo& AST::BaseClasses::TypeInfo::operator=(const TypeInfo &crRVal)
{
  _bIsConst   = crRVal._bIsConst;
  _bIsPointer = crRVal._bIsPointer;
  _eType      = crRVal._eType;

  _vecArrayDimensions.clear();
  _vecArrayDimensions.insert(_vecArrayDimensions.end(), crRVal._vecArrayDimensions.begin(), crRVal._vecArrayDimensions.end());

  return *this;
}

string AST::BaseClasses::TypeInfo::_GetBoolString(bool bValue)
{
  return bValue ? "true" : "false";
}

AST::BaseClasses::TypeInfo AST::BaseClasses::TypeInfo::CreateDereferencedType()
{
  TypeInfo ReturnType(*this);

  if (ReturnType.IsArray())
  {
    ReturnType.GetArrayDimensions().erase( ReturnType.GetArrayDimensions().begin() );
  }
  else if (ReturnType.GetPointer())
  {
    ReturnType.SetPointer(false);
  }
  else
  {
    throw ASTExceptions::NonDereferencableType();
  }

  return ReturnType;
}

AST::BaseClasses::TypeInfo AST::BaseClasses::TypeInfo::CreateSizedIntegerType(size_t szTypeSize, bool bSigned)
{
  TypeInfo ReturnType;

  ReturnType.SetConst(false);
  ReturnType.SetPointer(false);

  KnownTypes eType = KnownTypes::Unknown;

  switch (szTypeSize)
  {
  case sizeof( int8_t  ):   eType = bSigned ? KnownTypes::Int8  : KnownTypes::UInt8;    break;
  case sizeof( int16_t ):   eType = bSigned ? KnownTypes::Int16 : KnownTypes::UInt16;   break;
  case sizeof( int32_t ):   eType = bSigned ? KnownTypes::Int32 : KnownTypes::UInt32;   break;
  case sizeof( int64_t ):   eType = bSigned ? KnownTypes::Int64 : KnownTypes::UInt64;   break;
  }

  ReturnType.SetType(eType);

  return ReturnType;
}

size_t AST::BaseClasses::TypeInfo::GetTypeSize(KnownTypes eType)
{
  switch (eType)
  {
  case KnownTypes::Bool:    return static_cast< size_t >( 1 );
  case KnownTypes::Int8:    return sizeof( int8_t );
  case KnownTypes::UInt8:   return sizeof( uint8_t );
  case KnownTypes::Int16:   return sizeof( int16_t );
  case KnownTypes::UInt16:  return sizeof( uint16_t );
  case KnownTypes::Int32:   return sizeof( int32_t );
  case KnownTypes::UInt32:  return sizeof( uint32_t );
  case KnownTypes::Int64:   return sizeof( int64_t );
  case KnownTypes::UInt64:  return sizeof( uint64_t );
  case KnownTypes::Float:   return sizeof( float );
  case KnownTypes::Double:  return sizeof( double );
  case KnownTypes::Unknown: return static_cast< size_t >( 0 );
  default:                  throw InternalErrorException("Unknown type!"); 
  }
}

string AST::BaseClasses::TypeInfo::GetTypeString(KnownTypes eType)
{
  switch (eType)
  {
  case KnownTypes::Bool:    return "Bool";
  case KnownTypes::Int8:    return "Int8";
  case KnownTypes::UInt8:   return "UInt8";
  case KnownTypes::Int16:   return "Int16";
  case KnownTypes::UInt16:  return "UInt16";
  case KnownTypes::Int32:   return "Int32";
  case KnownTypes::UInt32:  return "UInt32";
  case KnownTypes::Int64:   return "Int64";
  case KnownTypes::UInt64:  return "UInt64";
  case KnownTypes::Float:   return "Float";
  case KnownTypes::Double:  return "Double";
  case KnownTypes::Unknown: return "Unknown";
  default:                  throw InternalErrorException("Unknown type!");
  }
}

bool AST::BaseClasses::TypeInfo::IsSigned(KnownTypes eType)
{
  switch (eType)
  {
  case KnownTypes::Int8:
  case KnownTypes::Int16:
  case KnownTypes::Int32:
  case KnownTypes::Int64:
  case KnownTypes::Float:
  case KnownTypes::Double:  return true;
  case KnownTypes::Bool:
  case KnownTypes::UInt8:
  case KnownTypes::UInt16:
  case KnownTypes::UInt32:
  case KnownTypes::UInt64:
  case KnownTypes::Unknown: return false;
  default:                  throw InternalErrorException("Unknown type!");
  }
}


string AST::BaseClasses::TypeInfo::DumpToXML(size_t szIntend)
{
  stringstream XmlStream;

  XmlStream << string(szIntend, ' ') << "<TypeInfo";

  XmlStream << " type=\""       << GetTypeString( _eType )        << "\"";
  XmlStream << " is_const=\""   << _GetBoolString( GetConst() )   << "\"";
  XmlStream << " is_pointer=\"" << _GetBoolString( GetPointer() ) << "\"";
  XmlStream << " is_array=\""   << _GetBoolString( IsArray() )    << "\"";

  if (IsArray())
  {
    XmlStream << " array_dim=\"";

    for each (auto itDim in _vecArrayDimensions)
    {
      XmlStream << "[" << itDim << "]";
    }
    
    XmlStream << "\"";
  }

  XmlStream << " />\n";

  return XmlStream.str();
}



string AST::BaseClasses::VariableInfo::DumpToXML(size_t szIntend)
{
  string strPadString("");
  strPadString.resize(szIntend, ' ');

  string strXmlString = strPadString + string("<Variable name=\"") + GetName() + string("\">\n");

  strXmlString += _Type.DumpToXML(szIntend + 2);

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


string AST::BaseClasses::Expression::_DumpResultTypeToXML(size_t szIntend)
{
  string strPadString(szIntend, ' ');

  string strXmlString  = strPadString + string("<ResultType>\n");
  strXmlString        += GetResultType().DumpToXML(szIntend + 2);
  strXmlString        += strPadString + string("</ResultType>\n");

  return strXmlString;
}




AST::BaseClasses::ExpressionPtr AST::Expressions::Value::GetSubExpression(IndexType SubExprIndex)
{
  throw ASTExceptions::ChildIndexOutOfRange();
}


string AST::Expressions::Constant::DumpToXML(size_t szIntend)
{
  string strXmlString(szIntend, ' ');

  strXmlString += string("<Constant type=\"") + BaseClasses::TypeInfo::GetTypeString(_eType) + string("\" ");
  strXmlString += string("value=\"") + GetAsString() + string("\" />\n");

  return strXmlString;
}

string AST::Expressions::Constant::GetAsString() const
{
  stringstream OutputStream;

  switch (_eType)
  {
  case KnownTypes::Bool:
    OutputStream << ( (_unionValues.ui64IntegralValue == static_cast< uint64_t >(0)) ? "false" : "true" );
    break;
  case KnownTypes::Int8:    OutputStream << static_cast< int8_t   >( _unionValues.ui64IntegralValue );    break;
  case KnownTypes::UInt8:   OutputStream << static_cast< uint8_t  >( _unionValues.ui64IntegralValue );    break;
  case KnownTypes::Int16:   OutputStream << static_cast< int16_t  >( _unionValues.ui64IntegralValue );    break;
  case KnownTypes::UInt16:  OutputStream << static_cast< uint16_t >( _unionValues.ui64IntegralValue );    break;
  case KnownTypes::Int32:   OutputStream << static_cast< int32_t  >( _unionValues.ui64IntegralValue );    break;
  case KnownTypes::UInt32:  OutputStream << static_cast< uint32_t >( _unionValues.ui64IntegralValue );    break;
  case KnownTypes::Int64:   OutputStream << static_cast< int64_t  >( _unionValues.ui64IntegralValue );    break;
  case KnownTypes::UInt64:  OutputStream << static_cast< uint64_t >( _unionValues.ui64IntegralValue );    break;
  case KnownTypes::Float:   OutputStream << static_cast< float  >( _unionValues.dFloatingPointValue );    break;
  case KnownTypes::Double:  OutputStream << static_cast< double >( _unionValues.dFloatingPointValue );    break;
  default:                  throw InternalErrorException("Unexpected constant data type!");
  }

  return OutputStream.str();
}

AST::BaseClasses::TypeInfo AST::Expressions::Constant::GetResultType() const
{
  BaseClasses::TypeInfo ResultType;

  ResultType.SetConst(true);
  ResultType.SetPointer(false);
  ResultType.SetType(GetValueType());

  return ResultType;
}


AST::BaseClasses::TypeInfo AST::Expressions::Identifier::GetResultType() const
{
  BaseClasses::VariableInfoPtr spVariableInfo = LookupVariableInfo();

  if (spVariableInfo)
  {
    return spVariableInfo->GetTypeInfo();
  }
  else
  {
    return BaseClasses::TypeInfo();
  }
}

string AST::Expressions::Identifier::DumpToXML(size_t szIntend)
{
  string strXmlString(szIntend, ' ');

  strXmlString += string("<Identifier name=\"") + GetAsString() + string("\" />\n");

  return strXmlString;
}

AST::BaseClasses::VariableInfoPtr AST::Expressions::Identifier::LookupVariableInfo() const
{
  BaseClasses::NodePtr spParent = GetThis();

  while (spParent)
  {
    if (spParent->GetNodeType() == BaseClasses::Node::NodeType::FunctionDeclaration)
    {
      return spParent->CastToType< FunctionDeclaration >()->GetVariableInfo( GetName() );
    }
    else
    {
      spParent = spParent->GetParent();
    }
  }

  return nullptr;
}


void AST::Expressions::MemoryAccess::SetIndexExpression(ExpressionPtr spIndexExpression)
{
  if (_spIndexExpr)
  {
    _RemoveParentFromChild(_spIndexExpr);
  }

  _spIndexExpr = spIndexExpression;

  if (_spIndexExpr)
  {
    _SetParentToChild(_spIndexExpr);
  }
}

void AST::Expressions::MemoryAccess::SetMemoryReference(ExpressionPtr spMemoryReference)
{
  if (_spMemoryRef)
  {
    _RemoveParentFromChild(_spMemoryRef);
  }

  _spMemoryRef = spMemoryReference;

  if (_spMemoryRef)
  {
    _SetParentToChild(_spMemoryRef);
  }
}

AST::BaseClasses::ExpressionPtr AST::Expressions::MemoryAccess::GetSubExpression(IndexType SubExprIndex)
{
  switch (SubExprIndex)
  {
  case 0:   return GetMemoryReference();
  case 1:   return GetIndexExpression();
  default:  throw ASTExceptions::ChildIndexOutOfRange();
  }
}

string AST::Expressions::MemoryAccess::DumpToXML(size_t szIntend)
{
  string strPadString(szIntend, ' ');

  string strXmlString  = strPadString + string("<MemoryAccess>\n");
  strXmlString        += _DumpResultTypeToXML(szIntend + 2);

  strXmlString += strPadString + string("  <MemoryRef>\n");
  if (GetMemoryReference())
  {
    strXmlString += GetMemoryReference()->DumpToXML(szIntend + 4);
  }
  strXmlString += strPadString + string("  </MemoryRef>\n");

  strXmlString += strPadString + string("  <Index>\n");
  if (GetIndexExpression())
  {
    strXmlString += GetIndexExpression()->DumpToXML(szIntend + 4);
  }
  strXmlString += strPadString + string("  </Index>\n");

  strXmlString += strPadString + string("</MemoryAccess>\n");

  return strXmlString;

}

AST::BaseClasses::TypeInfo AST::Expressions::MemoryAccess::GetResultType() const
{
  if (GetMemoryReference())
  {
    return GetMemoryReference()->GetResultType().CreateDereferencedType();
  }
  else
  {
    return BaseClasses::TypeInfo();
  }
}



string AST::Expressions::UnaryExpression::_DumpSubExpressionToXML(size_t szIntend)
{
  string strPadString(szIntend, ' ');

  string strXmlString  =_DumpResultTypeToXML(szIntend);
  strXmlString        += strPadString + string("<SubExpression>\n");

  if (GetSubExpression())
  {
    strXmlString += GetSubExpression()->DumpToXML(szIntend + 2);
  }

  strXmlString += strPadString + string("</SubExpression>\n");

  return strXmlString;
}

void AST::Expressions::UnaryExpression::SetSubExpression(BaseClasses::ExpressionPtr spSubExpr)
{
  if (_spSubExpression)
  {
    _RemoveParentFromChild(_spSubExpression);
  }

  _spSubExpression = spSubExpr;

  if (_spSubExpression)
  {
    _SetParentToChild(_spSubExpression);
  }
}

AST::BaseClasses::ExpressionPtr AST::Expressions::UnaryExpression::GetSubExpression(IndexType SubExprIndex)
{
  if (SubExprIndex == static_cast<IndexType>(0))
  {
    return GetSubExpression();
  }
  else
  {
    throw ASTExceptions::ChildIndexOutOfRange();
  }
}


string AST::Expressions::Conversion::DumpToXML(size_t szIntend)
{
  string strPadString(szIntend, ' ');

  string strXmlString = strPadString + string("<Conversion>\n");
  strXmlString += _DumpSubExpressionToXML(szIntend + 2);
  strXmlString += strPadString + string("</Conversion>\n");

  return strXmlString;
}

AST::BaseClasses::TypeInfo AST::Expressions::Parenthesis::GetResultType() const
{
  if (GetSubExpression())
  {
    return GetSubExpression()->GetResultType();
  }
  else
  {
    return BaseClasses::TypeInfo();
  }
}

string AST::Expressions::Parenthesis::DumpToXML(size_t szIntend)
{
  string strPadString(szIntend, ' ');

  string strXmlString  = strPadString + string("<Parenthesis>\n");
  strXmlString        += _DumpSubExpressionToXML(szIntend + 2);
  strXmlString        += strPadString + string("</Parenthesis>\n");

  return strXmlString;
}


string AST::Expressions::BinaryOperator::_DumpSubExpressionsToXML(size_t szIntend)
{
  string strPadString(szIntend, ' ');

  string strXmlString  = _DumpResultTypeToXML(szIntend);
  strXmlString        += strPadString + string("<LHS>\n");

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


string AST::Expressions::ArithmeticOperator::_GetOperatorTypeString(ArithmeticOperatorType eType)
{
  switch (eType)
  {
  case ArithmeticOperatorType::Add:           return "Add";
  case ArithmeticOperatorType::BitwiseAnd:    return "BitwiseAnd";
  case ArithmeticOperatorType::BitwiseOr:     return "BitwiseOr";
  case ArithmeticOperatorType::BitwiseXOr:    return "BitwiseXOr";
  case ArithmeticOperatorType::Divide:        return "Divide";
  case ArithmeticOperatorType::Modulo:        return "Modulo";
  case ArithmeticOperatorType::Multiply:      return "Multiply";
  case ArithmeticOperatorType::ShiftLeft:     return "ShiftLeft";
  case ArithmeticOperatorType::ShiftRight:    return "ShiftRight";
  case ArithmeticOperatorType::Subtract:      return "Subtract";
  default:                                    throw InternalErrorException("Unknown arithmetic operator type!");
  }
}

AST::BaseClasses::TypeInfo::KnownTypes AST::Expressions::ArithmeticOperator::_GetPromotedType(KnownTypes eTypeLHS, KnownTypes eTypeRHS)
{
  if      ((eTypeLHS == KnownTypes::Unknown) || (eTypeRHS == KnownTypes::Unknown))
  {
    return KnownTypes::Unknown;
  }
  else if ((eTypeLHS == KnownTypes::Double)  || (eTypeRHS == KnownTypes::Double))
  {
    return KnownTypes::Double;
  }
  else if ((eTypeLHS == KnownTypes::Float)   || (eTypeRHS == KnownTypes::Float))
  {
    return KnownTypes::Float;
  }
  else
  {
    // We have an integer type => Promote to the larger type and keep the sign
    size_t  szTypeSize  = std::max( TypeInfo::GetTypeSize(eTypeLHS), TypeInfo::GetTypeSize(eTypeRHS) );
    bool    bSigned     = TypeInfo::IsSigned(eTypeLHS) | TypeInfo::IsSigned(eTypeRHS);

    return TypeInfo::CreateSizedIntegerType(szTypeSize, bSigned).GetType();
  }
}

string AST::Expressions::ArithmeticOperator::DumpToXML(size_t szIntend)
{
  string strPadString(szIntend, ' ');

  string strXmlString = strPadString + string("<ArithmeticOperator ");
  strXmlString += string("type=\"") + _GetOperatorTypeString(_eOpType) + string("\">\n");

  strXmlString += _DumpSubExpressionsToXML(szIntend + 2);

  strXmlString += strPadString + string("</ArithmeticOperator>\n");

  return strXmlString;
}

AST::BaseClasses::TypeInfo AST::Expressions::ArithmeticOperator::GetResultType() const
{
  if ( GetLHS() && GetRHS() )     // Check if both children are set
  {
    TypeInfo TypeLHS = GetLHS()->GetResultType();
    TypeInfo TypeRHS = GetRHS()->GetResultType();

    if ( (TypeLHS.GetType() == KnownTypes::Unknown) || (TypeRHS.GetType() == KnownTypes::Unknown) )
    {
      // Cannot do arithmetic with unknown types => Return type is unknown
      return TypeInfo();
    }
    else if (TypeRHS.GetPointer() || TypeRHS.IsArray())
    {
      // Expected single value for right operand => Unknown type
      return TypeInfo();
    }
    else if (TypeLHS.IsArray())
    {
      // Array arithmetic is forbidden => Unknown type
      return TypeInfo();
    }
    else if (TypeLHS.GetPointer())
    {
      // Pointer arithmetic is only allowed for the "Add" and "Subtract" operator
      if ( (GetOperatorType() == ArithmeticOperatorType::Add) || (GetOperatorType() == ArithmeticOperatorType::Subtract) )
      {
        return TypeLHS;
      }
      else
      {
        return TypeInfo();
      }
    }
    else
    {
      // Both operands are single values => Return the promoted type
      TypeInfo ReturnType;

      ReturnType.SetConst(true);
      ReturnType.SetPointer(false);
      ReturnType.SetType( _GetPromotedType(TypeLHS.GetType(), TypeRHS.GetType()) );

      return ReturnType;
    }
  }
  else                            // Incomplete statement => Return type cannot be created
  {
    return TypeInfo();
  }
}


string AST::Expressions::AssignmentOperator::DumpToXML(size_t szIntend)
{
  string strPadString(szIntend, ' ');

  string strXmlString = strPadString + string("<AssignmentOperator>\n");

  strXmlString += _DumpSubExpressionsToXML(szIntend + 2);

  strXmlString += strPadString + string("</AssignmentOperator>\n");

  return strXmlString;
}

AST::BaseClasses::TypeInfo AST::Expressions::AssignmentOperator::GetResultType() const
{
  if (GetLHS())
  {
    BaseClasses::TypeInfo ResultType(GetLHS()->GetResultType());

    if ( (! ResultType.GetPointer()) && (! ResultType.IsArray()) )
    {
      ResultType.SetConst(true);
    }

    return ResultType;
  }
  else
  {
    return BaseClasses::TypeInfo();
  }
}


string AST::Expressions::RelationalOperator::_GetOperatorTypeString(RelationalOperatorType eType)
{
  switch (eType)
  {
  case RelationalOperatorType::Equal:         return "Equal";
  case RelationalOperatorType::Greater:       return "Greater";
  case RelationalOperatorType::GreaterEqual:  return "GreaterEqual";
  case RelationalOperatorType::Less:          return "Less";
  case RelationalOperatorType::LessEqual:     return "LessEqual";
  case RelationalOperatorType::LogicalAnd:    return "LogicalAnd";
  case RelationalOperatorType::LogicalOr:     return "LogicalOr";
  case RelationalOperatorType::NotEqual:      return "NotEqual";
  default:                                    throw InternalErrorException("Unknown relational operator type!");
  }
}

string AST::Expressions::RelationalOperator::DumpToXML(size_t szIntend)
{
  string strPadString(szIntend, ' ');

  string strXmlString = strPadString + string("<RelationalOperator ");
  strXmlString += string("type=\"") + _GetOperatorTypeString(_eOpType) + string("\">\n");

  strXmlString += _DumpSubExpressionsToXML(szIntend + 2);

  strXmlString += strPadString + string("</RelationalOperator>\n");

  return strXmlString;
}

AST::BaseClasses::TypeInfo AST::Expressions::RelationalOperator::GetResultType() const
{
  BaseClasses::TypeInfo ResultType;

  ResultType.SetConst(true);
  ResultType.SetPointer(false);
  ResultType.SetType(BaseClasses::TypeInfo::KnownTypes::Bool);

  return ResultType;
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

AST::BaseClasses::VariableInfoPtr  AST::FunctionDeclaration::GetVariableInfo(std::string strVariableName)
{
  auto itVariableEntry = _mapKnownVariables.find(strVariableName);

  if (itVariableEntry != _mapKnownVariables.end())
  {
    return itVariableEntry->second;
  }
  else
  {
    return nullptr;
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

