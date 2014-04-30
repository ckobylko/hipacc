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


class XMLSupport
{
public:

  typedef map< string, string >   AttributesMapType;


public:

  inline static string CreateXmlTag(const size_t cszIntend, string strName)
  {
    return CreateXmlTag(cszIntend, strName, string(""));
  }

  inline static string CreateXmlTag(const size_t cszIntend, string strName, const AttributesMapType &crmapAttributes)
  {
    return CreateXmlTag(cszIntend, strName, string(""), crmapAttributes);
  }

  inline static string CreateXmlTag(const size_t cszIntend, string strName, const string &crstrInternalText)
  {
    return CreateXmlTag(cszIntend, strName, crstrInternalText, AttributesMapType());
  }

  static string CreateXmlTag(const size_t cszIntend, string strName, const string &crstrInternalText, const AttributesMapType &crmapAttributes);


  inline static string GetPadString(const size_t cszIntend)    { return string(cszIntend, ' '); }


  template <typename ValueType>   inline static string ToString(ValueType TValue)
  {
    stringstream OutpuStream;
    OutpuStream << TValue;
    return OutpuStream.str();
  }

  template <>                     inline static string ToString<bool>(bool TValue)
  {
    return TValue ? "true" : "false";
  }
};


// Implementation of class AST::XMLSupport
string XMLSupport::CreateXmlTag(const size_t cszIntend, string strName, const string &crstrInternalText, const AttributesMapType &crmapAttributes)
{
  string strAttributes("");

  for each (auto itAttribute in crmapAttributes)
  {
    strAttributes += string(" ") + itAttribute.first + string("=\"") + itAttribute.second + string("\"");
  }


  if (crstrInternalText.empty())
  {
    return GetPadString(cszIntend) + string("<") + strName + strAttributes + string(" />\n");
  }
  else
  {
    string strXmlString("");

    strXmlString += GetPadString(cszIntend) + string("<") + strName + strAttributes + string(">\n");
    strXmlString += crstrInternalText;
    strXmlString += GetPadString(cszIntend) + string("</") + strName + string(">\n");

    return strXmlString;
  }
}



/***********************/
/***   BaseClasses   ***/
/***********************/

// Implementation of class AST::BaseClasses::TypeInfo
AST::BaseClasses::TypeInfo& AST::BaseClasses::TypeInfo::operator=(const TypeInfo &crRVal)
{
  _bIsConst   = crRVal._bIsConst;
  _bIsPointer = crRVal._bIsPointer;
  _eType      = crRVal._eType;

  _vecArrayDimensions.clear();
  _vecArrayDimensions.insert(_vecArrayDimensions.end(), crRVal._vecArrayDimensions.begin(), crRVal._vecArrayDimensions.end());

  return *this;
}

AST::BaseClasses::TypeInfo AST::BaseClasses::TypeInfo::CreateDereferencedType() const
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

AST::BaseClasses::TypeInfo AST::BaseClasses::TypeInfo::CreatePointerType() const
{
  TypeInfo ReturnType(*this);

  if (ReturnType.IsSingleValue())
  {
    ReturnType.SetPointer(true);
  }
  else
  {
    throw RuntimeErrorException("Only single value types have a corresponding pointer type!");
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

string AST::BaseClasses::TypeInfo::DumpToXML(const size_t cszIntend) const
{
  XMLSupport::AttributesMapType mapAttributes;

  mapAttributes["Type"]       = GetTypeString( _eType );
  mapAttributes["is_const"]   = XMLSupport::ToString( GetConst() );
  mapAttributes["is_pointer"] = XMLSupport::ToString( GetPointer() );
  mapAttributes["is_array"]   = XMLSupport::ToString( IsArray() );

  if (IsArray())
  {
    string strDim("");

    for each (auto itDim in _vecArrayDimensions)
    {
      strDim += string("[") + XMLSupport::ToString(itDim) + string("]");
    }

    mapAttributes["array_dim"] = strDim;
  }

  return XMLSupport::CreateXmlTag(cszIntend, "TypeInfo", mapAttributes);
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


// Implementation of class AST::BaseClasses::VariableInfo
string AST::BaseClasses::VariableInfo::DumpToXML(const size_t cszIntend) const
{
  XMLSupport::AttributesMapType mapAttributes;

  mapAttributes["name"] = GetName();

  return XMLSupport::CreateXmlTag(cszIntend, "Variable", _Type.DumpToXML(cszIntend + 2), mapAttributes);
}


// Implementation of class AST::BaseClasses::Node
string AST::BaseClasses::Node::_DumpChildToXml(const NodePtr spChild, const size_t cszIntend)
{
  return spChild ? spChild->DumpToXML(cszIntend) : "";
}

void AST::BaseClasses::Node::_SetParentToChild(NodePtr spChild) const
{
  if (spChild)
  {
    spChild->_SetParent(_wpThis.lock());
  }
}

AST::BaseClasses::NodePtr AST::BaseClasses::Node::GetParent()
{
  return _wpParent.expired() ? nullptr : _wpParent.lock();
}


// Implementation of class AST::BaseClasses::Expression
string AST::BaseClasses::Expression::_DumpResultTypeToXML(const size_t cszIntend) const
{
  return XMLSupport::CreateXmlTag(cszIntend, "ResultType", GetResultType().DumpToXML(cszIntend + 2));
}



/***********************/
/***   ControlFlow   ***/
/***********************/

// Implementation of class AST::ControlFlow::Loop
string AST::ControlFlow::Loop::_GetLoopTypeString(LoopType eType)
{
  switch (eType)
  {
  case LoopType::TopControlled:     return "TopControlled";
  case LoopType::BottomControlled:  return "BottomControlled";
  default:                          throw InternalErrorException("Unknown loop type!");
  }
}

string AST::ControlFlow::Loop::DumpToXML(const size_t cszIntend) const
{
  XMLSupport::AttributesMapType mapAttributes;

  mapAttributes["type"] = _GetLoopTypeString(GetLoopType());

  string strXmlString("");

  strXmlString += XMLSupport::CreateXmlTag( cszIntend + 2, "Condition", _DumpChildToXml(GetCondition(), cszIntend + 4) );
  strXmlString += XMLSupport::CreateXmlTag( cszIntend + 2, "Increment", _DumpChildToXml(GetIncrement(), cszIntend + 4) );
  strXmlString += XMLSupport::CreateXmlTag( cszIntend + 2, "Body",      _DumpChildToXml(GetBody(), cszIntend + 4) );

  return XMLSupport::CreateXmlTag(cszIntend, "Loop", strXmlString, mapAttributes);
}

AST::ScopePtr AST::ControlFlow::Loop::GetBody()
{
  if (! _spBody)
  {
    _SetChildPtr(_spBody, AST::CreateNode<AST::Scope>());
  }

  return _spBody;
}

const AST::ScopePtr AST::ControlFlow::Loop::GetBody() const
{
  CHECK_NULL_POINTER(_spBody);

  return _spBody;
}

AST::BaseClasses::NodePtr  AST::ControlFlow::Loop::GetChild(IndexType ChildIndex)
{
  switch (ChildIndex)
  {
  case 0:   return GetCondition();
  case 1:   return GetIncrement();
  case 2:   return GetBody();
  default:  throw ASTExceptions::ChildIndexOutOfRange();
  }
}



/***********************/
/***   Expressions   ***/
/***********************/

// Implementation of class AST::Expressions::Value
AST::BaseClasses::ExpressionPtr AST::Expressions::Value::GetSubExpression(IndexType SubExprIndex)
{
  throw ASTExceptions::ChildIndexOutOfRange();
}


// Implementation of class AST::Expressions::Constant
string AST::Expressions::Constant::DumpToXML(const size_t cszIntend) const
{
  XMLSupport::AttributesMapType mapAttributes;

  mapAttributes["type"]   = BaseClasses::TypeInfo::GetTypeString(_eType);
  mapAttributes["value"]  = GetAsString();

  return XMLSupport::CreateXmlTag(cszIntend, "Constant", mapAttributes);
}

string AST::Expressions::Constant::GetAsString() const
{
  switch (_eType)
  {
  case KnownTypes::Bool:    return XMLSupport::ToString( _unionValues.ui64IntegralValue == static_cast< uint64_t >(0) );
  case KnownTypes::Int8:    return XMLSupport::ToString( static_cast< int8_t   >(_unionValues.ui64IntegralValue) );
  case KnownTypes::UInt8:   return XMLSupport::ToString( static_cast< uint8_t  >(_unionValues.ui64IntegralValue) );
  case KnownTypes::Int16:   return XMLSupport::ToString( static_cast< int16_t  >(_unionValues.ui64IntegralValue) );
  case KnownTypes::UInt16:  return XMLSupport::ToString( static_cast< uint16_t >(_unionValues.ui64IntegralValue) );
  case KnownTypes::Int32:   return XMLSupport::ToString( static_cast< int32_t  >(_unionValues.ui64IntegralValue) );
  case KnownTypes::UInt32:  return XMLSupport::ToString( static_cast< uint32_t >(_unionValues.ui64IntegralValue) );
  case KnownTypes::Int64:   return XMLSupport::ToString( static_cast< int64_t  >(_unionValues.ui64IntegralValue) );
  case KnownTypes::UInt64:  return XMLSupport::ToString( static_cast< uint64_t >(_unionValues.ui64IntegralValue) );
  case KnownTypes::Float:   return XMLSupport::ToString( static_cast< float    >(_unionValues.dFloatingPointValue) );
  case KnownTypes::Double:  return XMLSupport::ToString( static_cast< double   >(_unionValues.dFloatingPointValue) );
  default:                  throw InternalErrorException("Unexpected constant data type!");
  }
}

AST::BaseClasses::TypeInfo AST::Expressions::Constant::GetResultType() const
{
  BaseClasses::TypeInfo ResultType;

  ResultType.SetConst(true);
  ResultType.SetPointer(false);
  ResultType.SetType(GetValueType());

  return ResultType;
}


// Implementation of class AST::Expressions::Identifier
string AST::Expressions::Identifier::DumpToXML(const size_t cszIntend) const
{
  XMLSupport::AttributesMapType mapAttributes;

  mapAttributes["name"] = GetName();

  return XMLSupport::CreateXmlTag(cszIntend, "Identifier", mapAttributes);
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


// Implementation of class AST::Expressions::MemoryAccess
string AST::Expressions::MemoryAccess::DumpToXML(const size_t cszIntend) const
{
  string strXmlString  = _DumpResultTypeToXML(cszIntend + 2);
  strXmlString        += XMLSupport::CreateXmlTag( cszIntend + 2, "MemoryRef", _DumpChildToXml(GetMemoryReference(), cszIntend + 4) );
  strXmlString        += XMLSupport::CreateXmlTag( cszIntend + 2, "Index",     _DumpChildToXml(GetIndexExpression(), cszIntend + 4) );

  return XMLSupport::CreateXmlTag(cszIntend, "MemoryAccess", strXmlString);
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


// Implementation of class AST::Expressions::UnaryExpression
string AST::Expressions::UnaryExpression::_DumpSubExpressionToXML(const size_t cszIntend) const
{
  string strXmlString  = _DumpResultTypeToXML(cszIntend);
  strXmlString        += XMLSupport::CreateXmlTag( cszIntend, "SubExpression", _DumpChildToXml(GetSubExpression(), cszIntend + 2) );
  return strXmlString;
}

AST::BaseClasses::ExpressionPtr AST::Expressions::UnaryExpression::GetSubExpression(IndexType SubExprIndex)
{
  switch (SubExprIndex)
  {
  case 0:   return GetSubExpression();
  default:  throw ASTExceptions::ChildIndexOutOfRange();
  }
}


// Implementation of class AST::Expressions::Conversion
string AST::Expressions::Conversion::DumpToXML(const size_t cszIntend) const
{
  return XMLSupport::CreateXmlTag(cszIntend, "Conversion", _DumpSubExpressionToXML(cszIntend + 2));
}


// Implementation of class AST::Expressions::Parenthesis
string AST::Expressions::Parenthesis::DumpToXML(const size_t cszIntend) const
{
  return XMLSupport::CreateXmlTag(cszIntend, "Parenthesis", _DumpSubExpressionToXML(cszIntend + 2));
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


// Implementation of class AST::Expressions::UnaryOperator
string AST::Expressions::UnaryOperator::_GetOperatorTypeString(UnaryOperatorType eType)
{
  switch (eType)
  {
  case UnaryOperatorType::AddressOf:        return "AddressOf";
  case UnaryOperatorType::BitwiseNot:       return "BitwiseNot";
  case UnaryOperatorType::LogicalNot:       return "LogicalNot";
  case UnaryOperatorType::Minus:            return "Minus";
  case UnaryOperatorType::Plus:             return "Plus";
  case UnaryOperatorType::PostDecrement:    return "PostDecrement";
  case UnaryOperatorType::PostIncrement:    return "PostIncrement";
  case UnaryOperatorType::PreDecrement:     return "PreDecrement";
  case UnaryOperatorType::PreIncrement:     return "PreIncrement";
  default:                                  throw InternalErrorException("Unknown unary operator type!");
  }
}

string AST::Expressions::UnaryOperator::DumpToXML(const size_t cszIntend) const
{
  XMLSupport::AttributesMapType mapAttributes;
  mapAttributes["type"] = _GetOperatorTypeString( GetOperatorType() );
  return XMLSupport::CreateXmlTag(cszIntend, "UnaryOperator", _DumpSubExpressionToXML(cszIntend + 2), mapAttributes);
}

AST::BaseClasses::TypeInfo AST::Expressions::UnaryOperator::GetResultType() const
{
  if (! GetSubExpression())
  {
    return BaseClasses::TypeInfo(BaseClasses::TypeInfo::KnownTypes::Unknown);
  }

  if (GetOperatorType() == UnaryOperatorType::AddressOf)
  {
    return GetSubExpression()->GetResultType().CreatePointerType();
  }
  else if (GetOperatorType() == UnaryOperatorType::LogicalNot)
  {
    return BaseClasses::TypeInfo(BaseClasses::TypeInfo::KnownTypes::Bool, true, false);
  }
  else
  {
    BaseClasses::TypeInfo ReturnType = GetSubExpression()->GetResultType();

    if (! ReturnType.GetPointer())
    {
      ReturnType.SetConst(true);
    }

    return ReturnType;
  }
}


// Implementation of class AST::Expressions::BinaryOperator
string AST::Expressions::BinaryOperator::_DumpSubExpressionsToXML(const size_t cszIntend) const
{
  string strXmlString = _DumpResultTypeToXML(cszIntend);

  strXmlString += XMLSupport::CreateXmlTag( cszIntend, "LHS", _DumpChildToXml(GetLHS(), cszIntend + 2) );
  strXmlString += XMLSupport::CreateXmlTag( cszIntend, "RHS", _DumpChildToXml(GetRHS(), cszIntend + 2) );

  return strXmlString;
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


// Implementation of class AST::Expressions::ArithmeticOperator
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

string AST::Expressions::ArithmeticOperator::DumpToXML(const size_t cszIntend) const
{
  XMLSupport::AttributesMapType mapAttributes;

  mapAttributes["type"] = _GetOperatorTypeString(_eOpType);

  return XMLSupport::CreateXmlTag(cszIntend, "ArithmeticOperator", _DumpSubExpressionsToXML(cszIntend + 2), mapAttributes);
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
    else if (! TypeRHS.IsSingleValue())
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


// Implementation of class AST::Expressions::AssignmentOperator
string AST::Expressions::AssignmentOperator::DumpToXML(const size_t cszIntend) const
{
  return XMLSupport::CreateXmlTag( cszIntend, "AssignmentOperator", _DumpSubExpressionsToXML(cszIntend + 2) );
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


// Implementation of class AST::Expressions::RelationalOperator
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

string AST::Expressions::RelationalOperator::DumpToXML(const size_t cszIntend) const
{
  XMLSupport::AttributesMapType mapAttributes;

  mapAttributes["type"] = _GetOperatorTypeString(_eOpType);

  return XMLSupport::CreateXmlTag( cszIntend, "RelationalOperator", _DumpSubExpressionsToXML(cszIntend + 2), mapAttributes );
}

AST::BaseClasses::TypeInfo AST::Expressions::RelationalOperator::GetResultType() const
{
  return BaseClasses::TypeInfo( BaseClasses::TypeInfo::KnownTypes::Bool, true, false );
}


// Implementation of class AST::Expressions::FunctionCall
void AST::Expressions::FunctionCall::AddCallParameter(ExpressionPtr spCallParam)
{
  CHECK_NULL_POINTER(spCallParam);

  _SetParentToChild(spCallParam);
  _vecCallParams.push_back(spCallParam);
}

string AST::Expressions::FunctionCall::DumpToXML(const size_t cszIntend) const
{
  // Dump return type
  string strXmlString = XMLSupport::CreateXmlTag(cszIntend + 2, "ReturnType", GetReturnType().DumpToXML(cszIntend + 4));

  // Dump call parameters
  for (IndexType i = 0; i < GetCallParameterCount(); ++i)
  {
    XMLSupport::AttributesMapType mapParamAttributes;

    mapParamAttributes["index"] = XMLSupport::ToString(static_cast<unsigned int>(i));

    ExpressionPtr spParam = GetCallParameter(i);
    strXmlString += XMLSupport::CreateXmlTag(cszIntend + 2, "Param", spParam->DumpToXML(cszIntend + 4), mapParamAttributes);
  }

  XMLSupport::AttributesMapType mapAttributes;

  mapAttributes["name"] = GetName();

  return XMLSupport::CreateXmlTag(cszIntend, "FunctionCall", strXmlString, mapAttributes);
}

AST::BaseClasses::ExpressionPtr AST::Expressions::FunctionCall::GetCallParameter(IndexType CallParamIndex) const
{
  if (CallParamIndex >= GetSubExpressionCount())
  {
    throw ASTExceptions::ChildIndexOutOfRange();
  }
  else
  {
    return _vecCallParams[ CallParamIndex ];
  }
}



/*************************/
/***   Other classes   ***/
/*************************/

// Implementation of class AST::Scope
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

string AST::Scope::DumpToXML(const size_t cszIntend) const
{
  string strXmlString("");

  for each (auto itNode in _Children)
  {
    strXmlString += itNode->DumpToXML(cszIntend + 2);
  }

  return XMLSupport::CreateXmlTag(cszIntend, "Scope", strXmlString);
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


// Implementation of class AST::FunctionDeclaration
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

string AST::FunctionDeclaration::DumpToXML(const size_t cszIntend) const
{
  XMLSupport::AttributesMapType mapAttributes;

  mapAttributes["name"] = GetName();

  string strXmlString("");

  // Dump known variables
  {
    string strXmlVariables("");

    for each (auto itVariable in _mapKnownVariables)
    {
      strXmlVariables += itVariable.second->DumpToXML(cszIntend + 4);
    }

    strXmlString += XMLSupport::CreateXmlTag(cszIntend + 2, "KnownVariables", strXmlVariables);
  }

  // Dump parameters
  {
    string strXmlParams("");

    for each (auto itParameter in _Parameters)
    {
      strXmlParams += itParameter->DumpToXML(cszIntend + 4);
    }

    strXmlString += XMLSupport::CreateXmlTag(cszIntend + 2, "Parameters", strXmlParams);
  }

  // Dump body
  strXmlString += XMLSupport::CreateXmlTag( cszIntend + 2, "Body", _DumpChildToXml(GetBody(), cszIntend + 4) );

  return XMLSupport::CreateXmlTag(cszIntend, "FunctionDeclaration", strXmlString, mapAttributes);
}

AST::ScopePtr AST::FunctionDeclaration::GetBody()
{
  if (!_spBody)
  {
    _SetChildPtr(_spBody, AST::CreateNode<Scope>());
  }

  return _spBody;
}

const AST::ScopePtr AST::FunctionDeclaration::GetBody() const
{
  CHECK_NULL_POINTER(_spBody);

  return _spBody;
}

AST::BaseClasses::NodePtr AST::FunctionDeclaration::GetChild(IndexType ChildIndex)
{
  switch (ChildIndex)
  {
  case 0:   return GetBody();
  default:  throw ASTExceptions::ChildIndexOutOfRange();
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


// vim: set ts=2 sw=2 sts=2 et ai:
