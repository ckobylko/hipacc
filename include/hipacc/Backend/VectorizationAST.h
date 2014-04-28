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

//===--- VectorizationAST.h - Implements a vectorizable syntax tree. -----------------===//
//
// This file implements the internally used vectorizable syntax tree (a simplification to clang's AST)
//
//===---------------------------------------------------------------------------------===//

#ifndef _BACKEND_VECTORIZATION_AST_H_
#define _BACKEND_VECTORIZATION_AST_H_

#include "llvm/Support/Casting.h"
#include "BackendExceptions.h"
#include <cstdint>
#include <map>
#include <memory>
#include <string>
#include <vector>
#include <type_traits>

namespace clang
{
namespace hipacc
{
namespace Backend
{
namespace Vectorization
{
  class ASTExceptions
  {
  public:

    class ChildIndexOutOfRange : public InternalErrorException
    {
    private:

      typedef InternalErrorException  BaseType;   //!< The base type of this class.

    public:

      inline ChildIndexOutOfRange() : BaseType("The index for the child node is out of range!")  {}
    };
  };


  class AST
  {
  // Forward declarations and type definitions
  public:

    class FunctionDeclaration;
    class Scope;

    typedef std::shared_ptr< FunctionDeclaration >  FunctionDeclarationPtr;
    typedef std::shared_ptr< Scope >                ScopePtr;


  // Class definitions
  public:

    class BaseClasses final
    {
    // Forward declarations and type definitions
    public:

      class Node;
      class Expression;
      class VariableInfo;

      typedef std::shared_ptr< Node >           NodePtr;
      typedef std::shared_ptr< Expression >     ExpressionPtr;
      typedef std::shared_ptr< VariableInfo >   VariableInfoPtr;


    public:

      class TypeInfo
      {
      public:

        typedef std::vector< size_t >    ArrayDimensionVectorType;

        enum class KnownTypes
        {
          Bool,
          Int8,
          UInt8,
          Int16,
          UInt16,
          Int32,
          UInt32,
          Int64,
          UInt64,
          Float,
          Double
        };


      private:

        KnownTypes                _eType;
        bool                      _bIsConst;
        bool                      _bIsPointer;
        ArrayDimensionVectorType  _vecArrayDimensions;

        static std::string _GetBoolString(bool bValue);

      public:

        inline TypeInfo() : _eType(KnownTypes::Int32), _bIsConst(false), _bIsPointer(false)   {}
        inline TypeInfo(const TypeInfo &crRVal)   { *this = crRVal; }
        TypeInfo& operator=(const TypeInfo &crRVal);


        inline bool IsArray() const       { return (! _vecArrayDimensions.empty()); }

        inline ArrayDimensionVectorType&        GetArrayDimensions()        { return _vecArrayDimensions; }
        inline const ArrayDimensionVectorType&  GetArrayDimensions() const  { return _vecArrayDimensions; }

        inline bool GetConst() const          { return _bIsConst; }
        inline void SetConst(bool bIsConst)   { _bIsConst = bIsConst; }

        inline bool GetPointer() const            { return _bIsPointer; }
        inline void SetPointer(bool bIsPointer)   { _bIsPointer = bIsPointer; }

        inline KnownTypes GetType() const             { return _eType; }
        inline void       SetType(KnownTypes eType)   { _eType = eType; }

        std::string DumpToXML(size_t szIntend);


        static std::string GetTypeString(KnownTypes eType);
      };

      class VariableInfo
      {
      private:

        std::string _strName;
        TypeInfo    _Type;

      public:

        inline std::string  GetName() const               { return _strName; }
        inline void         SetName(std::string strName)  { _strName = strName; }

        inline TypeInfo&        GetTypeInfo()       { return _Type; }
        inline const TypeInfo&  GetTypeInfo() const { return _Type; }

        std::string DumpToXML(size_t szIntend);
      };

      class Node
      {
      private:

        friend class AST;

      public:

        typedef size_t                    IndexType;

        enum class NodeType
        {
          FunctionDeclaration,
          Expression,
          Scope,
          Value
        };


      private:

        const NodeType          _ceNodeType;
        std::weak_ptr< Node >   _wpParent;
        std::weak_ptr< Node >   _wpThis;


        inline void _SetParent(NodePtr spParent)        { _wpParent = spParent; }


      protected:

        inline Node(NodeType eType) : _ceNodeType(eType)    {}


        void _RemoveParentFromChild(NodePtr spChild);
        void _SetParentToChild(NodePtr spChild);


      public:

        inline NodeType GetNodeType() const    { return _ceNodeType; }

        NodePtr GetParent();
        inline NodePtr GetThis()  { return _wpThis.lock(); }


        template <class NodeClass>
        inline NodeClass* CastToType()  { return dynamic_cast<NodeClass*>(this); }


      public:

        virtual NodePtr     GetChild(IndexType ChildIndex) = 0;
        virtual IndexType   GetChildCount() const = 0;


        virtual std::string DumpToXML(size_t szIntend) = 0;
      };

      class Expression : public Node
      {
      protected:

        typedef Node                  BaseType;
        typedef BaseType::IndexType   IndexType;

      public:

        enum class ExpressionType
        {
          BinaryOperator,
          Value,
          UnaryOperator
        };


      private:

        const ExpressionType    _ceExprType;

      public:

        inline Expression(ExpressionType eExprType) : BaseType(Node::NodeType::Expression), _ceExprType(eExprType)  {}

      public:

        virtual NodePtr   GetChild(IndexType ChildIndex) final override   { return GetSubExpression(ChildIndex); }
        virtual IndexType GetChildCount() const final override            { return GetSubExpressionCount(); }

        virtual ExpressionPtr   GetSubExpression(IndexType SubExprIndex) = 0;
        virtual IndexType       GetSubExpressionCount() const = 0;

      };

    };


    class Expressions final
    {
    // Forward declarations and type definitions
    public:

      class Value;
      class Constant;
      class Identifier;
      class BinaryOperator;
      class ArithmeticOperator;
      class AssignmentOperator;
      class RelationalOperator;

      typedef std::shared_ptr< Value >                ValuePtr;
      typedef std::shared_ptr< Constant >             ConstantPtr;
      typedef std::shared_ptr< Identifier >           IdentifierPtr;
      typedef std::shared_ptr< BinaryOperator >       BinaryOperatorPtr;
      typedef std::shared_ptr< ArithmeticOperator >   ArithmeticOperatorPtr;
      typedef std::shared_ptr< AssignmentOperator >   AssignmentOperatorPtr;
      typedef std::shared_ptr< RelationalOperator >   RelationalOperatorPtr;


    public:

      class Value : public BaseClasses::Expression
      {
      private:

        typedef BaseClasses::Expression   BaseType;
        typedef BaseType::IndexType       IndexType;

      public:

        enum class ValueType
        {
          Constant,
          Identifier,
          MemoryAccess
        };

      private:

        const ValueType   _ceValueType;

      public:

        inline  Value(ValueType eValueType) : BaseType(BaseType::ExpressionType::Value), _ceValueType(eValueType)   {}

        virtual std::string GetAsString() const = 0;

      public:

        virtual BaseClasses::ExpressionPtr  GetSubExpression(IndexType SubExprIndex) final override;
        virtual IndexType                   GetSubExpressionCount() const final override  { return static_cast< IndexType >(0); }
      };

      class Constant final : public Value
      {
      private:

        typedef Value  BaseType;

        typedef BaseClasses::TypeInfo::KnownTypes   KnownTypes;

        union
        {
          std::uint64_t ui64IntegralValue;
          double        dFloatingPointValue;
        } _unionValues;

        KnownTypes    _eType;

      public:

        inline Constant() : BaseType(BaseType::ValueType::Constant)   {}

        inline BaseClasses::TypeInfo::KnownTypes  GetValueType() const    { return _eType; }


        template <typename ValueType> inline ValueType GetValue() const
        {
          static_assert(std::is_arithmetic< ValueType >::value, "Expected a numeric value type!");

          switch (_eType)
          {
          case KnownTypes::Float: case KnownTypes::Double:
            return static_cast< ValueType >( _unionValues.dFloatingPointValue );
          default:
            return static_cast< ValueType >( _unionValues.ui64IntegralValue );
          }
        }

        template <>                   inline bool      GetValue<bool>() const
        {
          switch (_eType)
          {
          case KnownTypes::Float: case KnownTypes::Double:
            return ( _unionValues.dFloatingPointValue != 0. );
          default:
            return ( _unionValues.ui64IntegralValue   != static_cast< std::uint64_t >( 0 ) );
          }
        }


        template <typename ValueType> inline void SetValue(ValueType TValue)
        {
          static_assert(std::is_arithmetic< ValueType >::value, "Expected a numeric value type!");

          if (std::is_integral<ValueType>::value)
          {
            _unionValues.ui64IntegralValue  = static_cast< std::uint64_t >( TValue );

            bool bSigned = std::numeric_limits< ValueType >::is_signed;

            switch (sizeof(ValueType))
            {
            case 1:   _eType = bSigned ? KnownTypes::Int8  : KnownTypes::UInt8;   break;
            case 2:   _eType = bSigned ? KnownTypes::Int16 : KnownTypes::UInt16;  break;
            case 4:   _eType = bSigned ? KnownTypes::Int32 : KnownTypes::UInt32;  break;
            default:  _eType = bSigned ? KnownTypes::Int64 : KnownTypes::UInt64;  break;
            }
          }
          else
          {
            _unionValues.dFloatingPointValue = static_cast< double >( TValue );

            _eType = (sizeof(ValueType) == 4) ? KnownTypes::Float : KnownTypes::Double;
          }
        }

        template<>                    inline void SetValue<bool>(bool TValue)
        {
          _unionValues.ui64IntegralValue  = static_cast< std::uint64_t >( TValue ? 1 : 0 );
          _eType                          = KnownTypes::Bool;
        }



        virtual std::string GetAsString() const final override;

        virtual std::string DumpToXML(size_t szIntend) final override;
      };

      class Identifier final : public Value
      {
      private:

        typedef Value  BaseType;

        std::string   _strName;

      public:

        inline Identifier() : BaseType(BaseType::ValueType::Identifier)   {}

        inline std::string  GetName() const               { return _strName; }
        inline void         SetName(std::string strName)  { _strName = strName; }

        virtual std::string GetAsString() const final override { return GetName(); }

        virtual std::string DumpToXML(size_t szIntend) final override;
      };

      class MemoryAccess final : public Value
      {
      private:

        typedef Value  BaseType;

      public:

        inline MemoryAccess() : BaseType(BaseType::ValueType::MemoryAccess)  {}
      };


      class UnaryOperator final : public BaseClasses::Expression
      {
      private:

        typedef BaseClasses::Expression   BaseType;
        typedef BaseType::IndexType       IndexType;


      public:

        enum class UnaryOperatorType
        {
          AddressOf,
          BitwiseNot,
          Dereference,
          LogicalNot,
          Minus,
          Plus,
          PostDecrement,
          PostIncrement,
          PreDecrement,
          PreIncrement
        };

      private:

        UnaryOperatorType    _eOpType;

      public:

        inline UnaryOperator() : BaseType(BaseType::ExpressionType::UnaryOperator)  {}


        inline UnaryOperatorType  GetOperatorType() const                     { return _eOpType; }
        inline void               SetOperatorType(UnaryOperatorType eOpType)  { _eOpType = eOpType; }
      };

      class BinaryOperator : public BaseClasses::Expression
      {
      private:

        typedef BaseClasses::Expression     BaseType;
        typedef BaseType::IndexType         IndexType;
        typedef BaseClasses::ExpressionPtr  ExpressionPtr;

      public:

        enum class BinaryOperatorType
        {
          ArithmeticOperator,
          AssignmentOperator,
          RelationalOperator
        };


      private:

        const BinaryOperatorType  _ceBinOpType;
        ExpressionPtr             _spLHS;
        ExpressionPtr             _spRHS;

      protected:

        std::string _DumpSubExpressionsToXML(size_t szIntend);

      public:

        inline BinaryOperator(BinaryOperatorType eBinOpType) : BaseType(BaseType::ExpressionType::BinaryOperator), _ceBinOpType(eBinOpType)   {}


        inline ExpressionPtr  GetLHS()  { return _spLHS; }
        inline ExpressionPtr  GetRHS()  { return _spRHS; }

        void SetLHS(ExpressionPtr spNewLHS);
        void SetRHS(ExpressionPtr spNewRHS);


      public:

        virtual BaseClasses::ExpressionPtr  GetSubExpression(IndexType SubExprIndex) final override;
        virtual IndexType                   GetSubExpressionCount() const final override  { return static_cast< IndexType >(2); }

      };

      class ArithmeticOperator final : public BinaryOperator
      {
      private:

        typedef BinaryOperator   BaseType;

      public:

        enum class ArithmeticOperatorType
        {
          Add,
          BitwiseAnd,
          BitwiseOr,
          BitwiseXOr,
          Divide,
          Modulo,
          Multiply,
          ShiftLeft,
          ShiftRight,
          Subtract
        };


      private:

        ArithmeticOperatorType    _eOpType;

        static std::string _GetOperatorTypeString(ArithmeticOperatorType eType);

      public:

        inline ArithmeticOperator() : BaseType(BaseType::BinaryOperatorType::ArithmeticOperator)  {}


        inline ArithmeticOperatorType GetOperatorType() const                           { return _eOpType; }
        inline void                   SetOperatorType(ArithmeticOperatorType eOpType)   { _eOpType = eOpType; }


      public:

        virtual std::string DumpToXML(size_t szIntend) final override;

      };

      class AssignmentOperator final : public BinaryOperator
      {
      private:

        typedef BinaryOperator   BaseType;

      public:

        inline AssignmentOperator() : BaseType(BaseType::BinaryOperatorType::AssignmentOperator)  {}


      public:

        virtual std::string DumpToXML(size_t szIntend) final override;

      };

      class RelationalOperator final : public BinaryOperator
      {
      private:

        typedef BinaryOperator   BaseType;

      public:

        enum class RelationalOperatorType
        {
          Equal,
          Greater,
          GreaterEqual,
          Less,
          LessEqual,
          LogicalAnd,
          LogicalOr,
          NotEqual
        };

      private:

        RelationalOperatorType  _eOpType;

        static std::string _GetOperatorTypeString(RelationalOperatorType eType);

      public:

        inline RelationalOperator() : BaseType(BaseType::BinaryOperatorType::RelationalOperator)  {}


        inline RelationalOperatorType GetOperatorType() const                           { return _eOpType; }
        inline void                   SetOperatorType(RelationalOperatorType eOpType)   { _eOpType = eOpType; }


      public:

        virtual std::string DumpToXML(size_t szIntend) final override;

      };
    };


    class Scope final : public BaseClasses::Node
    {
    private:

      typedef BaseClasses::Node       BaseType;
      typedef BaseType::IndexType     IndexType;
      typedef BaseClasses::NodePtr    NodePtr;

      typedef std::vector< NodePtr >  ChildrenContainerType;


    private:

      ChildrenContainerType   _Children;


    public:

      inline Scope() : BaseType(Node::NodeType::Scope)   {}

      void AddChild(NodePtr spChild);
      void AddVariable(BaseClasses::VariableInfoPtr spVariableInfo);

    public:

      virtual NodePtr       GetChild(IndexType ChildIndex) final override;
      virtual IndexType     GetChildCount() const final override  { return static_cast< IndexType >(_Children.size()); }


      virtual std::string DumpToXML(size_t szIntend) final override;
    };


    class FunctionDeclaration : public BaseClasses::Node
    {
    private:

      typedef BaseClasses::Node     BaseType;
      typedef BaseType::IndexType   IndexType;
      typedef BaseClasses::NodePtr  NodePtr;

      typedef std::vector< Expressions::IdentifierPtr >              ParameterContainerType;
      typedef std::map< std::string, BaseClasses::VariableInfoPtr >  KnownVariablesMapType;


    private:

      ParameterContainerType  _Parameters;
      ScopePtr                _spBody;
      KnownVariablesMapType   _mapKnownVariables;
      std::string             _strName;



    public:

      FunctionDeclaration();

      void AddParameter(BaseClasses::VariableInfoPtr spVariableInfo);
      void AddVariable(BaseClasses::VariableInfoPtr spVariableInfo);

      ScopePtr GetBody();

      inline std::string  GetName() const               { return _strName; }
      inline void         SetName(std::string strName)  { _strName = strName; }

    public:

      virtual NodePtr       GetChild(IndexType ChildIndex) final override;
      virtual IndexType     GetChildCount() const final override  { return static_cast< IndexType >(1); }


      virtual std::string DumpToXML(size_t szIntend) final override;
    };




    public:

      template < class NodeClass >
      inline static std::shared_ptr< NodeClass > CreateNode()
      {
        static_assert(std::is_base_of< BaseClasses::Node, NodeClass >::value, "All nodes of the vectorizable AST must be derived from class \"Node\"");

        std::shared_ptr< NodeClass > spNode = std::make_shared< NodeClass >();

        spNode->_wpThis = BaseClasses::NodePtr( spNode );

        return spNode;
      }
  };
} // end namespace Vectorization
} // end namespace Backend
} // end namespace hipacc
} // end namespace clang


#endif  // _BACKEND_VECTORIZATION_AST_H_

// vim: set ts=2 sw=2 sts=2 et ai:

