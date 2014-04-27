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

      class VariableInfo
      {
      private:

        std::string _strName;

      public:

        inline std::string  GetName() const               { return _strName; }
        inline void         SetName(std::string strName)  { _strName = strName; }

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
      class AssignmentOperator;

      typedef std::shared_ptr< Value >                ValuePtr;
      typedef std::shared_ptr< Constant >             ConstantPtr;
      typedef std::shared_ptr< Identifier >           IdentifierPtr;
      typedef std::shared_ptr< AssignmentOperator >   AssignmentOperatorPtr;


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

      public:

        inline Constant() : BaseType(BaseType::ValueType::Constant)   {}

        virtual std::string GetAsString() const final override { return ""; } // TODO: Unfinished

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

      public:

        inline ArithmeticOperator() : BaseType(BaseType::BinaryOperatorType::ArithmeticOperator)  {}


        inline ArithmeticOperatorType GetOperatorType() const                           { return _eOpType; }
        inline void                   SetOperatorType(ArithmeticOperatorType eOpType)   { _eOpType = eOpType; }
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

      public:

        inline RelationalOperator() : BaseType(BaseType::BinaryOperatorType::RelationalOperator)  {}


        inline RelationalOperatorType GetOperatorType() const                           { return _eOpType; }
        inline void                   SetOperatorType(RelationalOperatorType eOpType)   { _eOpType = eOpType; }
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

