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


    class NonDereferencableType : public RuntimeErrorException
    {
    private:

      typedef RuntimeErrorException  BaseType;   //!< The base type of this class.

    public:

      inline NonDereferencableType() : BaseType("The specified type cannot be dereferenced!")  {}
    };

    class UnknownExpressionClass : public RuntimeErrorException
    {
    private:

      typedef RuntimeErrorException  BaseType;   //!< The base type of this class.

    public:

      inline UnknownExpressionClass(std::string strExprClassName) : BaseType( std::string( "The expression class \"") + strExprClassName + std::string("\" is unknown!") )  {}
    };

    class UnknownStatementClass : public RuntimeErrorException
    {
    private:

      typedef RuntimeErrorException  BaseType;   //!< The base type of this class.

    public:

      inline UnknownStatementClass(std::string strStmtClassName) : BaseType(std::string("The statement class \"") + strStmtClassName + std::string("\" is unknown!"))  {}
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
          Double,
          Unknown
        };


      private:

        KnownTypes                _eType;
        bool                      _bIsConst;
        bool                      _bIsPointer;
        ArrayDimensionVectorType  _vecArrayDimensions;


      public:

        inline TypeInfo(KnownTypes eType = KnownTypes::Unknown, bool bIsConst = false, bool bIsPointer = false)
        {
          _eType      = eType;
          _bIsConst   = bIsConst;
          _bIsPointer = bIsPointer;
        }

        inline TypeInfo(const TypeInfo &crRVal)   { *this = crRVal; }
        TypeInfo& operator=(const TypeInfo &crRVal);


        TypeInfo CreateDereferencedType() const;
        TypeInfo CreatePointerType() const;


        inline ArrayDimensionVectorType&        GetArrayDimensions()        { return _vecArrayDimensions; }
        inline const ArrayDimensionVectorType&  GetArrayDimensions() const  { return _vecArrayDimensions; }

        inline bool GetConst() const          { return _bIsConst; }
        inline void SetConst(bool bIsConst)   { _bIsConst = bIsConst; }

        inline bool GetPointer() const            { return _bIsPointer; }
        inline void SetPointer(bool bIsPointer)   { _bIsPointer = bIsPointer; }

        inline KnownTypes GetType() const             { return _eType; }
        inline void       SetType(KnownTypes eType)   { _eType = eType; }

        inline bool IsArray() const           { return (!_vecArrayDimensions.empty()); }
        inline bool IsDereferencable() const  { return (IsArray() || GetPointer()); }
        inline bool IsSingleValue() const     { return (! IsDereferencable()); }


        std::string DumpToXML(const size_t cszIntend) const;


      public:

        static TypeInfo     CreateSizedIntegerType(size_t szTypeSize, bool bSigned);

        static size_t       GetTypeSize(KnownTypes eType);
        static std::string  GetTypeString(KnownTypes eType);

        static bool         IsSigned(KnownTypes eType);
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

        std::string DumpToXML(const size_t cszIntend) const;
      };

      class Node
      {
      private:

        friend class AST;

      public:

        typedef size_t                    IndexType;

        enum class NodeType
        {
          ControlFlowStatement,
          Expression,
          FunctionDeclaration,
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


        static std::string _DumpChildToXml(const NodePtr spChild, const size_t cszIntend);

        template < typename NodeClassPtr >
        inline void _SetChildPtr(NodeClassPtr &rDestinationPtr, const NodeClassPtr &crSourcePtr)
        {
          // If the child is set, remove its parent pointer
          if (rDestinationPtr)
          {
            rDestinationPtr->_SetParent(nullptr);
          }

          // Set the child pointer and set the current node as its parent
          rDestinationPtr = crSourcePtr;
          _SetParentToChild(rDestinationPtr);
        }

        void _SetParentToChild(NodePtr spChild) const;


      public:

        inline NodeType GetNodeType() const    { return _ceNodeType; }

        NodePtr       GetParent();
        const NodePtr GetParent() const;

        inline NodePtr        GetThis()         { return _wpThis.lock(); }
        inline const NodePtr  GetThis() const   { return _wpThis.lock(); }


        template <class NodeClass>
        inline NodeClass* CastToType()
        {
          if (! IsType<NodeClass>())
          {
            throw RuntimeErrorException("Invalid node cast type!");
          }

          return dynamic_cast<NodeClass*>(this);
        }

        template <class NodeClass>
        inline bool IsType() const
        {
          static_assert( std::is_base_of< Node, NodeClass >::value, "All VAST nodes must be derived from class \"Node\"!" );

          return (dynamic_cast< const NodeClass* >(this) != nullptr);
        }


      public:

        virtual NodePtr     GetChild(IndexType ChildIndex) = 0;
        virtual IndexType   GetChildCount() const = 0;


        virtual std::string DumpToXML(const size_t cszIntend) const = 0;
      };

      class ControlFlowStatement : public Node
      {
      protected:

        typedef Node                  BaseType;
        typedef BaseType::IndexType   IndexType;

      public:

        enum class ControlFlowType
        {
          Loop
        };
        
      private:

        const ControlFlowType    _ceControlFlowType;

      public:

        inline ControlFlowStatement(ControlFlowType eCtlFlowType) : BaseType(Node::NodeType::ControlFlowStatement), _ceControlFlowType(eCtlFlowType)  {}

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
          FunctionCall,
          UnaryExpression,
          Value
        };


      private:

        const ExpressionType    _ceExprType;

      protected:

        std::string _DumpResultTypeToXML(const size_t cszIntend) const;


      public:

        inline Expression(ExpressionType eExprType) : BaseType(Node::NodeType::Expression), _ceExprType(eExprType)  {}

      public:

        virtual NodePtr   GetChild(IndexType ChildIndex) final override   { return GetSubExpression(ChildIndex); }
        virtual IndexType GetChildCount() const final override            { return GetSubExpressionCount(); }

        virtual TypeInfo  GetResultType() const = 0;

        virtual ExpressionPtr   GetSubExpression(IndexType SubExprIndex) = 0;
        virtual IndexType       GetSubExpressionCount() const = 0;
      };
    };


    class ControlFlow final
    {
      // Forward declarations and type definitions
    public:

      class Loop;

      typedef std::shared_ptr< Loop >   LoopPtr;

    public:

      class Loop final : public BaseClasses::ControlFlowStatement
      {
      private:

        typedef BaseClasses::ControlFlowStatement   BaseType;
        typedef BaseType::IndexType                 IndexType;

      public:
        
        enum class LoopType
        {
          TopControlled,
          BottomControlled
        };

      private:

        LoopType                    _eLoopType;
        BaseClasses::ExpressionPtr  _spConditionExpr;
        BaseClasses::ExpressionPtr  _spIncrementExpr;
        ScopePtr                    _spBody;


        static std::string _GetLoopTypeString(LoopType eType);


      public:

        inline Loop() : BaseType(BaseType::ControlFlowType::Loop)   {}


        ScopePtr        GetBody();
        const ScopePtr  GetBody() const;

        inline BaseClasses::ExpressionPtr         GetCondition()                                        { return _spConditionExpr; }
        inline const BaseClasses::ExpressionPtr   GetCondition() const                                  { return _spConditionExpr; }
        inline void                               SetCondition(BaseClasses::ExpressionPtr spCondition)  { _SetChildPtr(_spConditionExpr, spCondition); }

        inline BaseClasses::ExpressionPtr         GetIncrement()                                        { return _spIncrementExpr; }
        inline const BaseClasses::ExpressionPtr   GetIncrement() const                                  { return _spIncrementExpr; }
        inline void                               SetIncrement(BaseClasses::ExpressionPtr spIncrement)  { _SetChildPtr(_spIncrementExpr, spIncrement); }

        inline LoopType GetLoopType() const           { return _eLoopType; }
        inline void     SetLoopType(LoopType eType)   { _eLoopType = eType; }


      public:

        virtual std::string DumpToXML(const size_t cszIntend) const final override;

        virtual BaseClasses::NodePtr  GetChild(IndexType ChildIndex) final override;
        virtual IndexType             GetChildCount() const final override    { return static_cast< IndexType >( 3 ); }
      };
    };


    class Expressions final
    {
    // Forward declarations and type definitions
    public:

      class Value;
      class Constant;
      class Identifier;
      class MemoryAccess;
      class Conversion;
      class Parenthesis;
      class UnaryOperator;
      class BinaryOperator;
      class ArithmeticOperator;
      class AssignmentOperator;
      class RelationalOperator;
      class FunctionCall;

      typedef std::shared_ptr< Value >                ValuePtr;
      typedef std::shared_ptr< Constant >             ConstantPtr;
      typedef std::shared_ptr< Identifier >           IdentifierPtr;
      typedef std::shared_ptr< MemoryAccess >         MemoryAccessPtr;
      typedef std::shared_ptr< Conversion >           ConversionPtr;
      typedef std::shared_ptr< Parenthesis >          ParenthesisPtr;
      typedef std::shared_ptr< UnaryOperator >        UnaryOperatorPtr;
      typedef std::shared_ptr< BinaryOperator >       BinaryOperatorPtr;
      typedef std::shared_ptr< ArithmeticOperator >   ArithmeticOperatorPtr;
      typedef std::shared_ptr< AssignmentOperator >   AssignmentOperatorPtr;
      typedef std::shared_ptr< RelationalOperator >   RelationalOperatorPtr;
      typedef std::shared_ptr< FunctionCall >         FunctionCallPtr;


    public:

      class Value : public BaseClasses::Expression
      {
      protected:

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

      public:

        virtual BaseClasses::ExpressionPtr  GetSubExpression(IndexType SubExprIndex) override;
        virtual IndexType                   GetSubExpressionCount() const override  { return static_cast< IndexType >(0); }
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



        std::string GetAsString() const;

        virtual BaseClasses::TypeInfo GetResultType() const final override;

        virtual std::string DumpToXML(const size_t cszIntend) const final override;
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

        BaseClasses::VariableInfoPtr LookupVariableInfo() const;


        virtual BaseClasses::TypeInfo GetResultType() const final override;

        virtual std::string DumpToXML(const size_t cszIntend) const final override;
      };

      class MemoryAccess final : public Value
      {
      private:

        typedef Value                         BaseType;
        typedef BaseType::IndexType           IndexType;
        typedef BaseClasses::ExpressionPtr    ExpressionPtr;

        ExpressionPtr   _spMemoryRef;
        ExpressionPtr   _spIndexExpr;

      public:

        inline MemoryAccess() : BaseType(BaseType::ValueType::MemoryAccess), _spMemoryRef(nullptr), _spIndexExpr(nullptr)   {}


        inline ExpressionPtr        GetIndexExpression()                                  { return _spIndexExpr; }
        inline const ExpressionPtr  GetIndexExpression() const                            { return _spIndexExpr; }
        inline void                 SetIndexExpression(ExpressionPtr spIndexExpression)   { _SetChildPtr(_spIndexExpr, spIndexExpression); }

        inline ExpressionPtr        GetMemoryReference()                                  { return _spMemoryRef; }
        inline const ExpressionPtr  GetMemoryReference() const                            { return _spMemoryRef; }
        inline void                 SetMemoryReference(ExpressionPtr spMemoryReference)   { _SetChildPtr(_spMemoryRef, spMemoryReference); }


        virtual BaseClasses::TypeInfo GetResultType() const final override;

        virtual BaseClasses::ExpressionPtr  GetSubExpression(IndexType SubExprIndex) final override;
        virtual IndexType                   GetSubExpressionCount() const final override  { return static_cast< IndexType >(2); }


        virtual std::string DumpToXML(const size_t cszIntend) const final override;
      };


      class UnaryExpression : public BaseClasses::Expression
      {
      protected:

        typedef BaseClasses::Expression   BaseType;
        typedef BaseType::IndexType       IndexType;

      public:

        enum class UnaryExpressionType
        {
          Conversion,
          Parenthesis,
          UnaryOperator
        };

      private:

        const UnaryExpressionType   _ceUnaryExprType;
        BaseClasses::ExpressionPtr  _spSubExpression;

      protected:

        std::string _DumpSubExpressionToXML(const size_t cszIntend) const;

      public:

        inline UnaryExpression(UnaryExpressionType eType) : BaseType(BaseType::ExpressionType::UnaryExpression), _ceUnaryExprType(eType), _spSubExpression(nullptr)  {}

        inline BaseClasses::ExpressionPtr         GetSubExpression()                                      { return _spSubExpression; }
        inline const BaseClasses::ExpressionPtr   GetSubExpression() const                                { return _spSubExpression; }
        inline void                               SetSubExpression(BaseClasses::ExpressionPtr spSubExpr)  { _SetChildPtr(_spSubExpression, spSubExpr); }

        virtual BaseClasses::ExpressionPtr  GetSubExpression(IndexType SubExprIndex) final override;
        virtual IndexType                   GetSubExpressionCount() const final override  { return static_cast< IndexType >( 1 ); }

      };

      class Conversion final : public UnaryExpression
      {
      private:

        typedef UnaryExpression           BaseType;
        typedef BaseType::IndexType       IndexType;


        BaseClasses::TypeInfo   _ConvertType;

      public:

        inline Conversion() : BaseType(BaseType::UnaryExpressionType::Conversion)   {}

        inline BaseClasses::TypeInfo  GetConvertType() const                                    { return _ConvertType; }
        inline void                   SetConvertType(const BaseClasses::TypeInfo &crConvType)   { _ConvertType = crConvType; }

      public:

        virtual BaseClasses::TypeInfo GetResultType() const final override    { return GetConvertType(); }

        virtual std::string DumpToXML(const size_t cszIntend) const final override;
      };

      class Parenthesis final : public UnaryExpression
      {
      private:

        typedef UnaryExpression   BaseType;

      public:

        inline Parenthesis() : BaseType(BaseType::UnaryExpressionType::Parenthesis)   {}


      public:

        virtual BaseClasses::TypeInfo GetResultType() const final override;

        virtual std::string DumpToXML(const size_t cszIntend) const final override;
      };

      class UnaryOperator final : public UnaryExpression
      {
      private:

        typedef UnaryExpression           BaseType;
        typedef BaseType::IndexType       IndexType;


      public:

        enum class UnaryOperatorType
        {
          AddressOf,
          BitwiseNot,
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

        static std::string _GetOperatorTypeString(UnaryOperatorType eType);


      public:

        inline UnaryOperator() : BaseType(BaseType::UnaryExpressionType::UnaryOperator)  {}


        inline UnaryOperatorType  GetOperatorType() const                     { return _eOpType; }
        inline void               SetOperatorType(UnaryOperatorType eOpType)  { _eOpType = eOpType; }


      public:

        virtual std::string DumpToXML(const size_t cszIntend) const final override;

        virtual BaseClasses::TypeInfo GetResultType() const final override;
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

        std::string _DumpSubExpressionsToXML(const size_t cszIntend) const;

      public:

        inline BinaryOperator(BinaryOperatorType eBinOpType) : BaseType(BaseType::ExpressionType::BinaryOperator), _ceBinOpType(eBinOpType)   {}


        inline ExpressionPtr        GetLHS()                        { return _spLHS; }
        inline const ExpressionPtr  GetLHS() const                  { return _spLHS; }
        inline void                 SetLHS(ExpressionPtr spNewLHS)  { _SetChildPtr(_spLHS, spNewLHS); }

        inline ExpressionPtr        GetRHS()                        { return _spRHS; }
        inline const ExpressionPtr  GetRHS() const                  { return _spRHS; }
        inline void                 SetRHS(ExpressionPtr spNewRHS)  { _SetChildPtr(_spRHS, spNewRHS); }


      public:

        virtual BaseClasses::ExpressionPtr  GetSubExpression(IndexType SubExprIndex) final override;
        virtual IndexType                   GetSubExpressionCount() const final override  { return static_cast< IndexType >(2); }

      };

      class ArithmeticOperator final : public BinaryOperator
      {
      private:

        typedef BinaryOperator          BaseType;
        typedef BaseClasses::TypeInfo   TypeInfo;
        typedef TypeInfo::KnownTypes    KnownTypes;

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

        static KnownTypes _GetPromotedType(KnownTypes eTypeLHS, KnownTypes eTypeRHS);


      public:

        inline ArithmeticOperator() : BaseType(BaseType::BinaryOperatorType::ArithmeticOperator)  {}


        inline ArithmeticOperatorType GetOperatorType() const                           { return _eOpType; }
        inline void                   SetOperatorType(ArithmeticOperatorType eOpType)   { _eOpType = eOpType; }


      public:

        virtual BaseClasses::TypeInfo GetResultType() const final override;

        virtual std::string DumpToXML(const size_t cszIntend) const final override;

      };

      class AssignmentOperator final : public BinaryOperator
      {
      private:

        typedef BinaryOperator    BaseType;

      public:

        inline AssignmentOperator() : BaseType(BaseType::BinaryOperatorType::AssignmentOperator)  {}


      public:

        virtual BaseClasses::TypeInfo GetResultType() const final override;

        virtual std::string DumpToXML(const size_t cszIntend) const final override;

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

        virtual BaseClasses::TypeInfo GetResultType() const final override;

        virtual std::string DumpToXML(const size_t cszIntend) const final override;

      };


      class FunctionCall : public BaseClasses::Expression
      {
      private:

        typedef BaseClasses::Expression     BaseType;
        typedef BaseType::IndexType         IndexType;
        typedef BaseClasses::ExpressionPtr  ExpressionPtr;

      private:

        std::string                     _strName;
        BaseClasses::TypeInfo           _ReturnType;
        std::vector< ExpressionPtr >    _vecCallParams;

      public:

        inline FunctionCall() : BaseType(BaseType::ExpressionType::FunctionCall)    {}


        void          AddCallParameter(ExpressionPtr spCallParam);
        ExpressionPtr GetCallParameter(IndexType CallParamIndex) const;

        inline IndexType GetCallParameterCount() const    { return static_cast< IndexType >(_vecCallParams.size()); }

        inline std::string  GetName() const                   { return _strName; }
        inline void         SetName(std::string strNewName)   { _strName = strNewName; }

        inline BaseClasses::TypeInfo GetReturnType() const                            { return _ReturnType; }
        inline void                  SetReturnType(BaseClasses::TypeInfo ReturnType)  { _ReturnType = ReturnType; }


      public:

        virtual std::string DumpToXML(const size_t cszIntend) const final override;

        virtual BaseClasses::TypeInfo GetResultType() const final override    { return GetReturnType(); }

        virtual ExpressionPtr   GetSubExpression(IndexType SubExprIndex)      { return GetCallParameter(SubExprIndex); }
        virtual IndexType       GetSubExpressionCount() const final override  { return GetCallParameterCount(); }
      };
    };



    class IVariableContainer : public BaseClasses::Node
    {
    protected:

      typedef BaseClasses::Node       BaseType;
      typedef BaseType::IndexType     IndexType;

    public:

      inline IVariableContainer(BaseType::NodeType eNodeType) : BaseType(eNodeType)   {}


      virtual void                          AddVariable(BaseClasses::VariableInfoPtr spVariableInfo) = 0;
      virtual BaseClasses::VariableInfoPtr  GetVariableInfo(std::string strVariableName) const = 0;
    };


    class Scope final : public IVariableContainer
    {
    private:

      typedef IVariableContainer      BaseType;
      typedef BaseType::IndexType     IndexType;
      typedef BaseClasses::NodePtr    NodePtr;

      typedef std::vector< NodePtr >  ChildrenContainerType;


    private:

      ChildrenContainerType   _Children;


    public:

      inline Scope() : BaseType(Node::NodeType::Scope)   {}

      void AddChild(NodePtr spChild);

    public:

      virtual void                          AddVariable(BaseClasses::VariableInfoPtr spVariableInfo) final override;
      virtual BaseClasses::VariableInfoPtr  GetVariableInfo(std::string strVariableName) const final override;

      virtual NodePtr       GetChild(IndexType ChildIndex) final override;
      virtual IndexType     GetChildCount() const final override  { return static_cast< IndexType >(_Children.size()); }


      virtual std::string DumpToXML(const size_t cszIntend) const final override;
    };


    class FunctionDeclaration final : public IVariableContainer
    {
    private:

      typedef IVariableContainer    BaseType;
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

      inline FunctionDeclaration() : BaseType(Node::NodeType::FunctionDeclaration), _spBody(nullptr)  {}


      void AddParameter(BaseClasses::VariableInfoPtr spVariableInfo);

      ScopePtr        GetBody();
      const ScopePtr  GetBody() const;

      inline std::string  GetName() const               { return _strName; }
      inline void         SetName(std::string strName)  { _strName = strName; }

    public:

      virtual void                          AddVariable(BaseClasses::VariableInfoPtr spVariableInfo) final override;
      virtual BaseClasses::VariableInfoPtr  GetVariableInfo(std::string strVariableName) const final override;


      virtual NodePtr       GetChild(IndexType ChildIndex) final override;
      virtual IndexType     GetChildCount() const final override  { return static_cast< IndexType >(1); }


      virtual std::string DumpToXML(const size_t cszIntend) const final override;
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

