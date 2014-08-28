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
#include <limits>
#include <map>
#include <memory>
#include <set>
#include <string>
#include <vector>
#include <type_traits>

#define DEFINE_NODE_TYPES( _NodeClass_ ) \
    class _NodeClass_; \
    typedef std::shared_ptr< _NodeClass_       >  _NodeClass_##Ptr; \
    typedef std::shared_ptr< const _NodeClass_ >  _NodeClass_##ConstPtr;


namespace clang
{
namespace hipacc
{
namespace Backend
{
namespace Vectorization
{
  /** \brief  Contains common exceptions, which can be thrown by the abstract vectorized AST. */
  class ASTExceptions
  {
  public:

    /** \brief  Indicates that a specified child node index has been out of range. */
    class ChildIndexOutOfRange : public InternalErrorException
    {
    private:

      typedef InternalErrorException  BaseType;   //!< The base type of this class.

    public:

      inline ChildIndexOutOfRange() : BaseType("The index for the child node is out of range!")  {}
    };

    /** \brief  Indicates that newly specified variable declaration has a conflicting name. */
    class DuplicateVariableName : public RuntimeErrorException
    {
    private:

      typedef RuntimeErrorException  BaseType;   //!< The base type of this class.

    public:

      inline DuplicateVariableName(std::string strVarName) : BaseType(std::string("The variable name \"") + strVarName + std::string("\" is not unique!"))  {}
    };

    /** \brief  Indicates that an expression is deferenced, which is neither a pointer nor an array. */
    class NonDereferencableType : public RuntimeErrorException
    {
    private:

      typedef RuntimeErrorException  BaseType;   //!< The base type of this class.

    public:

      inline NonDereferencableType() : BaseType("The specified type cannot be dereferenced!")  {}
    };


    /** \brief  Externally used exception, indicating that a specific expression class is unknown. */
    class UnknownExpressionClass : public RuntimeErrorException
    {
    private:

      typedef RuntimeErrorException  BaseType;   //!< The base type of this class.

    public:

      inline UnknownExpressionClass(std::string strExprClassName) : BaseType( std::string( "The expression class \"") + strExprClassName + std::string("\" is unknown!") )  {}
    };

    /** \brief  Externally used exception, indicating that a specific statement class is unknown. */
    class UnknownStatementClass : public RuntimeErrorException
    {
    private:

      typedef RuntimeErrorException  BaseType;   //!< The base type of this class.

    public:

      inline UnknownStatementClass(std::string strStmtClassName) : BaseType(std::string("The statement class \"") + strStmtClassName + std::string("\" is unknown!"))  {}
    };
  };


  /** \brief  Contains all class definitions for the abstract <b>vectorized Annotated Syntax Tree (VAST)</b>. */
  class AST
  {
  public:

    typedef size_t    IndexType;    //!< Type defintions for the internally used indices

    /** \name Shared pointer type definitions */
    //@{

    class IVariableContainer;
    typedef std::shared_ptr< IVariableContainer       >  IVariableContainerPtr;         //!< Shared pointer type for objects of class IVariableContainer
    typedef std::shared_ptr< const IVariableContainer >  IVariableContainerConstPtr;    //!< Shared pointer type for constant objects of class IVariableContainer

    class FunctionDeclaration;
    typedef std::shared_ptr< FunctionDeclaration       >  FunctionDeclarationPtr;       //!< Shared pointer type for objects of class FunctionDeclaration
    typedef std::shared_ptr< const FunctionDeclaration >  FunctionDeclarationConstPtr;  //!< Shared pointer type for constant objects of class FunctionDeclaration

    class Scope;
    typedef std::shared_ptr< Scope       >  ScopePtr;       //!< Shared pointer type for objects of class Scope
    typedef std::shared_ptr< const Scope >  ScopeConstPtr;  //!< Shared pointer type for constant objects of class Scope

    //@}


  public:

    /** \brief  Helper class, which stores the position of a statement inside its enclosing scope. */
    class ScopePosition final
    {
    private:

      ScopePtr    _spScope;
      IndexType   _ChildIndex;

    public:

      inline ScopePosition(ScopePtr spScope, IndexType ChildIndex) : _spScope(spScope), _ChildIndex(ChildIndex)   {}
      inline ScopePosition(const ScopePosition &crRVal)   { *this = crRVal; }
      inline ScopePosition& operator=(const ScopePosition &crRVal)
      {
        _spScope    = crRVal._spScope;
        _ChildIndex = crRVal._ChildIndex;

        return *this;
      }


      /** \brief  Returns a shared pointer to the enclosing scope. */
      inline ScopePtr    GetScope()       { return _spScope; }

      /** \brief  Returns the referenced child node index in the enclosing scope. */
      inline IndexType   GetChildIndex()  { return _ChildIndex; }
    };


    /** \brief  Contains abstract base classes as well as commonly used node types. */
    class BaseClasses final
    {
    public:

      /** \name Shared pointer type definitions */
      //@{

      class VariableInfo;
      typedef std::shared_ptr< VariableInfo       >  VariableInfoPtr;       //!< Shared pointer type for objects of class VariableInfo
      typedef std::shared_ptr< const VariableInfo >  VariableInfoConstPtr;  //!< Shared pointer type for constant objects of class VariableInfo

      class Node;
      typedef std::shared_ptr< Node       >  NodePtr;       //!< Shared pointer type for objects of class Node
      typedef std::shared_ptr< const Node >  NodeConstPtr;  //!< Shared pointer type for constant objects of class Node

      class ControlFlowStatement;
      typedef std::shared_ptr< ControlFlowStatement       >  ControlFlowStatementPtr;       //!< Shared pointer type for objects of class ControlFlowStatement
      typedef std::shared_ptr< const ControlFlowStatement >  ControlFlowStatementConstPtr;  //!< Shared pointer type for constant objects of class ControlFlowStatement

      class Expression;
      typedef std::shared_ptr< Expression       >  ExpressionPtr;       //!< Shared pointer type for objects of class Expression
      typedef std::shared_ptr< const Expression >  ExpressionConstPtr;  //!< Shared pointer type for constant objects of class Expression

      //@}


    public:

      /** \brief    Encapsulates the information about qualiified type.
       *  \remarks  A the current stage, only the following kinds of types are suupported:<BR>
                    <UL>
                      <LI>Native element types</LI>
                      <LI>Pointer types to native elements</LI>
                      <LI>Multi-dimensional array types of native elements</LI>
                      <LI>Multi-dimensional arrays of pointer types of native elements</LI>
                    </UL> */
      class TypeInfo
      {
      public:

        typedef std::vector< size_t >    ArrayDimensionVectorType;  //!< Type definition for a list of array dimensions

        enum class KnownTypes
        {
          Bool,     //!< Internal ID for a boolean type
          Int8,     //!< Internal ID for a signed 8-bit integer type
          UInt8,    //!< Internal ID for an unsigned 8-bit integer type
          Int16,    //!< Internal ID for a signed 16-bit integer type
          UInt16,   //!< Internal ID for an unsigned 16-bit integer type
          Int32,    //!< Internal ID for a signed 32-bit integer type
          UInt32,   //!< Internal ID for an unsigned 32-bit integer type
          Int64,    //!< Internal ID for a signed 64-bit integer type
          UInt64,   //!< Internal ID for an unsigned 64-bit integer type
          Float,    //!< Internal ID for a single-precision floating-point type
          Double,   //!< Internal ID for a double-precision floating-point type
          Unknown   //!< Internal ID for all currently unknown types
        };


      private:

        KnownTypes                _eType;
        bool                      _bIsConst;
        bool                      _bIsPointer;
        ArrayDimensionVectorType  _vecArrayDimensions;


      public:

        /** \brief  Constructs a new TypeInfo object.
         *  \param  eType       The requested native element type.
         *  \param  bIsConst    A flag indicating, whether the specified type is marked as constant.
         *  \param  bIsPointer  A flag indicating, whether the specified type is a pointer. */
        inline TypeInfo(KnownTypes eType = KnownTypes::Unknown, bool bIsConst = false, bool bIsPointer = false)
        {
          _eType      = eType;
          _bIsConst   = bIsConst;
          _bIsPointer = bIsPointer;
        }

        inline TypeInfo(const TypeInfo &crRVal)   { *this = crRVal; }
        TypeInfo& operator=(const TypeInfo &crRVal);


        /** \brief  Returns the type, which would be created by dereferencing this type a single time. */
        TypeInfo CreateDereferencedType() const;

        /** \brief  Returns the type, which corresponds to a pointer to this type. */
        TypeInfo CreatePointerType() const;


        /** \brief  Returns a reference to the array dimensions list of this type. */
        inline ArrayDimensionVectorType&        GetArrayDimensions()        { return _vecArrayDimensions; }

        /** \brief  Returns a constant reference to the array dimensions list of this type. */
        inline const ArrayDimensionVectorType&  GetArrayDimensions() const  { return _vecArrayDimensions; }


        /** \brief  Returns the currently set <b>constant</b> marker of this type. */
        inline bool GetConst() const          { return _bIsConst; }

        /** \brief  Changes the <b>constant</b> marker of this type.
         *  \param  bIsConst  The new constant marker. */
        inline void SetConst(bool bIsConst)   { _bIsConst = bIsConst; }


        /** \brief  Returns the currently set <b>pointer</b> marker of this type. */
        inline bool GetPointer() const            { return _bIsPointer; }

        /** \brief  Changes the <b>pointer</b> marker of this type.
         *  \param  bIsPointer  The new pointer marker. */
        inline void SetPointer(bool bIsPointer)   { _bIsPointer = bIsPointer; }


        /** \brief  Returns the native element type of this type. */
        inline KnownTypes GetType() const             { return _eType; }

        /** \brief  Changes the native element type of this type.
         *  \param  eType   The requested new native element type. */
        inline void       SetType(KnownTypes eType)   { _eType = eType; }


        /** \brief  Returns, whether this type is an array type. */
        inline bool IsArray() const           { return (!_vecArrayDimensions.empty()); }

        /** \brief  Returns, whether this type is dereferencable, i.e. whether it is an pointer or an array. */
        inline bool IsDereferencable() const  { return (IsArray() || GetPointer()); }

        /** \brief  Returns, whether this type is a native element type. */
        inline bool IsSingleValue() const     { return (!IsDereferencable()); }


        /** \brief  Checks, whether this TypeInfo object is identical with another one.
         *  \param  crRVal                  The other TypeInfo object, the current one shall be compared with.
         *  \param  bIgnoreConstQualifier   A flag indicating, whether the <b>const</b> flag of both types shall be ignored during the comparison. */
        bool IsEqual(const TypeInfo &crRVal, bool bIgnoreConstQualifier);

        /** \brief  Checks, whether two TypeInfo objects describe the identical type.
         *  \param  crRVal  The other TypeInfo object, the current one shall be compared with. */
        inline bool operator==(const TypeInfo &crRVal)    { return IsEqual(crRVal, false); }


        /** \brief  Dumps the contents of this object into an XML string.
         *  \param  cszIntend   The intendation level, which shall be used for each line in the XML string, in space characters. */
        std::string DumpToXML(const size_t cszIntend) const;


      public:

        /** \brief  Creates a TypeInfo object representing an integer type with a specified size.
         *  \param  szTypeSize  The requested size of the integer element type in bytes.
         *  \param  bSigned     A flag indicating, whether the created integer type shall be signed. */
        static TypeInfo     CreateSizedIntegerType(size_t szTypeSize, bool bSigned);

        /** \brief  Returns the promoted element type, which would result from an operation on the two input element types.
         *  \param  eTypeLHS  The first input element type.
         *  \param  eTypeRHS  The second input element type. */
        static KnownTypes   GetPromotedType(KnownTypes eTypeLHS, KnownTypes eTypeRHS);

        /** \brief  Returns the size of a specific element type in bytes.
         *  \param  eType   The element type, whose size shall be returned. */
        static size_t       GetTypeSize(KnownTypes eType);

        /** \brief  Returns the string identifier of a specific element type.
         *  \param  eType   The element type, whose string identifier shall be returned. */
        static std::string  GetTypeString(KnownTypes eType);

        /** \brief  Checks, whether a certain element type is a signed type.
         *  \param  eType   The element type, which shall be checked. */
        static bool         IsSigned(KnownTypes eType);
      };

      /** \brief  Describes a variable declaration. */
      class VariableInfo
      {
      private:

        friend class AST;

        std::string _strName;
        TypeInfo    _Type;
        bool        _bVectorize;

        inline VariableInfo() : _strName(""), _bVectorize(false)    {}


      public:

        /** \brief  Creates a new object of this class.
         *  \param  strName     The requested name of the new declared variable.
         *  \param  crTypeInfo  The TypeInfo object for this declaration (its contents will be copied).
         *  \param  bVectorize  A flag indicating, whether the newly declared variable is vectorized.
         *  \return A shared pointer to the newly created VariableInfo object. */
        static VariableInfoPtr Create(std::string strName, const TypeInfo &crTypeInfo, bool bVectorize = false);


        /** \brief  Returns the currently set variable name of this declaration. */
        inline std::string  GetName() const               { return _strName; }

        /** \brief  Changes the name of the declared variable.
         *  \param  strName  The new variable name. */
        inline void         SetName(std::string strName)  { _strName = strName; }


        /** \brief  Returns a reference to the TypeInfo object of this variable declaration. */
        inline TypeInfo&        GetTypeInfo()       { return _Type; }

        /** \brief  Returns a constant reference to the TypeInfo object of this variable declaration. */
        inline const TypeInfo&  GetTypeInfo() const { return _Type; }


        /** \brief  Returns the currently set <b>vectorization</b> marker of this variable declaration. */
        inline bool GetVectorize() const            { return _bVectorize; }

        /** \brief  Changes the <b>vectorization</b> marker of this variable declaration.
         *  \param  bVectorize  The new vectorization marker. */
        inline void SetVectorize(bool bVectorize)   { _bVectorize = bVectorize; }


        /** \brief  Dumps the contents of this object into an XML string.
         *  \param  cszIntend   The intendation level, which shall be used for each line in the XML string, in space characters. */
        std::string DumpToXML(const size_t cszIntend) const;
      };

      /** \brief  Base class for all VAST nodes describing any kind of statements. */
      class Node
      {
      private:

        friend class AST;

        std::weak_ptr< Node >   _wpParent;
        std::weak_ptr< Node >   _wpThis;


        /** \brief  Sets the parent of a VAST node.
         *  \param  spParent  A shared pointer to the new parent VAST node. */
        inline void _SetParent(NodePtr spParent)        { _wpParent = spParent; }

      protected:

        inline Node()   {}


        /** \brief  Dumps the contents of a child node into an XML string.
         *  \param  spChild     A constant shared pointer to the child node, which shall be dumped.
         *  \param  cszIntend   The intendation level, which shall be used for each line in the XML string, in space characters. */
        static std::string _DumpChildToXml(const NodeConstPtr spChild, const size_t cszIntend);


        /** \brief  Links a specific child node pointer to another child node, and updates the parent pointers.
         *  \tparam NodeClassPtr      The shared pointer type of the child node pointers.
         *  \param  rDestinationPtr   A reference to the shared pointer of the child node, which shall be exchanged.
         *  \param  crSourcePtr       A constant reference to the shared pointer, which points to the new child node. */
        template < typename NodeClassPtr >
        inline void _SetChildPtr(NodeClassPtr &rDestinationPtr, const NodeClassPtr &crSourcePtr)
        {
          // If the child is set, remove its parent pointer
          if (rDestinationPtr)
          {
            // Only remove the parent of the child, if this node is the parent
            if (rDestinationPtr->GetParent() == GetThis())
            {
              rDestinationPtr->_SetParent(nullptr);
            }
          }

          // Set the child pointer and set the current node as its parent
          rDestinationPtr = crSourcePtr;
          _SetParentToChild(rDestinationPtr);
        }

        /** \brief  Sets this node as the parent node to a child node.
         *  \param  spChild   A shared pointer to the child node, whose parent pointer shall be updated. */
        void _SetParentToChild(NodePtr spChild) const;


      public:

        virtual ~Node() {}


        /** \brief  Returns the index of this node in the current AST, i.e. the number of parents. */
        IndexType GetHierarchyLevel() const;


        /** \brief  Returns a shared pointer to the direct parent of this node. */
        NodePtr               GetParent();

        /** \brief  Returns a constant shared pointer to the direct parent of this node. */
        inline NodeConstPtr   GetParent() const   { return const_cast<Node*>(this)->GetParent(); }


        /** \brief  Returns the current position of this node in its enclosing scope. */
        ScopePosition GetScopePosition();


        /** \brief  Returns a shared pointer to the current node. */
        inline NodePtr        GetThis()         { return _wpThis.lock(); }

        /** \brief  Returns a constant shared pointer to the current node. */
        inline NodeConstPtr   GetThis() const   { return _wpThis.lock(); }


        /** \brief  Checks, whether the current node is a leaf node, i.e. if it has no children. */
        inline bool IsLeafNode() const  { return (GetChildCount() == static_cast<IndexType>(0)); }



        /** \brief    Cast this node into another node type.
         *  \tparam   NodeClass   The requested target type of the cast.
         *  \remarks  If the current object is not implementing the requested type, an exception will be thrown. */
        template <class NodeClass>
        inline std::shared_ptr< NodeClass > CastToType()
        {
          if (! IsType<NodeClass>())
          {
            throw RuntimeErrorException("Invalid node cast type!");
          }

          return std::dynamic_pointer_cast< NodeClass >( GetThis() );
        }

        /** \brief    Cast this node into another constant node type.
         *  \tparam   NodeClass   The requested target type of the cast.
         *  \remarks  If the current object is not implementing the requested type, an exception will be thrown. */
        template <class NodeClass>
        inline std::shared_ptr< const NodeClass > CastToType() const
        {
          return const_cast< Node* >( this )->CastToType< const NodeClass >();
        }

        /** \brief    Checks, whether this node is implementing a specific node type.
         *  \tparam   NodeClass   The node type, which shall be checked. */
        template <class NodeClass>
        inline bool IsType() const
        {
          static_assert( std::is_base_of< Node, NodeClass >::value, "All VAST nodes must be derived from class \"Node\"!" );

          return (dynamic_cast< const NodeClass* >(this) != nullptr);
        }


      public:

        /** \name Abstract methods implemented by the derived classes. */
        //@{

        /** \brief    Returns a shared pointer to a specific child node of the current one.
         *  \param    ChildIndex  The index of the requested child node.
         *  \remarks  If the child index is out of range, a <b>ASTExceptions::ChildIndexOutOfRange</b> exception will be thrown. */
        virtual NodePtr     GetChild(IndexType ChildIndex) = 0;

        /** \brief  Returns the number of children of this node. */
        virtual IndexType   GetChildCount() const = 0;


        /** \brief  Dumps the contents of this node into an XML string.
         *  \param  cszIntend   The intendation level, which shall be used for each line in the XML string, in space characters. */
        virtual std::string DumpToXML(const size_t cszIntend) const = 0;

        //@}
      };

      /** \brief  Base class for all VAST nodes describing control-flow statements. */
      class ControlFlowStatement : public Node
      {
      protected:

        inline ControlFlowStatement()   {}

      public:

        virtual ~ControlFlowStatement() {}


        /** \brief  Returns, whether this control-flow statement needs to be vectorized. */
        virtual bool IsVectorized() const = 0;
      };

      /** \brief  Base class for all VAST nodes describing expressions. */
      class Expression : public Node
      {
      protected:

        inline Expression()   {}


        /** \brief  Dumps the result type of this expression into a XML string.
         *  \param  cszIntend   The intendation level, which shall be used for each line in the XML string, in space characters. */
        std::string _DumpResultTypeToXML(const size_t cszIntend) const;

        /** \brief    Returns the sub-expression index of a specified sub-expression.
         *  \param    spSubExpression   A constant shared pointer to the sub-expression, whose index shall be retrived.
         *  \remarks  If the specified sub-expression is not a child of the current expression, an exception will be thrown. */
        IndexType _FindSubExpressionIndex(ExpressionConstPtr spSubExpression) const;

      public:

        virtual ~Expression() {}


        /** \name Public methods inherited from class Node. */
        //@{

        virtual NodePtr   GetChild(IndexType ChildIndex) final override   { return GetSubExpression(ChildIndex); }
        virtual IndexType GetChildCount() const final override            { return GetSubExpressionCount(); }

        //@}


        /** \brief  Returns the sub-expression index of this expression in its parent expression, if the current expression is a sub-expression.
         *  \sa     IsSubExpression(). */
        IndexType GetParentIndex() const;

        /** \brief  Returns, whether the current expression is a sub-expression of another one. */
        bool      IsSubExpression() const;


        /** \brief  Returns, whether this expression is vectorized. */
        virtual bool      IsVectorized();

        /** \brief  Returns, whether this expression is vectorized. */
        inline  bool      IsVectorized() const  { return const_cast< Expression* >(this)->IsVectorized(); }


        /** \name Abstract methods implemented by the derived expression classes. */
        //@{

        /** \brief  Returns the result type of this expression. */
        virtual TypeInfo  GetResultType() const = 0;

        /** \brief  Returns a shared pointer to the sub-expression with the specified index.
         *  \param  SubExprIndex  The index of the requested sub-expression. */
        virtual ExpressionPtr   GetSubExpression(IndexType SubExprIndex) = 0;

        /** \brief  Returns the number of sub-expressions for this expression. */
        virtual IndexType       GetSubExpressionCount() const = 0;

        /** \brief  Replaces a specific sub-expression with another one.
         *  \param  SubExprIndex  The index of the sub-expression, which shall be replaced.
         *  \param  spSubExpr     A shared pointer to new sub-expression. */
        virtual void            SetSubExpression(IndexType SubExprIndex, ExpressionPtr spSubExpr) = 0;

        //@}


        /** \brief  Returns a constant shared pointer to the sub-expression with the specified index.
         *  \param  SubExprIndex  The index of the requested sub-expression. */
        inline ExpressionConstPtr GetSubExpression(IndexType SubExprIndex) const  { return const_cast< Expression* >(this)->GetSubExpression(SubExprIndex); }
      };
    };


    /** \brief  Contains all class definitions for VAST nodes describing control-flow statements. */
    class ControlFlow final
    {
    public:

      /** \name Shared pointer type definitions */
      //@{

      class Loop;
      typedef std::shared_ptr< Loop       >  LoopPtr;       //!< Shared pointer type for objects of class Loop
      typedef std::shared_ptr< const Loop >  LoopConstPtr;  //!< Shared pointer type for constant objects of class Loop

      class LoopControlStatement;
      typedef std::shared_ptr< LoopControlStatement       >  LoopControlStatementPtr;       //!< Shared pointer type for objects of class LoopControlStatement
      typedef std::shared_ptr< const LoopControlStatement >  LoopControlStatementConstPtr;  //!< Shared pointer type for constant objects of class LoopControlStatement

      class ConditionalBranch;
      typedef std::shared_ptr< ConditionalBranch       >  ConditionalBranchPtr;       //!< Shared pointer type for objects of class ConditionalBranch
      typedef std::shared_ptr< const ConditionalBranch >  ConditionalBranchConstPtr;  //!< Shared pointer type for constant objects of class ConditionalBranch

      class BranchingStatement;
      typedef std::shared_ptr< BranchingStatement       >  BranchingStatementPtr;       //!< Shared pointer type for objects of class BranchingStatement
      typedef std::shared_ptr< const BranchingStatement >  BranchingStatementConstPtr;  //!< Shared pointer type for constant objects of class BranchingStatement

      class ReturnStatement;
      typedef std::shared_ptr< ReturnStatement       >  ReturnStatementPtr;       //!< Shared pointer type for objects of class ReturnStatement
      typedef std::shared_ptr< const ReturnStatement >  ReturnStatementConstPtr;  //!< Shared pointer type for constant objects of class ReturnStatement

      //@}


    public:

      /** \brief  Describes all kinds of loop statements. */
      class Loop final : public BaseClasses::ControlFlowStatement
      {
      public:

        /** \brief  Enumeration of all supported loop types. */
        enum class LoopType
        {
          TopControlled,      //!< Internal ID for top-controlled loops, like <b>while-</b> and <b>for-</b>loops.
          BottomControlled    //!< Internal ID for bottom-controlled loops, like <b>do-while-</b>loops.
        };

      private:

        friend class AST;

        LoopType                    _eLoopType;
        BaseClasses::ExpressionPtr  _spConditionExpr;
        BaseClasses::ExpressionPtr  _spIncrementExpr;
        ScopePtr                    _spBody;
        bool                        _bForceVectorization;


        inline Loop() : _eLoopType(LoopType::TopControlled), _bForceVectorization(false)   {}

        /** \brief  Returns the string identifier of a specific loop type.
         *  \param  eType   The loop type, whose string identifier shall be returned. */
        static std::string _GetLoopTypeString(LoopType eType);


      public:

        /** \brief  Creates a new object of this class.
         *  \param  eType         The requested type of this loop.
         *  \param  spCondition   A shared pointer to expression object, which shall be used as the condition.
         *  \param  spIncrement   A shared pointer to expression object, which describes the increment after each iteration. */
        static LoopPtr Create(LoopType eType = LoopType::TopControlled, BaseClasses::ExpressionPtr spCondition = nullptr, BaseClasses::ExpressionPtr spIncrement = nullptr);

        virtual ~Loop() {}


        /** \brief  Returns a shared pointer to the scope object, which encapsulates the loop body. */
        ScopePtr        GetBody();

        /** \brief  Returns a constant shared pointer to the scope object, which encapsulates the loop body. */
        ScopeConstPtr   GetBody() const;


        /** \brief  Returns a shared pointer to the condition expression object. */
        inline BaseClasses::ExpressionPtr         GetCondition()                                        { return _spConditionExpr; }

        /** \brief  Returns a constant shared pointer to the condition expression object. */
        inline BaseClasses::ExpressionConstPtr    GetCondition() const                                  { return _spConditionExpr; }

        /** \brief  Replaces the currently set condition expression object.
         *  \param  spCondition   A shared pointer to the expression object, which shall be used as the new condition. */
        inline void                               SetCondition(BaseClasses::ExpressionPtr spCondition)  { _SetChildPtr(_spConditionExpr, spCondition); }


        /** \brief  Returns a shared pointer to the increment expression object. */
        inline BaseClasses::ExpressionPtr         GetIncrement()                                        { return _spIncrementExpr; }

        /** \brief  Returns a constant shared pointer to the increment expression object. */
        inline BaseClasses::ExpressionConstPtr    GetIncrement() const                                  { return _spIncrementExpr; }

        /** \brief    Replaces the currently set increment expression object.
         *  \param    spIncrement   A shared pointer to the expression object, which shall be used as the new increment expression.
         *  \remarks  The increment expression is the expression, which will be called after each loop iteration. If this is not required,
                      it can be set to <b>nullptr</b>. */
        inline void                               SetIncrement(BaseClasses::ExpressionPtr spIncrement)  { _SetChildPtr(_spIncrementExpr, spIncrement); }


        /** \brief  Returns the currently set <b>forced vectorization</b> flag. */
        inline bool GetForcedVectorization() const          { return _bForceVectorization; }

        /** \brief  Sets a flag, which indicates whether the loop must be vectorized, even if its condition expression is not.
         *  \param  bForceVec   The new forced vectorization flag. */
        inline void SetForcedVectorization(bool bForceVec)  { _bForceVectorization = bForceVec; }


        /** \brief  Returns the currently set loop type. */
        inline LoopType GetLoopType() const           { return _eLoopType; }

        /** \brief  Changes the loop type.
         *  \param  eType   The new loop type. */
        inline void     SetLoopType(LoopType eType)   { _eLoopType = eType; }


      public:

        /** \name Public methods inherited from class BaseClasses::Node. */
        //@{

        virtual std::string DumpToXML(const size_t cszIntend) const final override;

        virtual BaseClasses::NodePtr  GetChild(IndexType ChildIndex) final override;
        virtual IndexType             GetChildCount() const final override    { return static_cast< IndexType >( 3 ); }

        //@}


        /** \name Public methods inherited from class BaseClasses::ControlFlowStatement. */
        //@{

        virtual bool IsVectorized() const final override;

        //@}
      };

      /** \brief  Describes loop control statements, i.e. <b>break</b> and <b>continue</b> statements. */
      class LoopControlStatement final : public BaseClasses::ControlFlowStatement
      {
      public:

        /** \brief  Enumeration of all supported loop control types. */
        enum class LoopControlType
        {
          Break,    //!< Internal ID of the <b>break</b> statement.
          Continue  //!< Internal ID of the <b>continue</b> statement.
        };


      private:

        friend class AST;

        LoopControlType _eControlType;


        inline LoopControlStatement() : _eControlType(LoopControlType::Break)  {}


        /** \brief  Returns the string identifier of a specific loop control type.
         *  \param  eType   The loop control type, whose string identifier shall be returned. */
        static std::string _GetLoopControlTypeString(LoopControlType eType);


      public:

        /** \brief  Creates a new object of this class.
         *  \param  eCtrlType   The requested type of this loop control statement. */
        static LoopControlStatementPtr Create(LoopControlType eCtrlType);

        virtual ~LoopControlStatement() {}


        /** \brief  Returns the currently set loop control type. */
        inline LoopControlType  GetControlType() const                        { return _eControlType; }

        /** \brief  Changes the loop control type.
         *  \param  eNewCtrlType   The new loop control type. */
        inline void             SetControlType(LoopControlType eNewCtrlType)  { _eControlType = eNewCtrlType; }


        /** \brief  Returns a shared pointer to the enclosing loop of this statement. */
        LoopPtr               GetControlledLoop();

        /** \brief  Returns a constant shared pointer to the enclosing loop of this statement. */
        inline LoopConstPtr   GetControlledLoop() const   { return const_cast< LoopControlStatement* >(this)->GetControlledLoop(); }

      public:

        /** \name Public methods inherited from class BaseClasses::Node. */
        //@{

        virtual std::string DumpToXML(const size_t cszIntend) const final override;

        virtual BaseClasses::NodePtr  GetChild(IndexType ChildIndex) final override   { throw ASTExceptions::ChildIndexOutOfRange(); }
        virtual IndexType             GetChildCount() const final override            { return static_cast< IndexType >(0); }

        //@}


        /** \name Public methods inherited from class BaseClasses::ControlFlowStatement. */
        //@{

        virtual bool IsVectorized() const final override;

        //@}
      };

      /** \brief  Describes a conditional branch of a multi-branch control-flow statement. */
      class ConditionalBranch final : public BaseClasses::ControlFlowStatement
      {
      private:

        friend class AST;

        typedef BaseClasses::ExpressionPtr          ExpressionPtr;        //!< Type alias for shared pointers to class <b>BaseClasses::Expression</b>.
        typedef BaseClasses::ExpressionConstPtr     ExpressionConstPtr;   //!< Type alias for constant shared pointers to class <b>BaseClasses::Expression</b>.

      private:

        ExpressionPtr   _spCondition;
        ScopePtr        _spBody;

        inline ConditionalBranch() : _spCondition(nullptr), _spBody(nullptr)   {}


      public:

        /** \brief  Creates a new object of this class.
         *  \param  spCondition   A shared pointer to the expression object, which shall be used as the condition. */
        static ConditionalBranchPtr Create(ExpressionPtr spCondition = nullptr);

        virtual ~ConditionalBranch()  {}


        /** \brief  Returns a shared pointer to the scope object, which encapsulates the branch body. */
        ScopePtr        GetBody();

        /** \brief  Returns a constant shared pointer to the scope object, which encapsulates the branch body. */
        ScopeConstPtr   GetBody() const;


        /** \brief  Returns a shared pointer to the condition expression object. */
        inline ExpressionPtr        GetCondition()                            { return _spCondition; }

        /** \brief  Returns a constant shared pointer to the condition expression object. */
        inline ExpressionConstPtr   GetCondition() const                      { return _spCondition; }

        /** \brief  Replaces the currently set condition expression object.
         *  \param  spCondition   A shared pointer to the expression object, which shall be used as the new condition. */
        inline void                 SetCondition(ExpressionPtr spCondition)   { _SetChildPtr(_spCondition, spCondition); }


      public:

        /** \name Public methods inherited from class BaseClasses::Node. */
        //@{

        virtual std::string DumpToXML(const size_t cszIntend) const final override;

        virtual BaseClasses::NodePtr  GetChild(IndexType ChildIndex) final override;
        virtual IndexType             GetChildCount() const final override    { return static_cast< IndexType >( 2 ); }

        //@}


        /** \name Public methods inherited from class BaseClasses::ControlFlowStatement. */
        //@{

        virtual bool IsVectorized() const final override;

        //@}
      };

      /** \brief  Describes a multi-branch control-flow statement, e.g. an <b>if-statement</b>. */
      class BranchingStatement final : public BaseClasses::ControlFlowStatement
      {
      private:

        friend class AST;

        std::vector< ConditionalBranchPtr >   _vecBranches;
        ScopePtr                              _spDefaultBranch;


        inline BranchingStatement() : _spDefaultBranch(nullptr)   {}


      public:

        /** \brief  Creates a new object of this class. */
        static BranchingStatementPtr Create();

        virtual ~BranchingStatement() {}


        /** \brief  Adds another conditional branch at the end of this statement.
         *  \param  spBranch  A shared pointer to the conditional branch, which shall be added. */
        void                  AddConditionalBranch(ConditionalBranchPtr spBranch);

        /** \brief  Returns a shared pointer to the conditional branch object with a specified index.
         *  \param  BranchIndex   The index of the requested conditional branch. */
        ConditionalBranchPtr  GetConditionalBranch(IndexType BranchIndex);

        /** \brief  Returns the number of conditional branches in this statement. */
        inline IndexType      GetConditionalBranchesCount() const   { return static_cast< IndexType >( _vecBranches.size() ); }

        /** \brief  Removes the conditional branch with a specified index for this statement.
         *  \param  BranchIndex   The index of the conditional branch, which shall be removed. */
        void                  RemoveConditionalBranch(IndexType BranchIndex);


        /** \brief  Returns a shared pointer to the scope object, which encapsulates the unconditional default branch. */
        ScopePtr        GetDefaultBranch();

        /** \brief  Returns a constant shared pointer to the scope object, which encapsulates the unconditional default branch. */
        ScopeConstPtr   GetDefaultBranch() const;


        /** \brief  Returns a constant shared pointer to the conditional branch object with a specified index.
         *  \param  BranchIndex   The index of the requested conditional branch. */
        inline ConditionalBranchConstPtr  GetConditionalBranch(IndexType BranchIndex) const
        {
          return const_cast< BranchingStatement* >( this )->GetConditionalBranch( BranchIndex );
        }


      public:

        /** \name Public methods inherited from class BaseClasses::Node. */
        //@{

        virtual std::string DumpToXML(const size_t cszIntend) const final override;

        virtual BaseClasses::NodePtr  GetChild(IndexType ChildIndex) final override;
        virtual IndexType             GetChildCount() const final override    { return GetConditionalBranchesCount() + 1; }

        //@}


        /** \name Public methods inherited from class BaseClasses::ControlFlowStatement. */
        //@{

        virtual bool IsVectorized() const final override;

        //@}
      };

      /** \brief    Describes a <b>return</b> statement.
       *  \remarks  Since at the current stage of development only void-functions can be expressed, these statements do not contain a return expression. */
      class ReturnStatement final : public BaseClasses::ControlFlowStatement
      {
      private:

        friend class AST;

        inline ReturnStatement()  {}

      public:

        /** \brief  Creates a new object of this class. */
        static ReturnStatementPtr Create();

        virtual ~ReturnStatement() {}

      public:

        /** \name Public methods inherited from class BaseClasses::Node. */
        //@{

        virtual std::string DumpToXML(const size_t cszIntend) const final override;

        virtual BaseClasses::NodePtr  GetChild(IndexType ChildIndex) final override   { throw ASTExceptions::ChildIndexOutOfRange(); }
        virtual IndexType             GetChildCount() const final override            { return static_cast< IndexType >( 0 ); }

        //@}


        /** \name Public methods inherited from class BaseClasses::ControlFlowStatement. */
        //@{

        virtual bool IsVectorized() const final override;

        //@}
      };
    };


    class Expressions final
    {
    // Public type definitions
    public:

      DEFINE_NODE_TYPES( Value );
      DEFINE_NODE_TYPES( Constant );
      DEFINE_NODE_TYPES( Identifier );
      DEFINE_NODE_TYPES( MemoryAccess );

      DEFINE_NODE_TYPES( UnaryExpression );
      DEFINE_NODE_TYPES( Conversion );
      DEFINE_NODE_TYPES( Parenthesis );
      DEFINE_NODE_TYPES( UnaryOperator );

      DEFINE_NODE_TYPES( BinaryOperator );
      DEFINE_NODE_TYPES( ArithmeticOperator );
      DEFINE_NODE_TYPES( AssignmentOperator );
      DEFINE_NODE_TYPES( RelationalOperator );

      DEFINE_NODE_TYPES( FunctionCall );


    // Class definitions
    public:

      class Value : public BaseClasses::Expression
      {
      private:

        typedef BaseClasses::ExpressionPtr  ExpressionPtr;

      protected:

        inline Value()  {}

      public:

        virtual ~Value()  {}


        virtual ExpressionPtr   GetSubExpression(IndexType SubExprIndex) override                           { throw ASTExceptions::ChildIndexOutOfRange(); }
        virtual IndexType       GetSubExpressionCount() const override                                      { return static_cast< IndexType >(0); }
        virtual void            SetSubExpression(IndexType SubExprIndex, ExpressionPtr spSubExpr) override  { throw ASTExceptions::ChildIndexOutOfRange(); }
      };

      class Constant final : public Value
      {
      private:

        friend class AST;

        typedef BaseClasses::TypeInfo::KnownTypes   KnownTypes;

        union
        {
          std::uint64_t ui64IntegralValue;
          double        dFloatingPointValue;
        } _unionValues;

        KnownTypes    _eType;


        inline Constant()   {}

        template < typename SourceValueType >
        inline void _ChangeType(KnownTypes eNewType)
        {
          SourceValueType TValue = GetValue< SourceValueType >();

          switch (eNewType)
          {
          case KnownTypes::Bool:    SetValue( TValue != static_cast< SourceValueType >( 0 ) );  break;
          case KnownTypes::Int8:    SetValue( static_cast< std::int8_t   >( TValue ) );         break;
          case KnownTypes::UInt8:   SetValue( static_cast< std::uint8_t  >( TValue ) );         break;
          case KnownTypes::Int16:   SetValue( static_cast< std::int16_t  >( TValue ) );         break;
          case KnownTypes::UInt16:  SetValue( static_cast< std::uint16_t >( TValue ) );         break;
          case KnownTypes::Int32:   SetValue( static_cast< std::int32_t  >( TValue ) );         break;
          case KnownTypes::UInt32:  SetValue( static_cast< std::uint32_t >( TValue ) );         break;
          case KnownTypes::Int64:   SetValue( static_cast< std::int64_t  >( TValue ) );         break;
          case KnownTypes::UInt64:  SetValue( static_cast< std::uint64_t >( TValue ) );         break;
          case KnownTypes::Float:   SetValue( static_cast< float         >( TValue ) );         break;
          case KnownTypes::Double:  SetValue( static_cast< double        >( TValue ) );         break;
          default:                  throw RuntimeErrorException(std::string("Invalid constant type: ") + BaseClasses::TypeInfo::GetTypeString(eNewType));
          }
        }


      public:

        template <typename ValueType>
        static ConstantPtr Create(ValueType TValue)
        {
          ConstantPtr spConstant = AST::CreateNode<Constant>();

          spConstant->SetValue(TValue);

          return spConstant;
        }

        virtual ~Constant() {}


        inline BaseClasses::TypeInfo::KnownTypes  GetValueType() const    { return _eType; }


        void  ChangeType(KnownTypes eNewType);

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


        std::string GetAsString() const;

        virtual BaseClasses::TypeInfo GetResultType() const final override;

        virtual std::string DumpToXML(const size_t cszIntend) const final override;
      };

      class Identifier final : public Value
      {
      private:

        friend class AST;

      private:

        std::string   _strName;

        inline Identifier()   {}


      public:

        static IdentifierPtr Create( std::string strName );

        virtual ~Identifier() {}


        inline std::string  GetName() const               { return _strName; }
        inline void         SetName(std::string strName)  { _strName = strName; }

        BaseClasses::VariableInfoPtr              LookupVariableInfo();
        inline BaseClasses::VariableInfoConstPtr  LookupVariableInfo() const
        {
          return const_cast< Identifier* >( this )->LookupVariableInfo();
        }


        virtual bool IsVectorized() final override;

        virtual BaseClasses::TypeInfo GetResultType() const final override;

        virtual std::string DumpToXML(const size_t cszIntend) const final override;
      };

      class MemoryAccess final : public Value
      {
      private:

        friend class AST;

        typedef BaseClasses::ExpressionPtr        ExpressionPtr;
        typedef BaseClasses::ExpressionConstPtr   ExpressionConstPtr;

      private:

        ExpressionPtr   _spMemoryRef;
        ExpressionPtr   _spIndexExpr;

        inline MemoryAccess() : _spMemoryRef(nullptr), _spIndexExpr(nullptr)   {}


      public:

        static MemoryAccessPtr Create(ExpressionPtr spMemoryReference = nullptr, ExpressionPtr spIndexExpression = nullptr);

        virtual ~MemoryAccess() {}


        inline ExpressionPtr        GetIndexExpression()                                  { return _spIndexExpr; }
        inline ExpressionConstPtr   GetIndexExpression() const                            { return _spIndexExpr; }
        inline void                 SetIndexExpression(ExpressionPtr spIndexExpression)   { _SetChildPtr(_spIndexExpr, spIndexExpression); }

        inline ExpressionPtr        GetMemoryReference()                                  { return _spMemoryRef; }
        inline ExpressionConstPtr   GetMemoryReference() const                            { return _spMemoryRef; }
        inline void                 SetMemoryReference(ExpressionPtr spMemoryReference)   { _SetChildPtr(_spMemoryRef, spMemoryReference); }


        virtual BaseClasses::TypeInfo GetResultType() const final override;

        virtual ExpressionPtr   GetSubExpression(IndexType SubExprIndex) final override;
        virtual IndexType       GetSubExpressionCount() const final override  { return static_cast< IndexType >(2); }
        virtual void            SetSubExpression(IndexType SubExprIndex, ExpressionPtr spSubExpr) final override;


        virtual std::string DumpToXML(const size_t cszIntend) const final override;
      };


      class UnaryExpression : public BaseClasses::Expression
      {
      private:

        BaseClasses::ExpressionPtr  _spSubExpression;

      protected:

        std::string _DumpSubExpressionToXML(const size_t cszIntend) const;

        inline UnaryExpression() : _spSubExpression(nullptr)  {}

      public:

        virtual ~UnaryExpression()  {}


        inline BaseClasses::ExpressionPtr       GetSubExpression()                                      { return _spSubExpression; }
        inline BaseClasses::ExpressionConstPtr  GetSubExpression() const                                { return _spSubExpression; }
        inline void                             SetSubExpression(BaseClasses::ExpressionPtr spSubExpr)  { _SetChildPtr(_spSubExpression, spSubExpr); }

        virtual BaseClasses::ExpressionPtr  GetSubExpression(IndexType SubExprIndex) final override;
        virtual IndexType                   GetSubExpressionCount() const final override  { return static_cast< IndexType >( 1 ); }
        virtual void                        SetSubExpression(IndexType SubExprIndex, BaseClasses::ExpressionPtr spSubExpr) final override;
      };

      class Conversion final : public UnaryExpression
      {
      private:

        friend class AST;

      private:

        BaseClasses::TypeInfo   _ConvertType;
        bool                    _bIsExplicit;

        inline Conversion() : _bIsExplicit(true)   {}


      public:

        static ConversionPtr Create(const BaseClasses::TypeInfo &crConvertType, BaseClasses::ExpressionPtr spSubExpression = nullptr, bool bExplicit = true);

        virtual ~Conversion() {}


        inline BaseClasses::TypeInfo  GetConvertType() const                                    { return _ConvertType; }
        inline void                   SetConvertType(const BaseClasses::TypeInfo &crConvType)   { _ConvertType = crConvType; }

        inline bool GetExplicit() const             { return _bIsExplicit; }
        inline void SetExplicit(bool bIsExplicit)   { _bIsExplicit = bIsExplicit; }

      public:

        virtual BaseClasses::TypeInfo GetResultType() const final override    { return GetConvertType(); }

        virtual std::string DumpToXML(const size_t cszIntend) const final override;
      };

      class Parenthesis final : public UnaryExpression
      {
      private:

        friend class AST;

        inline Parenthesis()  {}


      public:

        static ParenthesisPtr Create(BaseClasses::ExpressionPtr spSubExpression = nullptr);

        virtual ~Parenthesis()  {}


      public:

        virtual BaseClasses::TypeInfo GetResultType() const final override;

        virtual std::string DumpToXML(const size_t cszIntend) const final override;
      };

      class UnaryOperator final : public UnaryExpression
      {
      private:

        friend class AST;

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

        inline UnaryOperator() : _eOpType(UnaryOperatorType::Plus)  {}


      public:

        static UnaryOperatorPtr Create(UnaryOperatorType eType = UnaryOperatorType::Plus, BaseClasses::ExpressionPtr spSubExpression = nullptr);

        virtual ~UnaryOperator()  {}


        static std::string GetOperatorTypeString(UnaryOperatorType eType);


        inline UnaryOperatorType  GetOperatorType() const                     { return _eOpType; }
        inline void               SetOperatorType(UnaryOperatorType eOpType)  { _eOpType = eOpType; }


      public:

        virtual std::string DumpToXML(const size_t cszIntend) const final override;

        virtual BaseClasses::TypeInfo GetResultType() const final override;
      };


      class BinaryOperator : public BaseClasses::Expression
      {
      private:

        typedef BaseClasses::ExpressionPtr        ExpressionPtr;
        typedef BaseClasses::ExpressionConstPtr   ExpressionConstPtr;

        ExpressionPtr             _spLHS;
        ExpressionPtr             _spRHS;

      protected:

        std::string _DumpSubExpressionsToXML(const size_t cszIntend) const;

        inline BinaryOperator()   {}

      public:

        virtual ~BinaryOperator() {}


        inline ExpressionPtr        GetLHS()                        { return _spLHS; }
        inline ExpressionConstPtr   GetLHS() const                  { return _spLHS; }
        inline void                 SetLHS(ExpressionPtr spNewLHS)  { _SetChildPtr(_spLHS, spNewLHS); }

        inline ExpressionPtr        GetRHS()                        { return _spRHS; }
        inline ExpressionConstPtr   GetRHS() const                  { return _spRHS; }
        inline void                 SetRHS(ExpressionPtr spNewRHS)  { _SetChildPtr(_spRHS, spNewRHS); }


      public:

        virtual ExpressionPtr GetSubExpression(IndexType SubExprIndex) override;
        virtual IndexType     GetSubExpressionCount() const override      { return static_cast< IndexType >(2); }
        virtual void          SetSubExpression(IndexType SubExprIndex, ExpressionPtr spSubExpr) override;
      };

      class ArithmeticOperator final : public BinaryOperator
      {
      private:

        friend class AST;

        typedef BaseClasses::ExpressionPtr  ExpressionPtr;
        typedef BaseClasses::TypeInfo       TypeInfo;
        typedef TypeInfo::KnownTypes        KnownTypes;

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

        inline ArithmeticOperator() : _eOpType(ArithmeticOperatorType::Add)  {}


      public:

        static ArithmeticOperatorPtr Create(ArithmeticOperatorType eOpType = ArithmeticOperatorType::Add, ExpressionPtr spLHS = nullptr, ExpressionPtr spRHS = nullptr);

        virtual ~ArithmeticOperator() {}


        static std::string GetOperatorTypeString(ArithmeticOperatorType eType);


        inline ArithmeticOperatorType GetOperatorType() const                           { return _eOpType; }
        inline void                   SetOperatorType(ArithmeticOperatorType eOpType)   { _eOpType = eOpType; }


      public:

        virtual BaseClasses::TypeInfo GetResultType() const final override;

        virtual std::string DumpToXML(const size_t cszIntend) const final override;

      };

      class AssignmentOperator final : public BinaryOperator
      {
      private:

        friend class AST;

        typedef BinaryOperator              BaseType;
        typedef BaseClasses::ExpressionPtr  ExpressionPtr;

      private:

        IdentifierPtr   _spMask;

        inline AssignmentOperator() : _spMask(nullptr)  {}


      public:

        static AssignmentOperatorPtr Create(ExpressionPtr spLHS = nullptr, ExpressionPtr spRHS = nullptr, IdentifierPtr spMask = nullptr);

        virtual ~AssignmentOperator() {}

        inline IdentifierPtr        GetMask()                         { return _spMask; }
        inline IdentifierConstPtr   GetMask() const                   { return _spMask; }
        inline void                 SetMask(IdentifierPtr spNewMask)  { _SetChildPtr(_spMask, spNewMask); }


        inline bool IsMasked() const  { return static_cast<bool>( GetMask() ); }


      public:

        virtual BaseClasses::TypeInfo GetResultType() const final override;

        virtual std::string DumpToXML(const size_t cszIntend) const final override;


        virtual ExpressionPtr GetSubExpression(IndexType SubExprIndex) final override;
        virtual IndexType     GetSubExpressionCount() const final override;
        virtual void          SetSubExpression(IndexType SubExprIndex, ExpressionPtr spSubExpr) final override;
      };

      class RelationalOperator final : public BinaryOperator
      {
      private:

        friend class AST;

        typedef BaseClasses::ExpressionPtr   ExpressionPtr;

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

        inline RelationalOperator() : _eOpType(RelationalOperatorType::Equal)  {}


      public:

        static RelationalOperatorPtr  Create(RelationalOperatorType eOpType = RelationalOperatorType::Equal, ExpressionPtr spLHS = nullptr, ExpressionPtr spRHS = nullptr);

        virtual ~RelationalOperator() {}


        static std::string GetOperatorTypeString(RelationalOperatorType eType);


        BaseClasses::TypeInfo GetComparisonType() const;

        inline RelationalOperatorType GetOperatorType() const                           { return _eOpType; }
        inline void                   SetOperatorType(RelationalOperatorType eOpType)   { _eOpType = eOpType; }


      public:

        virtual BaseClasses::TypeInfo GetResultType() const final override;

        virtual std::string DumpToXML(const size_t cszIntend) const final override;

      };


      class FunctionCall : public BaseClasses::Expression
      {
      private:

        friend class AST;

        typedef BaseClasses::ExpressionPtr        ExpressionPtr;
        typedef BaseClasses::ExpressionConstPtr   ExpressionConstPtr;

      private:

        std::string                     _strName;
        BaseClasses::TypeInfo           _ReturnType;
        std::vector< ExpressionPtr >    _vecCallParams;

        inline FunctionCall()   {}


      public:

        static FunctionCallPtr Create(std::string strFunctionName, const BaseClasses::TypeInfo &crReturnType);

        virtual ~FunctionCall() {}


        void          AddCallParameter(ExpressionPtr spCallParam);
        ExpressionPtr GetCallParameter(IndexType CallParamIndex);
        void          SetCallParameter(IndexType CallParamIndex, ExpressionPtr spCallParam);

        inline IndexType          GetCallParameterCount() const    { return static_cast< IndexType >(_vecCallParams.size()); }
        inline ExpressionConstPtr GetCallParameter(IndexType CallParamIndex) const
        {
          return const_cast< FunctionCall* >( this )->GetCallParameter( CallParamIndex );
        }


        inline std::string  GetName() const                   { return _strName; }
        inline void         SetName(std::string strNewName)   { _strName = strNewName; }

        inline BaseClasses::TypeInfo GetReturnType() const                                      { return _ReturnType; }
        inline void                  SetReturnType(const BaseClasses::TypeInfo &crReturnType)   { _ReturnType = crReturnType; }


      public:

        virtual std::string DumpToXML(const size_t cszIntend) const final override;

        virtual BaseClasses::TypeInfo GetResultType() const final override    { return GetReturnType(); }

        virtual ExpressionPtr   GetSubExpression(IndexType SubExprIndex)                                          { return GetCallParameter(SubExprIndex); }
        virtual IndexType       GetSubExpressionCount() const final override                                      { return GetCallParameterCount(); }
        virtual void            SetSubExpression(IndexType SubExprIndex, ExpressionPtr spSubExpr) final override  { SetCallParameter(SubExprIndex, spSubExpr); }
      };
    };


    class VectorSupport final
    {
    // Public type definitions
    public:

      DEFINE_NODE_TYPES( VectorExpression );
      DEFINE_NODE_TYPES( BroadCast );
      DEFINE_NODE_TYPES( CheckActiveElements );
      DEFINE_NODE_TYPES( VectorIndex );


    // Class definitions
    public:

      class VectorExpression : public BaseClasses::Expression
      {
      protected:

        inline VectorExpression()   {}

      public:

        virtual ~VectorExpression()  {}
      };

      class BroadCast final : public VectorExpression
      {
      private:

        friend class AST;

        typedef BaseClasses::ExpressionPtr        ExpressionPtr;
        typedef BaseClasses::ExpressionConstPtr   ExpressionConstPtr;

      private:

        ExpressionPtr _spSubExpression;

        inline BroadCast() : _spSubExpression(nullptr)   {}


      public:

        static BroadCastPtr Create(ExpressionPtr spSubExpression = nullptr);

        virtual ~BroadCast()  {}


        inline ExpressionPtr        GetSubExpression()                          { return _spSubExpression; }
        inline ExpressionConstPtr   GetSubExpression() const                    { return _spSubExpression; }
        inline void                 SetSubExpression(ExpressionPtr spSubExpr)   { _SetChildPtr(_spSubExpression, spSubExpr); }


      public:

        virtual bool IsVectorized() final override      { return true; }

        virtual BaseClasses::TypeInfo  GetResultType() const final override;

        virtual ExpressionPtr   GetSubExpression(IndexType SubExprIndex) final override;
        virtual IndexType       GetSubExpressionCount() const final override      { return static_cast< IndexType >( 1 ); }
        virtual void            SetSubExpression(IndexType SubExprIndex, ExpressionPtr spSubExpr) final override;

        virtual std::string DumpToXML(const size_t cszIntend) const final override;
      };

      class CheckActiveElements final : public VectorExpression
      {
      private:

        friend class AST;

        typedef BaseClasses::ExpressionPtr        ExpressionPtr;
        typedef BaseClasses::ExpressionConstPtr   ExpressionConstPtr;

      public:

        enum class CheckType
        {
          All,
          Any,
          None
        };

      private:

        CheckType     _eCheckType;
        ExpressionPtr _spSubExpression;

        inline CheckActiveElements() : _eCheckType(CheckType::All), _spSubExpression(nullptr)  {}


      public:

        static CheckActiveElementsPtr Create(CheckType eCheckType = CheckType::All, ExpressionPtr spSubExpression = nullptr);

        virtual ~CheckActiveElements()  {}


        static std::string GetCheckTypeString(CheckType eType);


        inline CheckType  GetCheckType() const                    { return _eCheckType; }
        inline void       SetCheckType(CheckType eNewCheckType)   { _eCheckType = eNewCheckType; }

        inline ExpressionPtr        GetSubExpression()                          { return _spSubExpression; }
        inline ExpressionConstPtr   GetSubExpression() const                    { return _spSubExpression; }
        inline void                 SetSubExpression(ExpressionPtr spSubExpr)   { _SetChildPtr(_spSubExpression, spSubExpr); }

      public:

        virtual bool IsVectorized() final override                            { return false; }

        virtual BaseClasses::TypeInfo  GetResultType() const final override   { return BaseClasses::TypeInfo( BaseClasses::TypeInfo::KnownTypes::Bool, true, false ); }

        virtual ExpressionPtr   GetSubExpression(IndexType SubExprIndex) final override;
        virtual IndexType       GetSubExpressionCount() const final override                                      { return static_cast< IndexType >(1); }
        virtual void            SetSubExpression(IndexType SubExprIndex, ExpressionPtr spSubExpr) final override;

        virtual std::string DumpToXML(const size_t cszIntend) const final override;
      };

      class VectorIndex final : public VectorExpression
      {
      private:

        friend class AST;

        typedef BaseClasses::ExpressionPtr          ExpressionPtr;
        typedef BaseClasses::TypeInfo::KnownTypes   KnownTypes;

      private:

        KnownTypes  _eType;

        inline VectorIndex() : _eType(KnownTypes::Int32)  {}

      public:

        static VectorIndexPtr Create(KnownTypes eType = KnownTypes::Int32);

        virtual ~VectorIndex()  {}


        inline KnownTypes   GetType() const                 { return _eType; }
        inline void         SetType(KnownTypes eNewType)    { _eType = eNewType; }


      public:

        virtual bool IsVectorized() final override      { return true; }

        virtual BaseClasses::TypeInfo  GetResultType() const final override;

        virtual ExpressionPtr   GetSubExpression(IndexType SubExprIndex) final override                           { throw ASTExceptions::ChildIndexOutOfRange(); }
        virtual IndexType       GetSubExpressionCount() const final override                                      { return static_cast< IndexType >(0); }
        virtual void            SetSubExpression(IndexType SubExprIndex, ExpressionPtr spSubExpr) final override  { throw ASTExceptions::ChildIndexOutOfRange(); }

        virtual std::string DumpToXML(const size_t cszIntend) const final override;
      };
    };



    /** \brief  Base class for all VAST nodes, which may contain variable declarations. */
    class IVariableContainer : public BaseClasses::Node
    {
    protected:

      inline IVariableContainer()   {}

    public:

      virtual ~IVariableContainer() {}


      /** \brief  Adds a new variable declaration to the list of known variables.
       *  \param  spVariableInfo  A shared pointer to the VariableInfo object, describing the new variable declaration. */
      virtual void                          AddVariable(BaseClasses::VariableInfoPtr spVariableInfo) = 0;

      /** \brief    Returns a shared pointer to a VariableInfo object for a known variable declaration.
       *  \param    strVariableName   The unique name of the variable, whose declaration shall be looked up.
       *  \remarks  If the specified variable declaration is not known, <b>nullptr</b> will be returned. */
      virtual BaseClasses::VariableInfoPtr  GetVariableInfo(std::string strVariableName) = 0;

      /** \brief  Checks, whether a sepcific variable is already known.
       *  \param  crstrVariableName   The unique name of the variable, whose presence shall be checked. */
      virtual bool                          IsVariableUsed(const std::string &crstrVariableName) const = 0;


      /** \brief    Returns a constant shared pointer to a VariableInfo object for a known variable declaration.
       *  \param    strVariableName   The unique name of the variable, whose declaration shall be looked up.
       *  \remarks  If the specified variable declaration is not known, <b>nullptr</b> will be returned. */
      inline BaseClasses::VariableInfoConstPtr  GetVariableInfo(std::string strVariableName) const
      {
        return const_cast< IVariableContainer* >( this )->GetVariableInfo( strVariableName );
      }
    };

    /** \brief  Describes a syntactic scope, i.e. a compound of statements. */
    class Scope final : public IVariableContainer
    {
    public:

      typedef std::vector< Expressions::IdentifierPtr >   VariableDeclarationVectorType;  //!< Type definition for a list of declared variables.

    private:

      friend class AST;

      typedef BaseClasses::NodePtr    NodePtr;    //!< Type alias for shared pointers to class <b>BaseClasses::Node</b>.

      typedef std::vector< NodePtr >  ChildrenContainerType;


    private:

      ChildrenContainerType     _Children;
      std::set< std::string >   _setDeclaredVariables;

      inline Scope()  {}


      /** \brief  Returns a shared pointer to the first parent VAST node, which is derived from the class IVariableContainer. */
      IVariableContainerPtr               _GetParentVariableContainer();

      /** \brief  Returns a constant shared pointer to the first parent VAST node, which is derived from the class IVariableContainer. */
      inline IVariableContainerConstPtr   _GetParentVariableContainer() const { return const_cast< Scope* >(this)->_GetParentVariableContainer(); }


    public:

      /** \brief  Creates a new object of this class. */
      static ScopePtr Create();

      virtual ~Scope()  {}


      /** \name Public methods for accessing the child node container. */
      //@{

      /** \brief  Adds another VAST node at the end of the node container.
       *  \param  spChild   A shared pointer to the node, which shall be added to the scope. */
      void      AddChild(NodePtr spChild);

      /** \brief    Returns the position index of a specific child node inside the scope.
       *  \param    spChildNode   A shared pointer to the child node, whose position index shall be retrived.
       *  \remarks  If the specified VAST node cannot be found inside the scope, an exception will be thrown. */
      IndexType GetChildIndex(NodePtr spChildNode);

      /** \brief    Inserts a new VAST node at a certain position inside the scope.
       *  \param    ChildIndex    The index of the position, the new VAST node shall be inserted to.
       *  \param    spChildNode   The new VAST node, that shall be inserted into the scope.
       *  \remarks  If the index is out of range, a <b>ASTExceptions::ChildIndexOutOfRange</b> exception will be thrown. */
      void      InsertChild(IndexType ChildIndex, NodePtr spChildNode);

      /** \brief    Removes a specific child node from the scope.
       *  \param    ChildIndex  The index of the child node, which shall be removed.
       *  \remarks  If the index is out of range, a <b>ASTExceptions::ChildIndexOutOfRange</b> exception will be thrown. */
      void      RemoveChild(IndexType ChildIndex);

      /** \brief    Replaces a specific child node of the scope with another VAST node.
       *  \param    ChildIndex    The index of the child node, which shall be replaced.
       *  \param    spChildNode   The new VAST node, that shall be inserted into the scope.
       *  \remarks  If the index is out of range, a <b>ASTExceptions::ChildIndexOutOfRange</b> exception will be thrown. */
      void      SetChild(IndexType ChildIndex, NodePtr spChildNode);

      //@}



      /** \name Public methods for accessing the child node container. */
      //@{

      /** \brief    Adds a new variable declaration to this scope.
       *  \param    spVariableInfo  A shared pointer to the VariableInfo object, describing the variable declaration that shall be added.
       *  \remarks  This function automatically adds the variable declaration into the enclosing FunctionDeclaration object of this scope. */
      void                            AddVariableDeclaration(BaseClasses::VariableInfoPtr spVariableInfo);

      /** \brief  Returns a list of all variables, which are declared exactly in the current scope. */
      VariableDeclarationVectorType   GetVariableDeclarations() const;

      /** \brief  Checks, whether a certain variable is declared exactly in the current scope.
       *  \param  strVariableName   The unique name of the variable, which shall be checked. */
      inline bool                     HasVariableDeclaration(std::string strVariableName) const   { return (_setDeclaredVariables.count(strVariableName) != 0); }

      /** \brief    Imports all variable declarations of another scope into the current one.
       *  \param    spOtherScope  A shared pointer to the Scope object, whose variable declarations shall be imported.
       *  \remarks  All variable declarations of the imported scope will be cleared by this function. */
      void                            ImportVariableDeclarations(ScopePtr spOtherScope);

      //@}

      /** \brief  Fully devours another scope, i.e. all its internal child nodes and variable declarations will be appended to the current scope.
       *  \param  spOtherScope  A shared pointer to the Scope object, which shall be imported. Its contents will be cleared in that process. */
      void ImportScope(ScopePtr spOtherScope);

      /** \brief  Returns <b>true</b> if and only if this scope has no child nodes. */
      inline bool IsEmpty() const   { return (GetChildCount() == static_cast<IndexType>(0)); }


    public:

      /** \name Public methods inherited from class IVariableContainer. */
      //@{

      virtual void                          AddVariable(BaseClasses::VariableInfoPtr spVariableInfo) final override;
      virtual BaseClasses::VariableInfoPtr  GetVariableInfo(std::string strVariableName) final override;
      virtual bool                          IsVariableUsed(const std::string &crstrVariableName) const final override;

      //@}


      /** \name Public methods inherited from class BaseClasses::Node. */
      //@{

      virtual NodePtr     GetChild(IndexType ChildIndex) final override;
      virtual IndexType   GetChildCount() const final override  { return static_cast< IndexType >(_Children.size()); }

      virtual std::string DumpToXML(const size_t cszIntend) const final override;

      //@}
    };

    /** \brief    Describes a function declaration.
     *  \remarks  This class is the main declaration context for all variables declared inside a function. */
    class FunctionDeclaration final : public IVariableContainer
    {
    private:

      friend class AST;

      typedef BaseClasses::NodePtr  NodePtr;    //!< Type alias for shared pointers to class <b>BaseClasses::Node</b>.

      typedef std::vector< Expressions::IdentifierPtr >              ParameterContainerType;
      typedef std::map< std::string, BaseClasses::VariableInfoPtr >  KnownVariablesMapType;


    private:

      ParameterContainerType  _Parameters;
      ScopePtr                _spBody;
      KnownVariablesMapType   _mapKnownVariables;
      std::string             _strName;

      inline FunctionDeclaration() : _spBody(nullptr)  {}


    public:

      /** \brief  Creates a new object of this class.
       *  \param  strFunctionName   The requested name of the function declaration. */
      static FunctionDeclarationPtr Create(std::string strFunctionName);

      virtual ~FunctionDeclaration()  {}


      /** \name Public methods for accessing the function paramater container. */
      //@{

      /** \brief  Adds a new function parameter at the end of the current parameter list.
       *  \param  spVariableInfo    A shared pointer to the VariableInfo object, describing the new parameter declaration. */
      void                        AddParameter(BaseClasses::VariableInfoPtr spVariableInfo);

      /** \brief    Returns a shared pointer to the identifier object of a specific function parameter.
       *  \param    iParamIndex   The index of the function parameter, which shall be returned.
       *  \remarks  If the function parameter index is out of range, a <b>ASTExceptions::ChildIndexOutOfRange</b> exception will be thrown. */
      Expressions::IdentifierPtr  GetParameter(IndexType iParamIndex);

      /** \brief  Returns the number of parameters for this function declaration. */
      inline IndexType            GetParameterCount() const   { return static_cast< IndexType >( _Parameters.size() ); }

      /** \brief    Replaces an existing function parameter with a new one.
       *  \param    iParamIndex       The index of the function parameter, which shall be replaced.
       *  \param    spVariableInfo    A shared pointer to the VariableInfo object, describing the new parameter declaration.
       *  \remarks  If the function parameter index is out of range, a <b>ASTExceptions::ChildIndexOutOfRange</b> exception will be thrown. */
      void                        SetParameter(IndexType iParamIndex, BaseClasses::VariableInfoPtr spVariableInfo);


      /** \brief    Returns a constant shared pointer to the identifier object of a specific function parameter.
       *  \param    iParamIndex   The index of the function parameter, which shall be returned.
       *  \remarks  If the function parameter index is out of range, a <b>ASTExceptions::ChildIndexOutOfRange</b> exception will be thrown. */
      inline Expressions::IdentifierConstPtr  GetParameter(IndexType iParamIndex) const
      {
        return const_cast< FunctionDeclaration* >( this )->GetParameter( iParamIndex );
      }

      //@}


      /** \brief  Returns a shared pointer to the scope, which contains the whole function body. */
      ScopePtr        GetBody();

      /** \brief  Returns a constant shared pointer to the scope, which contains the whole function body. */
      ScopeConstPtr   GetBody() const;


      /** \brief  Returns the name of the function declaration. */
      inline std::string  GetName() const               { return _strName; }

      /** \brief  Changes the name of the function declaration.
       *  \param  strName   The requested new function name. */
      inline void         SetName(std::string strName)  { _strName = strName; }

    public:

      /** \brief  Returns the names of all known variable declarations inside the function, including its parameters. */
      std::vector< std::string >  GetKnownVariableNames() const;


      /** \name Public methods inherited from class IVariableContainer. */
      //@{

      virtual void                          AddVariable(BaseClasses::VariableInfoPtr spVariableInfo) final override;
      virtual BaseClasses::VariableInfoPtr  GetVariableInfo(std::string strVariableName) final override;
      virtual bool                          IsVariableUsed(const std::string &crstrVariableName) const final override;

      //@}


      /** \name Public methods inherited from class BaseClasses::Node. */
      //@{

      virtual NodePtr     GetChild(IndexType ChildIndex) final override;
      virtual IndexType   GetChildCount() const final override  { return static_cast< IndexType >(1); }

      virtual std::string DumpToXML(const size_t cszIntend) const final override;

      //@}
    };

  private:

    /** \brief    Generic internal method, which creates a new object of a VAST class.
     *  \tparam   NodeClass   The type of the VAST object, which shall be created. It must be derived from class <b>BaseClasses::Node</b>.
     *  \return   A shared pointer to the newly created VAST object. */
    template < class NodeClass >
    inline static std::shared_ptr< NodeClass > CreateNode()
    {
      static_assert(std::is_base_of< BaseClasses::Node, NodeClass >::value, "All nodes of the vectorizable AST must be derived from class \"Node\"");

      std::shared_ptr< NodeClass > spNode( new NodeClass );

      spNode->_wpThis = BaseClasses::NodePtr( spNode );

      return spNode;
    }
  };


  template <> inline bool AST::Expressions::Constant::GetValue<bool>() const
  {
    switch (_eType)
    {
    case KnownTypes::Float: case KnownTypes::Double:
      return (_unionValues.dFloatingPointValue != 0.);
    default:
      return (_unionValues.ui64IntegralValue != static_cast< std::uint64_t >(0));
    }
  }

  template <> inline void AST::Expressions::Constant::SetValue<bool>(bool TValue)
  {
    _unionValues.ui64IntegralValue  = static_cast< std::uint64_t >( TValue ? 1 : 0 );
    _eType                          = KnownTypes::Bool;
  }
} // end namespace Vectorization
} // end namespace Backend
} // end namespace hipacc
} // end namespace clang

#undef DEFINE_NODE_TYPES

#endif  // _BACKEND_VECTORIZATION_AST_H_

// vim: set ts=2 sw=2 sts=2 et ai:

