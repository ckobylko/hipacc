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

//===--- Vectorizer.h - Implements a vectorizing component for clang's syntax trees. -===//
//
// This file implements a vectorizing component for clang's syntax trees
//
//===---------------------------------------------------------------------------------===//

#ifndef _BACKEND_VECTORIZER_H_
#define _BACKEND_VECTORIZER_H_

#include <list>
#include <map>
#include <type_traits>
#include "ClangASTHelper.h"
#include "CommonDefines.h"
#include "VectorizationAST.h"

#include "stdio.h"

namespace clang
{
namespace hipacc
{
namespace Backend
{
/** \brief  Contains all classes and type definitions, which are used during the vectorization process. */
namespace Vectorization
{
  /** \brief  Implements the whole algorithmics required for the vectorization of a function. */
  class Vectorizer final
  {
  public:

    /** \brief  Base class for all classes, which export an abstract vectorized AST back into a Clang-specific AST. */
    class VASTExporterBase
    {
    protected:

      typedef ClangASTHelper::FunctionDeclarationVectorType   FunctionDeclVectorType;   //!< Type alias for a vector of function declarations.
      typedef ClangASTHelper::QualTypeVectorType              QualTypeVectorType;       //!< Type alias for a vector of qualified types.

    private:

      typedef std::map< unsigned int, FunctionDeclVectorType >        FunctionDeclParamCountMapType;
      typedef std::map< std::string, FunctionDeclParamCountMapType >  FunctionDeclNameMapType;


      ClangASTHelper            _ASTHelper;
      ::clang::DeclContext      *_pDeclContext;

      FunctionDeclNameMapType                       _mapKnownFunctions;
      std::map< std::string, ::clang::ValueDecl* >  _mapKnownDeclarations;


    private:

      VASTExporterBase(const VASTExporterBase &)            = delete;
      VASTExporterBase& operator=(const VASTExporterBase &) = delete;


      /** \brief  Returns the Clang type-equivalent of a VAST variable variable declaration. */
      ::clang::QualType _GetVariableType(AST::BaseClasses::VariableInfoPtr spVariableInfo);


    protected:

      /** \brief  Constructor.
       *  \param  rAstContext   A reference to the current Clang AST context.  */
      VASTExporterBase(::clang::ASTContext &rAstContext);


      /** \brief  Returns a reference to the currently used ClangASTHelper object. */
      inline ClangASTHelper&      _GetASTHelper()     { return _ASTHelper; }

      /** \brief  Returns a reference to the current Clang AST context. */
      inline ::clang::ASTContext& _GetASTContext()    { return _GetASTHelper().GetASTContext(); }


      /** \brief    Resets the internal state of the VAST exporter.
       *  \remarks  This function should be called everytime in between the exports of two different VAST function declarations. */
      void _Reset();


      /** \brief  Adds a new Clang function declaration to the current AST context.
       *  \param  pFunctionDecl   A pointer to the function declaration object, which shall be added to the AST context. */
      void _AddKnownFunctionDeclaration(::clang::FunctionDecl *pFunctionDecl);


      /** \brief  Converts a VAST constant into a corresponding Clang literal.
       *  \param  spConstant  A shared pointer to the VAST constant node, which shall be converted. */
      ::clang::Expr*          _BuildConstant(AST::Expressions::ConstantPtr spConstant);

      /** \brief    Converts a VAST function declaration into a Clang function declaration and adds it to the current AST context.
       *  \param    spFunction  A shared pointer to the VAST function declaration node, which shall be converted.
       *  \remarks  This method does not process the function body, but only its declaration header. */
      ::clang::FunctionDecl*  _BuildFunctionDeclaration(AST::FunctionDeclarationPtr spFunction);

      /** \brief  Creates a Clang loop statement object.
       *  \param  eLoopType     The VAST-specific type of the loop. It affects the type of the created Clang loop statement.
       *  \param  pCondition    A pointer to the Clang expression object, which shall be used as the loop condition.
       *  \param  pBody         A pointer to the Clang statement object, which describes the body of the loop.
       *  \param  pIncrement    A pointer to an optional Clang expression object, which describes the increment expression for each loop iteration. */
      ::clang::Stmt*          _BuildLoop(AST::ControlFlow::Loop::LoopType eLoopType, ::clang::Expr *pCondition, ::clang::Stmt *pBody, ::clang::Expr *pIncrement = nullptr);

      /** \brief  Converts a VAST loop control statement into its Clang counterpart.
       *  \param  spLoopControl   A shared pointer to the VAST loop control statement node, which shall be converted. */
      ::clang::Stmt*          _BuildLoopControlStatement(AST::ControlFlow::LoopControlStatementPtr spLoopControl);

      /** \brief  Creates a Clang declaration statement object for a specific VAST variable.
       *  \param  spIdentifier      A shared pointer to the VAST indentifier node, which references the variable that shall be declared.
       *  \param  pInitExpression   A pointer to an optional Clang expression object, which can be used for the initialization of the variable declaration. */
      ::clang::ValueDecl*     _BuildValueDeclaration(AST::Expressions::IdentifierPtr spIdentifier, ::clang::Expr *pInitExpression = nullptr);


      /** \brief  Converts a VAST-specific arithmetic operator type into its Clang counterpart.
       *  \param  eOpType   The VAST-specific arithmetic operator type, which shall be converted. */
      static ::clang::BinaryOperatorKind  _ConvertArithmeticOperatorType(AST::Expressions::ArithmeticOperator::ArithmeticOperatorType eOpType);

      /** \brief  Converts a VAST-specific relational operator type into its Clang counterpart.
       *  \param  eOpType   The VAST-specific relational operator type, which shall be converted. */
      static ::clang::BinaryOperatorKind  _ConvertRelationalOperatorType(AST::Expressions::RelationalOperator::RelationalOperatorType eOpType);

      /** \brief  Converts a VAST-specific unary operator type into its Clang counterpart.
       *  \param  eOpType   The VAST-specific unary operator type, which shall be converted. */
      static ::clang::UnaryOperatorKind   _ConvertUnaryOperatorType(AST::Expressions::UnaryOperator::UnaryOperatorType eOpType);


      /** \brief  Converts a VAST-specific type information into a qualified Clang type.
       *  \param  crTypeInfo  A constant reference to the VAST type information object, which shall be converted. */
      ::clang::QualType       _ConvertTypeInfo(const AST::BaseClasses::TypeInfo &crTypeInfo);


      /** \brief  Creates all kinds of explicit Clang cast expression objects.
       *  \param  crSourceType  The VAST-specific type of the sub-expression for the cast expression.
       *  \param  crTargetType  The requested VAST-specific target type of the cast expression.
       *  \param  pSubExpr      A pointer to the Clang expression object, whose return value shall be casted. */
      ::clang::CastExpr*      _CreateCast(const AST::BaseClasses::TypeInfo &crSourceType, const AST::BaseClasses::TypeInfo &crTargetType, ::clang::Expr *pSubExpr);

      /** \brief  Creates Clang cast expression objects for pointer casts.
       *  \param  crSourceType  The VAST-specific type of the sub-expression for the cast expression. It must be a pointer type.
       *  \param  crTargetType  The requested VAST-specific target type of the cast expression. It must be a pointer type.
       *  \param  pSubExpr      A pointer to the Clang expression object, whose return value shall be casted. */
      ::clang::CastExpr*      _CreateCastPointer(const AST::BaseClasses::TypeInfo &crSourceType, const AST::BaseClasses::TypeInfo &crTargetType, ::clang::Expr *pSubExpr);

      /** \brief  Creates all kinds of explicit Clang cast expression objects for scalar single values, i.e. no pointers or arrays.
       *  \param  crSourceType  The VAST-specific type of the sub-expression for the cast expression. It must be a single element type.
       *  \param  crTargetType  The requested VAST-specific target type of the cast expression. It must be a single element type.
       *  \param  pSubExpr      A pointer to the Clang expression object, whose return value shall be casted. */
      ::clang::CastExpr*      _CreateCastSingleValue(const AST::BaseClasses::TypeInfo &crSourceType, const AST::BaseClasses::TypeInfo &crTargetType, ::clang::Expr *pSubExpr);


      /** \brief  Creates a Clang declaration reference expression object for a particular variable.
       *  \param  strValueName  The unique name of the variable, which shall be referenced. */
      ::clang::DeclRefExpr*   _CreateDeclarationReference(std::string strValueName);

      /** \brief  Creates a Clang parenthesis expression around another expression.
       *  \param  pSubExpr  A pointer to the Clang expression object, which shall be wrapped into a parenthesis. */
      ::clang::ParenExpr*     _CreateParenthesis(::clang::Expr *pSubExpr);


      /** \brief    Returns the known Clang function declaration object, which is the best match for a specific calling syntax.
       *  \param    strFunctionName   The fully qualified name of the function, which shall be looked up.
       *  \param    crvecArgTypes     A vector of the qualified Clang types of all call parameters, which is used to select the correct function overload.
       *  \remarks  If no matching function declaration could be found, <b>nullptr</b> will be returned. */
      ::clang::FunctionDecl*  _GetFirstMatchingFunctionDeclaration(std::string strFunctionName, const QualTypeVectorType &crvecArgTypes);

      /** \brief  Returns a list of all known function declaration objects, which match a specified function name and call parameter count.
       *  \param  strFunctionName   The fully qualified name of the functions, which shall be looked up.
       *  \param  uiParamCount      The number of requested call parameters for the function lookup. */
      FunctionDeclVectorType  _GetMatchingFunctionDeclarations(std::string strFunctionName, unsigned int uiParamCount);

      /** \brief  Checks, whether a specific variable declaration is already known.
       *  \param  strDeclName   The unique name of the variable, which shall be checked for a conflicting declaration. */
      bool _HasValueDeclaration(std::string strDeclName);


      /** \brief    Abstract method, which returns the qualified Clang type of a <b>vectorized</b> VAST-specific type. 
       *  \param    crOriginalTypeInfo  A constant reference to the VAST-specific type information object, whose vectorized Clang type counterpart shall be returned.
       *  \remarks  As this type matching depends on the target architecture, this method must be implemented by the derived classes. */
      virtual ::clang::QualType _GetVectorizedType(AST::BaseClasses::TypeInfo &crOriginalTypeInfo) = 0;

    public:

      virtual ~VASTExporterBase()
      {
        _Reset();

        _mapKnownFunctions.clear();
      }
    };


  private:

    typedef AST::IndexType IndexType;   //!< Type alias for the internally used index type of the VAST.

    /** \brief  Internal class, which handles the conversion of a Clang-specific AST into an abstract vectorized AST. */
    class VASTBuilder
    {
    private:

      /** \brief    Internal helper class, which handles the mapping of declared variable names to unique variable names.
       *  \remarks  The C++ language allows variable hiding by a re-declaration of the same variable name in a nested scope.
       *            Since the VAST requires unique variable names throughout the whole function, this mapping is required. */
      class VariableNameTranslator final
      {
      private:

        typedef std::map< std::string, std::string >  RenameMapType;

        std::list< RenameMapType >    _lstRenameStack;

      public:

        /** \brief    Adds a new layer to the declaration stack.
         *  \remarks  This method should be called, whenever the AST converter enters a scope. */
        inline void AddLayer()  { _lstRenameStack.push_front(RenameMapType()); }

        /** \brief    Removes the current layer from the declaration stack.
         *  \remarks  This method should be called, whenever the AST converter leaves a scope. */
        inline void PopLayer()  { _lstRenameStack.pop_front(); }


        /** \brief  Adds a new variable name mapping into the lookup table.
         *  \param  strOriginalName   The name of the variable in its original declaration.
         *  \param  strNewName        The requested unique name for this variable. */
        void          AddRenameEntry( std::string strOriginalName, std::string strNewName );

        /** \brief  Returns the mapped unique name for a specific variable. 
         *  \param  strOriginalName   The name of the variable in its original declaration. */
        std::string   TranslateName( std::string strOriginalName ) const;
      };


    private:

      /** \brief  Returns the VAST-specific counterpart of a qualified Clang type.
       *  \param  qtSourceType  The qualified Clang type, which shall be converted. */
      inline static AST::BaseClasses::TypeInfo _ConvertTypeInfo(::clang::QualType qtSourceType)
      {
        AST::BaseClasses::TypeInfo ReturnType;

        _ConvertTypeInfo(ReturnType, qtSourceType);

        return ReturnType;
      }

      /** \brief  Converts a qualified Clang type and writes its contents to a VAST-specific type information object.
       *  \param  rTypeInfo     A reference to the VAST type information object, which shall be filled with the converted type information.
       *  \param  qtSourceType  The qualified Clang type, which shall be converted. */
      static void _ConvertTypeInfo(AST::BaseClasses::TypeInfo &rTypeInfo, ::clang::QualType qtSourceType);



      /** \brief  Converts a Clang binary operator object (and its sub-expressions) into a VAST binary operator node.
       *  \param  pExprLHS  A pointer to the Clang expression object, which describes the left-hand-side of the binary operator.
       *  \param  pExprRHS  A pointer to the Clang expression object, which describes the right-hand-side of the binary operator.
       *  \param  eOpKind   The Clang-specific operator code of the binary operator. */
      AST::Expressions::BinaryOperatorPtr   _BuildBinaryOperatorExpression(::clang::Expr *pExprLHS, ::clang::Expr *pExprRHS, ::clang::BinaryOperatorKind eOpKind);

      /** \brief    Converts a Clang if-statement object into a VAST branching statement node.
       *  \param    pIfStmt           A pointer to the Clang if-statement object, which shall be converted.
       *  \param    spEnclosingScope  A shared pointer to the VAST scope node, the new branching statement shall be nested into.
       *  \remarks  If possible, this function will flatten the Clang-specific if-cascades into multi-branch statements. */
      void                                  _BuildBranchingStatement(::clang::IfStmt *pIfStmt, AST::ScopePtr spEnclosingScope);

      /** \brief  Converts the conditional branch of a Clang if-statement object into a VAST conditional branch node.
       *  \param  pIfStmt               A pointer to the Clang if-statement object, whose conditional branch shall be converted.
       *  \param  spBranchingStatement  A shared pointer to the VAST branching statement node, the new conditional branch shall be added to.
       *  \return A pointer to the contents of the <b>else</b> branch of the input if-statement. */
      ::clang::Stmt*                        _BuildConditionalBranch(::clang::IfStmt *pIfStmt, AST::ControlFlow::BranchingStatementPtr spBranchingStatement);

      /** \brief  Converts a Clang literal expression object into a VAST constant node.
       *  \param  pExpression   A pointer to the Clang expression object, which shall be converted. It must have a <b>literal</b> type. */
      AST::Expressions::ConstantPtr         _BuildConstantExpression(::clang::Expr *pExpression);

      /** \brief  Converts a Clang cast expression object into a VAST conversion node.
       *  \param  pCastExpr   A pointer to the Clang cast expression object, which shall be converted. */
      AST::Expressions::ConversionPtr       _BuildConversionExpression(::clang::CastExpr *pCastExpr);

      /** \brief  Base method, which converts any kinds of Clang expression objects into their VAST counterparts.
       *  \param  pExpression   A pointer to the Clang expression object, which shall be converted. */
      AST::BaseClasses::ExpressionPtr       _BuildExpression(::clang::Expr *pExpression);

      /** \brief    Creates a VAST identifier node for a specific variable name.
       *  \param    strIdentifierName   The originally declared name of the requested variable.
       *  \remarks  This function performs a lookup of the original Clang variable name to the unique VAST variable name. */
      AST::Expressions::IdentifierPtr       _BuildIdentifier(std::string strIdentifierName);

      /** \brief  Converts any kinds of Clang loop statement objects into a VAST loop node.
       *  \param  pLoopStatement    A pointer to the Clang statement object, which shall be converted. It must have a <b>loop</b> type.
       *  \param  spEnclosingScope  A shared pointer to the VAST scope node, the new loop shall be nested into. */
      void                                  _BuildLoop(::clang::Stmt *pLoopStatement, AST::ScopePtr spEnclosingScope);

      /** \brief  Base method, which converts any kinds of Clang statement objects into their VAST counterparts.
       *  \param  pStatement        A pointer to the Clang statement object, which shall be converted.
       *  \param  spEnclosingScope  A shared pointer to the VAST scope node, the new statement shall be added to.
       *  \return A shared pointer to the newly created VAST node, if it has to be added to the enclosing scope, or <b>nullptr</b> if the converted
       *          statement has already been linked into the VAST. */
      AST::BaseClasses::NodePtr             _BuildStatement(::clang::Stmt *pStatement, AST::ScopePtr spEnclosingScope);

      /** \brief  Converts a Clang unary operator object (and its sub-expression) into a VAST unary operator node.
       *  \param  pSubExpr  A pointer to the Clang expression object, which describes the sub-expression of the unary operator.
       *  \param  eOpKind   The Clang-specific operator code of the unary operator. */
      AST::Expressions::UnaryOperatorPtr    _BuildUnaryOperatorExpression(::clang::Expr *pSubExpr, ::clang::UnaryOperatorKind eOpKind);

      /** \brief  Converts a Clang-specific value declaration object into a VAST variable info node.
       *  \param  pVarDecl              A pointer to the Clang value declaration object, which shall be converted.
       *  \param  spVariableContainer   A shared pointer to the VAST variable container node, whose context the variable shall be declared in. */
      AST::BaseClasses::VariableInfoPtr     _BuildVariableInfo(::clang::VarDecl *pVarDecl, AST::IVariableContainerPtr spVariableContainer);


      /** \brief  Converts the whole contents of a Clang compound statement and adds them to a VAST scope node.
       *  \param  spScope             A shared pointer to the VAST scope node, the converted contents shall be added to.
       *  \param  pCompoundStatement  A pointer to the Clang compound statement object, which shall be converted. */
      void _ConvertScope(AST::ScopePtr spScope, ::clang::CompoundStmt *pCompoundStatement);


    private:

      VariableNameTranslator  _VarTranslator;


    public:

      /** \brief  Converts a Clang function declaration object into a VAST function declaration node. 
       *  \param  pFunctionDeclaration  A pointer to the Clang function declaration object, whicc shall be converted. */
      AST::FunctionDeclarationPtr BuildFunctionDecl(::clang::FunctionDecl *pFunctionDeclaration);


      /** \brief    Returns the next free unique variable name for a new variable declaration.
       *  \param    spVariableContainer   A shared pointer to the VAST variable container, where the variable shall be declared in.
       *  \param    strRootName           The requested root name of the variable declaration. It will be used to create the unique name.
       *  \remarks  Unique names will be created by concatenating the root name with the suffix <b>_&lt;index&gt;</b>, unless the root name itself is still free. */
      static std::string          GetNextFreeVariableName(AST::IVariableContainerPtr spVariableContainer, std::string strRootName);

      /** \brief  Returns the prefix, which shall be used for all temporary variable names. */
      inline static std::string   GetTemporaryNamePrefix()   { return "_temp"; }
    };

    class VASTExportArray final : public VASTExporterBase
    {
    private:

      typedef VASTExporterBase    BaseType;


      class VectorIndex final
      {
      private:

        enum class VectorIndexType
        {
          Constant,
          Identifier
        };


        const VectorIndexType     _ceIndexType;
        const IndexType           _ciVectorIndex;
        const ::clang::ValueDecl  *_cpIndexExprDecl;


        VectorIndex(const VectorIndex &) = delete;
        VectorIndex& operator=(const VectorIndex &) = delete;

      public:

        inline VectorIndex(IndexType iVecIdx = static_cast<IndexType>(0)) : _ceIndexType(VectorIndexType::Constant), _ciVectorIndex(iVecIdx), _cpIndexExprDecl(nullptr)  {}

        inline VectorIndex(::clang::ValueDecl *pIndexDecl) : _ceIndexType(VectorIndexType::Identifier), _ciVectorIndex(0), _cpIndexExprDecl(pIndexDecl)  {}


        ::clang::Expr* CreateIndexExpression(ClangASTHelper &rASTHelper) const;
      };


    private:

      const IndexType       _VectorWidth;

      ::clang::ValueDecl    *_pVectorIndexExpr;



    private:

      ::clang::CompoundStmt*  _BuildCompoundStatement(AST::ScopePtr spScope);

      ::clang::Expr*          _BuildExpression(AST::BaseClasses::ExpressionPtr spExpression, const VectorIndex &crVectorIndex);

      ::clang::Stmt*          _BuildExpressionStatement(AST::BaseClasses::ExpressionPtr spExpression);

      ::clang::Expr*          _BuildFunctionCall(AST::Expressions::FunctionCallPtr spFunctionCall, const VectorIndex &crVectorIndex);

      ::clang::IfStmt*        _BuildIfStatement(AST::ControlFlow::BranchingStatementPtr spBranchingStatement);

      ::clang::Stmt*          _BuildLoop(AST::ControlFlow::LoopPtr spLoop);


      virtual ::clang::QualType _GetVectorizedType(AST::BaseClasses::TypeInfo &crOriginalTypeInfo) final override;


    public:

      VASTExportArray(IndexType VectorWidth, ::clang::ASTContext &rAstContext);


      ::clang::FunctionDecl* ExportVASTFunction(AST::FunctionDeclarationPtr spVASTFunction, bool bUnrollVectorLoops);

    };


    class Transformations final
    {
    public:

      enum class DirectionType
      {
        BottomUp,
        TopDown
      };

    private:

      template < class TransformationType >
      inline static void _ParseChildren(typename TransformationType::TargetTypePtr spCurrentNode, TransformationType &rTransformation)
      {
        typedef typename TransformationType::ChildTargetType   ChildTargetType;
        static_assert(std::is_base_of< AST::BaseClasses::Node, ChildTargetType >::value, "The child target type of the VAST transformation must be derived from class\"AST::BaseClasses::Node\"!");

        for (IndexType iChildIdx = static_cast<IndexType>(0); iChildIdx < spCurrentNode->GetChildCount(); ++iChildIdx)
        {
          AST::BaseClasses::NodePtr spChildNode = spCurrentNode->GetChild(iChildIdx);
          if (! spChildNode)
          {
            continue;
          }

          if (spChildNode->IsType<ChildTargetType>())
          {
            iChildIdx = rTransformation.ProcessChild(spCurrentNode, iChildIdx, spChildNode->CastToType<ChildTargetType>());
          }
        }
      }

      class TransformationBase
      {
      public:

        virtual ~TransformationBase()   {}

        virtual DirectionType GetSearchDirection() const    { return DirectionType::BottomUp; }
      };


    public:

      template < class TransformationType >
      inline static void Run(AST::BaseClasses::NodePtr spCurrentNode, TransformationType &rTransformation)
      {
        typedef typename TransformationType::TargetType   TargetType;

        static_assert(std::is_base_of< AST::BaseClasses::Node, TargetType >::value,     "The target type of the VAST transformation must be derived from class\"AST::BaseClasses::Node\"!");
        static_assert(std::is_base_of< TransformationBase, TransformationType >::value, "The transformation class must be derived from class\"TransformationBase\"!");

        // Skip unset children
        if (! spCurrentNode)
        {
          return;
        }

        // Check if the current node is of the target type
        const bool cbIsTarget = spCurrentNode->IsType<TargetType>();

        // Get the search direction
        DirectionType eSearchDirection  = rTransformation.GetSearchDirection();
        if ((eSearchDirection != DirectionType::BottomUp) && (eSearchDirection != DirectionType::TopDown))
        {
          throw InternalErrorException("Invalid VAST transformation search direction type!");
        }


        // Execute the transformation before parsing the children for "top-down" search
        if (cbIsTarget && (eSearchDirection == DirectionType::TopDown))
        {
          rTransformation.Execute( spCurrentNode->CastToType<TargetType>() );
        }

        // Parse all children of the current node
        for (IndexType iChildIdx = static_cast<IndexType>(0); iChildIdx < spCurrentNode->GetChildCount(); ++iChildIdx)
        {
          Run(spCurrentNode->GetChild(iChildIdx), rTransformation);
        }

        // Execute the transformation after parsing the children for "bottom-up" search
        if (cbIsTarget && (eSearchDirection == DirectionType::BottomUp))
        {
          rTransformation.Execute( spCurrentNode->CastToType<TargetType>() );
        }
      }

      template < class TransformationType >
      inline static void RunSimple(AST::BaseClasses::NodePtr spCurrentNode)
      {
        // Required by GCC
        TransformationType Transform;
        Run( spCurrentNode, Transform );
      }


    public:

      template < class NodeClass >
      class FindNodes final : public TransformationBase
      {
      private:

        static_assert( std::is_base_of< AST::BaseClasses::Node, NodeClass >::value, "The NodeClass type must be derived from class \"AST::BaseClasses::Node\" !" );

        const DirectionType _ceSearchDirection;

      public:

        typedef NodeClass                       TargetType;
        typedef std::shared_ptr< TargetType >   TargetTypePtr;

        std::list< TargetTypePtr >  lstFoundNodes;


        FindNodes(DirectionType eSearchDirection = DirectionType::BottomUp) : _ceSearchDirection(eSearchDirection)  {}

        virtual DirectionType GetSearchDirection() const    { return _ceSearchDirection; }


        inline void Execute(TargetTypePtr spFoundNode)    { lstFoundNodes.push_back(spFoundNode); }
      };


      class CheckInternalDeclaration final : public TransformationBase
      {
      public:

        typedef AST::Scope    TargetType;

      private:

        std::string _strDeclName;
        bool        _bFound;

      public:

        inline CheckInternalDeclaration(std::string strDeclName) : _strDeclName(strDeclName), _bFound(false)  {}

        void Execute(AST::ScopePtr spScope);

        inline bool Found() const   { return _bFound; }
      };

      class FindBranchingInternalAssignments final : public TransformationBase
      {
      public:

        typedef AST::ControlFlow::BranchingStatement      TargetType;

        std::map< AST::Expressions::AssignmentOperatorPtr, std::list< AST::BaseClasses::ExpressionPtr > >  mapConditionalAssignments;

        void Execute(AST::ControlFlow::BranchingStatementPtr spBranchingStmt);
      };

      class FindLoopInternalAssignments final : public TransformationBase
      {
      public:

        typedef AST::ControlFlow::Loop      TargetType;

        std::map< AST::Expressions::AssignmentOperatorPtr, std::list< AST::BaseClasses::ExpressionPtr > >  mapConditionalAssignments;

        void Execute(AST::ControlFlow::LoopPtr spLoop);
      };

      class FlattenMemoryAccesses final : public TransformationBase
      {
      public:

        typedef AST::Expressions::MemoryAccess  TargetType;

        void Execute(AST::Expressions::MemoryAccessPtr spMemoryAccess);
      };

      class FlattenScopes final : public TransformationBase
      {
      public:

        typedef AST::Scope      TargetType;
        typedef AST::ScopePtr   TargetTypePtr;
        typedef AST::Scope      ChildTargetType;

        inline void Execute(AST::ScopePtr spCurrentScope)   { _ParseChildren(spCurrentScope, *this); }

        IndexType ProcessChild(AST::ScopePtr spParentScope, IndexType iChildIndex, AST::ScopePtr spChildScope);
      };

      class InsertRequiredBroadcasts final : public TransformationBase
      {
      public:

        typedef AST::Expressions::BinaryOperator    TargetType;

        void Execute(AST::Expressions::BinaryOperatorPtr spCurrentBinOp);
      };

      class InsertRequiredConversions final : public TransformationBase
      {
      public:

        typedef AST::Expressions::BinaryOperator    TargetType;

        void Execute(AST::Expressions::BinaryOperatorPtr spCurrentBinOp);
      };

      class RemoveImplicitConversions final : public TransformationBase
      {
      public:

        typedef AST::BaseClasses::Expression      TargetType;
        typedef AST::BaseClasses::ExpressionPtr   TargetTypePtr;
        typedef AST::Expressions::Conversion      ChildTargetType;

        void Execute(AST::BaseClasses::ExpressionPtr spCurrentExpression)   { _ParseChildren(spCurrentExpression, *this); }

        IndexType ProcessChild(AST::BaseClasses::ExpressionPtr spParentExpression, IndexType iChildIndex, AST::Expressions::ConversionPtr spConversion);
      };

      class RemoveUnnecessaryConversions final : public TransformationBase
      {
      public:

        typedef AST::BaseClasses::Expression      TargetType;
        typedef AST::BaseClasses::ExpressionPtr   TargetTypePtr;
        typedef AST::Expressions::Conversion      ChildTargetType;

        void Execute(AST::BaseClasses::ExpressionPtr spCurrentExpression)   { _ParseChildren(spCurrentExpression, *this); }

        IndexType ProcessChild(AST::BaseClasses::ExpressionPtr spParentExpression, IndexType iChildIndex, AST::Expressions::ConversionPtr spConversion);
      };

      class SeparateBranchingStatements final : public TransformationBase
      {
      public:

        typedef AST::ControlFlow::BranchingStatement    TargetType;

        void Execute(AST::ControlFlow::BranchingStatementPtr spBranchingStmt);
      };


    public:

      typedef FindNodes< AST::Expressions::AssignmentOperator >   FindAssignments;
    };


    static AST::BaseClasses::VariableInfoPtr _GetAssigneeInfo(AST::Expressions::AssignmentOperatorPtr spAssignment);

    static void _CreateLocalMaskComputation(AST::ScopePtr spParentScope, AST::BaseClasses::ExpressionPtr spCondition, std::string strLocalMaskName, std::string strGlobalMaskName, bool bExclusiveBranches);

    static void _CreateVectorizedConditionalBranch(AST::ScopePtr spParentScope, AST::ScopePtr spBranchScope, std::string strMaskName);

    static void _FlattenSubExpression(const std::string &crstrTempVarNameRoot, AST::BaseClasses::ExpressionPtr spSubExpression);

  public:

    AST::FunctionDeclarationPtr ConvertClangFunctionDecl(::clang::FunctionDecl *pFunctionDeclaration);

    ::clang::FunctionDecl*      ConvertVASTFunctionDecl(AST::FunctionDeclarationPtr spVASTFunction, const size_t cszVectorWidth, ::clang::ASTContext &rASTContext, bool bUnrollVectorLoops);


    inline void FlattenMemoryAccesses(AST::BaseClasses::NodePtr spRootNode)         { Transformations::RunSimple< Transformations::FlattenMemoryAccesses        >( spRootNode ); }
    inline void FlattenScopeTrees(AST::BaseClasses::NodePtr spRootNode)             { Transformations::RunSimple< Transformations::FlattenScopes                >( spRootNode ); }
    inline void RemoveUnnecessaryConversions(AST::BaseClasses::NodePtr spRootNode)  { Transformations::RunSimple< Transformations::RemoveUnnecessaryConversions >( spRootNode ); }
    inline void SeparateBranchingStatements(AST::BaseClasses::NodePtr spRootNode)   { Transformations::RunSimple< Transformations::SeparateBranchingStatements  >( spRootNode ); }


    void RebuildControlFlow(AST::FunctionDeclarationPtr spFunction);

    void RebuildDataFlow(AST::FunctionDeclarationPtr spFunction, bool bEnsureMonoTypeVectorExpressions = false);

    void VectorizeFunction(AST::FunctionDeclarationPtr spFunction);

    static void DumpVASTNodeToXML(AST::BaseClasses::NodePtr spVastNode, std::string strXmlFilename);
  };
} // end namespace Vectorization
} // end namespace Backend
} // end namespace hipacc
} // end namespace clang


#endif  // _BACKEND_VECTORIZER_H_

// vim: set ts=2 sw=2 sts=2 et ai:

