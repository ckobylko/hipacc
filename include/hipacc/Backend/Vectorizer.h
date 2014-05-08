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

#include <clang/AST/StmtVisitor.h>
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
namespace Vectorization
{
  class Vectorizer final
  {
  private:

    typedef AST::IndexType IndexType;


    class VASTBuilder : public ::clang::StmtVisitor< VASTBuilder >
    {
    private:

      class VariableNameTranslator final
      {
      private:

        typedef std::map< std::string, std::string >  RenameMapType;

        std::list< RenameMapType >    _lstRenameStack;

      public:

        inline void AddLayer()  { _lstRenameStack.push_front(RenameMapType()); }
        inline void PopLayer()  { _lstRenameStack.pop_front(); }

        void          AddRenameEntry( std::string strOriginalName, std::string strNewName );
        std::string   TranslateName( std::string strOriginalName ) const;
      };


    private:

      template <typename ValueType>
      inline static AST::Expressions::ConstantPtr _CreateConstant(ValueType TValue)
      {
        AST::Expressions::ConstantPtr spConstant = AST::CreateNode< AST::Expressions::Constant >();

        spConstant->SetValue( TValue );

        return spConstant;
      }

      inline static AST::BaseClasses::TypeInfo _ConvertTypeInfo(::clang::QualType qtSourceType)
      {
        AST::BaseClasses::TypeInfo ReturnType;

        _ConvertTypeInfo(ReturnType, qtSourceType);

        return ReturnType;
      }

      static void _ConvertTypeInfo(AST::BaseClasses::TypeInfo &rTypeInfo, ::clang::QualType qtSourceType);



      AST::Expressions::BinaryOperatorPtr   _BuildBinaryOperatorExpression(::clang::Expr *pExprLHS, ::clang::Expr *pExprRHS, ::clang::BinaryOperatorKind eOpKind);

      void                                  _BuildBranchingStatement(::clang::IfStmt *pIfStmt, AST::ScopePtr spEnclosingScope);

      ::clang::Stmt*                        _BuildConditionalBranch(::clang::IfStmt *pIfStmt, AST::ControlFlow::BranchingStatementPtr spBranchingStatement);

      AST::Expressions::ConstantPtr         _BuildConstantExpression(::clang::Expr *pExpression);

      AST::Expressions::ConversionPtr       _BuildConversionExpression(::clang::CastExpr *pCastExpr);

      AST::BaseClasses::ExpressionPtr       _BuildExpression(::clang::Expr *pExpression);

      AST::Expressions::IdentifierPtr       _BuildIdentifier(std::string strIdentifierName);

      void                                  _BuildLoop(::clang::Stmt *pLoopStatement, AST::ScopePtr spEnclosingScope);

      AST::BaseClasses::NodePtr             _BuildStatement(::clang::Stmt *pStatement, AST::ScopePtr spEnclosingScope);

      AST::Expressions::UnaryOperatorPtr    _BuildUnaryOperatorExpression(::clang::Expr *pSubExpr, ::clang::UnaryOperatorKind eOpKind);

      AST::BaseClasses::VariableInfoPtr     _BuildVariableInfo(::clang::VarDecl *pVarDecl, AST::IVariableContainerPtr spVariableContainer);

      void _ConvertScope(AST::ScopePtr spScope, ::clang::CompoundStmt *pCompoundStatement);


    private:

      VariableNameTranslator  _VarTranslator;


    public:

      AST::FunctionDeclarationPtr BuildFunctionDecl(::clang::FunctionDecl *pFunctionDeclaration);


      static AST::Expressions::AssignmentOperatorPtr  CreateAssignmentOperator(AST::BaseClasses::ExpressionPtr spLHS, AST::BaseClasses::ExpressionPtr spRHS);

      static AST::Expressions::IdentifierPtr          CreateIdentifier(std::string strIdentifierName);


      static std::string          GetNextFreeVariableName(AST::IVariableContainerPtr spVariableContainer, std::string strRootName);

      inline static std::string   GetTemporaryNamePrefix()   { return "_temp"; }



    // Debug stuff
    private:

      unsigned int _uiIntend = 2;

    public:

      void Import(::clang::FunctionDecl *pFunctionDeclaration)
      {
        printf("\n\nImport function decl:\n");
        Visit(pFunctionDeclaration->getBody());

        printf("\n\nImport finished!");
      }

      void VisitExpr(::clang::Expr *E)
      {
        printf("  %s\n", E->getStmtClassName() );
      }

      void VisitStmt(::clang::Stmt *S)
      {
        if (S == nullptr)
          return;

        for (unsigned int i = 0; i < _uiIntend; ++i)
        {
          printf(" ");
        }

        printf("%s\n", S->getStmtClassName());

        _uiIntend += 2;

        for (::clang::Stmt::child_iterator itChild = S->child_begin(); itChild != S->child_end(); itChild++)
        {
           VisitStmt(*itChild);
        }

        _uiIntend -= 2;
      }

//      void VisitCompoundStmt(::clang::CompoundStmt *S)
//      {
//        printf("  %s\n", S->getStmtClassName());
//      }

      void VisitBinaryOperator(BinaryOperator *E)
      {
        printf("  %s\n", E->getStmtClassName());
      }
    };


    class VASTExportArray final
    {
    private:

      typedef ClangASTHelper::FunctionDeclarationVectorType           FunctionDeclVectorType;
      typedef std::map< unsigned int, FunctionDeclVectorType >        FunctionDeclParamCountMapType;
      typedef std::map< std::string, FunctionDeclParamCountMapType >  FunctionDeclNameMapType;


      ClangASTHelper        _ASTHelper;

      const IndexType       _VectorWidth;
      ::clang::DeclContext  *_pDeclContext;

      std::map< std::string, ::clang::ValueDecl* >  _mapKnownDeclarations;
      FunctionDeclNameMapType                       _mapKnownFunctions;


    private:

      ::clang::CompoundStmt*  _BuildCompoundStatement(AST::ScopePtr spScope);

      ::clang::Expr*          _BuildConstant(AST::Expressions::ConstantPtr spConstant);

      ::clang::Expr*          _BuildExpression(AST::BaseClasses::ExpressionPtr spExpression, IndexType iVectorIndex);

      ::clang::Stmt*          _BuildExpressionStatement(AST::BaseClasses::ExpressionPtr spExpression);

      ::clang::Expr*          _BuildFunctionCall(AST::Expressions::FunctionCallPtr spFunctionCall, IndexType iVectorIndex);

      ::clang::IfStmt*        _BuildIfStatement(AST::ControlFlow::BranchingStatementPtr spBranchingStatement);

      ::clang::Stmt*          _BuildLoop(AST::ControlFlow::LoopPtr spLoop);

      ::clang::QualType _ConvertTypeInfo(const AST::BaseClasses::TypeInfo &crTypeInfo);

      ::clang::ValueDecl* _CreateValueDeclaration(AST::Expressions::IdentifierPtr spIdentifier, ::clang::Expr *pInitExpression = nullptr);

      AST::BaseClasses::TypeInfo _GetVectorizedType(AST::BaseClasses::TypeInfo &crOriginalTypeInfo);

      bool _HasValueDeclaration(std::string strDeclName);


      FunctionDeclVectorType _GetMatchingFunctionDeclarations( std::string strFunctionName, unsigned int uiParamCount );


    public:

      VASTExportArray(IndexType VectorWidth, ::clang::ASTContext &rAstContext);


      ::clang::FunctionDecl* ExportVASTFunction(AST::FunctionDeclarationPtr spVASTFunction);

    };



    class Transformations final
    {
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


    public:

      class CheckInternalDeclaration final
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

      class FindAssignments final
      {
      public:

        typedef AST::Expressions::AssignmentOperator      TargetType;

        std::list< AST::Expressions::AssignmentOperatorPtr >  lstAssignments;

        inline void Execute(AST::Expressions::AssignmentOperatorPtr spAssignment)   { lstAssignments.push_back(spAssignment); }
      };

      class FindBranchingInternalAssignments final
      {
      public:

        typedef AST::ControlFlow::BranchingStatement      TargetType;

        std::map< AST::Expressions::AssignmentOperatorPtr, std::list< AST::BaseClasses::ExpressionPtr > >  mapConditionalAssignments;

        void Execute(AST::ControlFlow::BranchingStatementPtr spBranchingStmt);
      };

      class FindLoopInternalAssignments final
      {
      public:

        typedef AST::ControlFlow::Loop      TargetType;

        std::map< AST::Expressions::AssignmentOperatorPtr, std::list< AST::BaseClasses::ExpressionPtr > >  mapConditionalAssignments;

        void Execute(AST::ControlFlow::LoopPtr spLoop);
      };

      class FlattenMemoryAccesses final
      {
      public:

        typedef AST::Expressions::MemoryAccess  TargetType;

        void Execute(AST::Expressions::MemoryAccessPtr spMemoryAccess);
      };

      class FlattenScopes final
      {
      public:

        typedef AST::Scope      TargetType;
        typedef AST::ScopePtr   TargetTypePtr;
        typedef AST::Scope      ChildTargetType;

        inline void Execute(AST::ScopePtr spCurrentScope)   { _ParseChildren(spCurrentScope, *this); }

        IndexType ProcessChild(AST::ScopePtr spParentScope, IndexType iChildIndex, AST::ScopePtr spChildScope);
      };

      class RemoveUnnecessaryConversions final
      {
      public:

        typedef AST::BaseClasses::Expression      TargetType;
        typedef AST::BaseClasses::ExpressionPtr   TargetTypePtr;
        typedef AST::Expressions::Conversion      ChildTargetType;

        void Execute(AST::BaseClasses::ExpressionPtr spCurrentExpression)   { _ParseChildren(spCurrentExpression, *this); }

        IndexType ProcessChild(AST::BaseClasses::ExpressionPtr spParentExpression, IndexType iChildIndex, AST::Expressions::ConversionPtr spConversion);
      };

      class SeparateBranchingStatements final
      {
      public:

        typedef AST::ControlFlow::BranchingStatement    TargetType;

        void Execute(AST::ControlFlow::BranchingStatementPtr spBranchingStmt);
      };
    };


    template < class TransformationType >
    inline static void _RunVASTTransformation(AST::BaseClasses::NodePtr spCurrentNode, TransformationType &rTransformation)
    {
      typedef typename TransformationType::TargetType   TargetType;
      static_assert( std::is_base_of< AST::BaseClasses::Node, TargetType >::value, "The target type of the VAST transformation must be derived from class\"AST::BaseClasses::Node\"!" );

      // Skip unset children
      if (! spCurrentNode)
      {
        return;
      }

      // Run a depth-first search on the whole VAST tree
      for (IndexType iChildIdx = static_cast<IndexType>(0); iChildIdx < spCurrentNode->GetChildCount(); ++iChildIdx)
      {
        _RunVASTTransformation(spCurrentNode->GetChild(iChildIdx), rTransformation);
      }

      // Execute the transformation if the current node is of the target type
      if (spCurrentNode->IsType<TargetType>())
      {
        rTransformation.Execute(spCurrentNode->CastToType<TargetType>());
      }
    }


    static AST::BaseClasses::VariableInfoPtr _GetAssigneeInfo(AST::Expressions::AssignmentOperatorPtr spAssignment);



  public:

    void Import(::clang::FunctionDecl *pFunctionDeclaration)
    {
      VASTBuilder b;
      b.Import(pFunctionDeclaration);
    }


    AST::FunctionDeclarationPtr ConvertClangFunctionDecl(::clang::FunctionDecl *pFunctionDeclaration);

    ::clang::FunctionDecl*      ConvertVASTFunctionDecl(AST::FunctionDeclarationPtr spVASTFunction, const size_t cszVectorWidth, ::clang::ASTContext &rASTContext);


    inline void FlattenMemoryAccesses(AST::BaseClasses::NodePtr spRootNode)         { _RunVASTTransformation(spRootNode, Transformations::FlattenMemoryAccesses()); }
    inline void FlattenScopeTrees(AST::BaseClasses::NodePtr spRootNode)             { _RunVASTTransformation(spRootNode, Transformations::FlattenScopes()); }
    inline void RemoveUnnecessaryConversions(AST::BaseClasses::NodePtr spRootNode)  { _RunVASTTransformation(spRootNode, Transformations::RemoveUnnecessaryConversions()); }
    inline void SeparateBranchingStatements(AST::BaseClasses::NodePtr spRootNode)   { _RunVASTTransformation(spRootNode, Transformations::SeparateBranchingStatements()); }

    void VectorizeFunction(AST::FunctionDeclarationPtr spFunction);

    static void DumpVASTNodeToXML(AST::BaseClasses::NodePtr spVastNode, std::string strXmlFilename);
  };
} // end namespace Vectorization
} // end namespace Backend
} // end namespace hipacc
} // end namespace clang


#endif  // _BACKEND_VECTORIZER_H_

// vim: set ts=2 sw=2 sts=2 et ai:

