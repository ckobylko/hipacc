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
#include <type_traits>
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

    typedef AST::BaseClasses::Node::IndexType IndexType;


    class VASTBuilder : public ::clang::StmtVisitor< VASTBuilder >
    {
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


      static AST::Expressions::BinaryOperatorPtr  _BuildBinaryOperatorExpression(::clang::Expr *pExprLHS, ::clang::Expr *pExprRHS, ::clang::BinaryOperatorKind eOpKind);

      static AST::Expressions::ConstantPtr        _BuildConstantExpression(::clang::Expr *pExpression);

      static AST::Expressions::ConversionPtr      _BuildConversionExpression(::clang::CastExpr *pCastExpr);

      static AST::BaseClasses::ExpressionPtr      _BuildExpression(::clang::Expr *pExpression);

      static void                                 _BuildLoop(::clang::Stmt *pLoopStatement, AST::ScopePtr spEnclosingScope);

      static AST::BaseClasses::NodePtr            _BuildStatement(::clang::Stmt *pStatement, AST::ScopePtr spEnclosingScope);

      static AST::Expressions::UnaryOperatorPtr   _BuildUnaryOperatorExpression(::clang::Expr *pSubExpr, ::clang::UnaryOperatorKind eOpKind);

      static AST::BaseClasses::VariableInfoPtr    _BuildVariableInfo(::clang::VarDecl *pVarDecl);

      static void _ConvertScope(AST::ScopePtr spScope, ::clang::CompoundStmt *pCompoundStatement);

      static void _ConvertTypeInfo(AST::BaseClasses::TypeInfo &rTypeInfo, ::clang::QualType qtSourceType);

    public:

      static AST::FunctionDeclarationPtr BuildFunctionDecl(::clang::FunctionDecl *pFunctionDeclaration);



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

      class FindAssignments final
      {
      public:

        typedef AST::Expressions::AssignmentOperator      TargetType;

        std::list< AST::Expressions::AssignmentOperatorPtr >  lstAssignments;

        inline void Execute(AST::Expressions::AssignmentOperatorPtr spAssignment)   { lstAssignments.push_back(spAssignment); }
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
    };


    template < class TransformationType >
    inline void _RunVASTTransformation(AST::BaseClasses::NodePtr spCurrentNode, TransformationType &rTransformation)
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


  public:

    void Import(::clang::FunctionDecl *pFunctionDeclaration)
    {
      VASTBuilder b;
      b.Import(pFunctionDeclaration);
    }


    AST::FunctionDeclarationPtr ConvertClangFunctionDecl(::clang::FunctionDecl *pFunctionDeclaration);


    inline void FlattenScopeTrees(AST::BaseClasses::NodePtr spRootNode)             { _RunVASTTransformation(spRootNode, Transformations::FlattenScopes()); }
    inline void RemoveUnnecessaryConversions(AST::BaseClasses::NodePtr spRootNode)  { _RunVASTTransformation(spRootNode, Transformations::RemoveUnnecessaryConversions()); }

    void VectorizeFunction(AST::FunctionDeclarationPtr spFunction);

    static void DumpVASTNodeToXML(AST::BaseClasses::NodePtr spVastNode, std::string strXmlFilename);
  };
} // end namespace Vectorization
} // end namespace Backend
} // end namespace hipacc
} // end namespace clang


#endif  // _BACKEND_VECTORIZER_H_

// vim: set ts=2 sw=2 sts=2 et ai:

