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

//===--- ClangASTHelper.h - Implements helper class for easy clang AST handling. -----===//
//
// This file implements a helper class which contains a few methods for easy clang AST handling.
//
//===---------------------------------------------------------------------------------===//

#ifndef _BACKEND_CLANG_AST_HELPER_H_
#define _BACKEND_CLANG_AST_HELPER_H_

#include "hipacc/AST/ASTNode.h"
#include <string>

namespace clang
{
namespace hipacc
{
namespace Backend
{
  /** \brief  Helper class which contains a few methods for easy clang AST handling. */
  class ClangASTHelper final
  {
  public:

    typedef ::llvm::SmallVector< ::clang::Expr*,     16U >  ExpressionVectorType;   //!< Type definition for a vector of expressions.
    typedef ::llvm::SmallVector< ::clang::QualType,  16U >  QualTypeVectorType;     //!< Type definition for a vector of qualified types.
    typedef ::llvm::SmallVector< ::clang::Stmt*,     16U >  StatementVectorType;    //!< Type definition for a vector of statements.
    typedef ::llvm::SmallVector< std::string,        16U >  StringVectorType;       //!< Type definition for a vector of strings.

  private:

    ::clang::ASTContext   &_rCtx;   //!< A reference to the current AST context.


    ClangASTHelper(const ClangASTHelper &) = delete;
    ClangASTHelper& operator=(const ClangASTHelper &) = delete;

  public:

    /** \brief  Constructor.
     *  \param  rAstContext   A reference to the current AST context. */
    ClangASTHelper(::clang::ASTContext &rAstContext) : _rCtx(rAstContext)   {}

    /** \brief  Returns a reference to the current AST context. */
    inline ::clang::ASTContext& GetASTContext()   { return _rCtx; }

    /** \brief  Returns the corresponding pointer type for a qualified clang type.
     *  \param  crPointeeType   A reference to the qualified type whose pointer type shall be returned. */
    inline ::clang::QualType    GetPointerType(const ::clang::QualType &crPointeeType)  { return GetASTContext().getPointerType(crPointeeType); }


    /** \name AST node creation methods */
    //@{

    /** \brief  Creates an subscript expression.
     *  \param  pArrayRef         A pointer to a declaration reference expression of the array.
     *  \param  pIndexExpression  A pointer to the expression object, which returns the index of the subscript.
     *  \param  crReturnType      The return type of the array subscript.
     *  \param  bIsLValue         Specifies, whether the array subscript expression is used as a L-value of another expression. */
    ::clang::ArraySubscriptExpr*  CreateArraySubscriptExpression(::clang::DeclRefExpr *pArrayRef, ::clang::Expr *pIndexExpression, const ::clang::QualType &crReturnType, bool bIsLValue = false);

    /** \brief  Creates a binary operator object of a specified type.
     *  \param  pLhs            A pointer to the expression object, which shall be on the left-hand-side.
     *  \param  pRhs            A pointer to the expression object, which shall be on the right-hand-side.
     *  \param  eOperatorKind   The type of the binary operator.
     *  \param  crReturnType    The return type of the operator expression. */
    ::clang::BinaryOperator*      CreateBinaryOperator(::clang::Expr *pLhs, ::clang::Expr *pRhs, ::clang::BinaryOperatorKind eOperatorKind, const ::clang::QualType &crReturnType);

    /** \brief  Creates a binary operator object which represents a "less than" comparison.
     *  \param  pLhs  A pointer to the expression object, which shall be on the left-hand-side.
     *  \param  pRhs  A pointer to the expression object, which shall be on the right-hand-side. */
    ::clang::BinaryOperator*      CreateBinaryOperatorLessThan(::clang::Expr *pLhs, ::clang::Expr *pRhs);

    /** \brief  Wraps a statement object into a compound statement object.
     *  \param  pStatement  A pointer to the statement object, which shall be encapsulated into an compound statement. */
    ::clang::CompoundStmt*        CreateCompoundStatement(::clang::Stmt *pStatement);

    /** \brief  Constructs a compound statement object around a vector of statement objects.
     *  \param  crvecStatements   A reference to the statement vector. */
    ::clang::CompoundStmt*        CreateCompoundStatement(const StatementVectorType &crvecStatements);

    /** \brief  Constructs a declaration reference expression which points to a specific declaration.
     *  \param  pValueDecl  A pointer to the value declaration object. */
    ::clang::DeclRefExpr*         CreateDeclarationReferenceExpression(::clang::ValueDecl *pValueDecl);

    /** \brief  Constructs a declaration statement for a specific declaration.
     *  \param  pDeclRef  A pointer to a declaration reference expression object which points to the specific declaration. */
    ::clang::DeclStmt*            CreateDeclarationStatement(::clang::DeclRefExpr *pDeclRef);

    /** \brief  Constructs a declaration statement for a specific declaration.
     *  \param  pValueDecl  A pointer to the value declaration object. */
    ::clang::DeclStmt*            CreateDeclarationStatement(::clang::ValueDecl *pValueDecl);

    /** \brief  Constructs a function call expression.
     *  \param  pFunctionDecl   A pointer to the function declaration which the constructed call shall point to.
     *  \param  crvecArguments  A vector containing the argument expressions for the function call. */
    ::clang::CallExpr*            CreateFunctionCall(::clang::FunctionDecl *pFunctionDecl, const ExpressionVectorType &crvecArguments);

    /** \brief  Constructs a function declaration statement.
     *  \param  strFunctionName     The desired name of the newly declared function.
     *  \param  crReturnType        The qualified return type of the function.
     *  \param  crvecArgumentNames  A vector containing the names of the function arguments.
     *  \param  crvecArgumentTypes  A vector containing the qualified types of the function arguments. */
    ::clang::FunctionDecl*        CreateFunctionDeclaration(std::string strFunctionName, const ::clang::QualType &crReturnType, const StringVectorType &crvecArgumentNames, const QualTypeVectorType &crvecArgumentTypes);

    /** \brief  Creates an implicit cast expression object.
     *  \param  pOperandExpression  A pointer to the expression object whose return type shall be implicitly casted.
     *  \param  crReturnType        The qualified return type of the cast.
     *  \param  eCastKind           The internal kind of the cast.
     *  \param  bIsLValue           Specifies, whether the implicit cast expression is used as a L-value of another expression. */
    ::clang::ImplicitCastExpr*    CreateImplicitCastExpression(::clang::Expr *pOperandExpression, const ::clang::QualType &crReturnType, ::clang::CastKind eCastKind, bool bIsLValue = false);

    /** \brief  Creates an integer literal expression (i.e. a compile time constant).
     *  \param  iValue  The value of the integer literal. */
    ::clang::IntegerLiteral*      CreateIntegerLiteral(int32_t iValue);

    /** \brief  Creates a parenthesis expression around another expression.
     *  \param  pSubExpression  A pointer to the expression object which shall be encapsulated into a parenthesis expression. */
    ::clang::ParenExpr*           CreateParenthesisExpression(::clang::Expr *pSubExpression);

    /** \brief  Constructs a post increment statement for a declaration reference expression object.
     *  \param  pDeclRef  A pointer to the declaration reference expression, which shall be used in the post increment operator. */
    ::clang::UnaryOperator*       CreatePostIncrementOperator(::clang::DeclRefExpr *pDeclRef);

    /** \brief    Creates a new variable declaration object.
     *  \param    pParentFunction     A pointer to the function declaration object in whose context the new variable shall be declared.
     *  \param    crstrVariableName   The name of the newly declared variable.
     *  \param    crVariableType      The qualified type of newly declared variable.
     *  \param    pInitExpression     A pointer to the initialization expression object for the variable declaration (i.e. the R-value of the assignment).
     *  \remarks  The created variable declaration is automatically added to the declaration context of the specified function declaration. */
    ::clang::VarDecl*             CreateVariableDeclaration(::clang::FunctionDecl *pParentFunction, const std::string &crstrVariableName, const ::clang::QualType &crVariableType, ::clang::Expr *pInitExpression);

    //@}

  public:


    /** \brief  Counts the number of declaration references to a specific declaration inside a statement tree.
     *  \param  pStatement          A pointer to the root of the statement tree which shall be parsed for the specified declaration references.
     *  \param  crstrReferenceName  The name of the declaration reference whose appearances shall be counted. */
    static unsigned int     CountNumberOfReferences(::clang::Stmt *pStatement, const std::string &crstrReferenceName);

    /** \brief    Looks up a specific declaration.
     *  \param    pFunction       A pointer to the function declaration object whose declaration context will be searched for the specified declaration.
     *  \param    crstrDeclName   The name of the declaration which shall be searched for.
     *  \return   If successful, a pointer to a newly created declaration reference expression for the found declaration, and zero otherwise. */
    ::clang::DeclRefExpr*   FindDeclaration(::clang::FunctionDecl *pFunction, const std::string &crstrDeclName);

    /** \brief  Checks whether a statement tree has only one branch (i.e. none of its nodes has more than one child).
     *  \param  pStatement  A pointer to the root of the statement tree. */
    static bool             IsSingleBranchStatement(::clang::Stmt *pStatement);

    /** \brief  Replaces <b>all</b> instances of a declaration reference in a statement tree by a new value declaration.
     *  \param  pStatement        A pointer to the root of the statement tree which shall be parsed for the specified declaration references.
     *  \param  crstrDeclRefName  The name of the declaration reference which shall be replaced.
     *  \param  pNewDecl          A pointer to the value declaration to which all reference will be updated. */
    static void             ReplaceDeclarationReferences(::clang::Stmt* pStatement, const std::string &crstrDeclRefName, ::clang::ValueDecl *pNewDecl);
  };
} // end namespace Backend
} // end namespace hipacc
} // end namespace clang


#endif  // _BACKEND_CLANG_AST_HELPER_H_

// vim: set ts=2 sw=2 sts=2 et ai:

