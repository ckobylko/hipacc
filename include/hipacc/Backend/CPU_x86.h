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

//===--- CPU_x86.h - Implements the C++ code generator for x86-based CPUs. -----------===//
//
// This file implements the C++ code generator for CPUs which are based on the x86-microarchitecture.
//
//===---------------------------------------------------------------------------------===//

#ifndef _BACKEND_CPU_X86_H_
#define _BACKEND_CPU_X86_H_

#include "CodeGeneratorBaseImplT.h"
#include "hipacc/DSL/ClassRepresentation.h"
#include <list>
#include <utility>
#include <vector>

namespace clang
{
namespace hipacc
{
namespace Backend
{
  /** \brief  The backend for CPUs which are based on the x86-microarchitecture. */
  class CPU_x86 final
  {
  private:

    /** \brief  Contains the IDs of all supported specific compiler switches for this backend. */
    enum class CompilerSwitchTypeEnum
    {
      VectorizeKernel
    };


    /** \brief  Contains all known specific compiler switches for this backend. */
    class KnownSwitches final
    {
    public:

      /** \brief  The switch type for the "kernel function vectorization" switch. */
      struct VectorizeKernel final
      {
        /** \brief  Returns the command argument for this switch. */
        inline static std::string Key()                 { return "-v"; }

        /** \brief  Returns the additional options string for this switch. */
        inline static std::string AdditionalOptions()   { return ""; }

        /** \brief  Returns the description for this switch. */
        inline static std::string Description()         { return "Run a whole function vectorization on the kernel function"; }
      };
    };


    /** \brief  Helper class which contains a few methods for easy clang AST handling. */
    class ClangASTHelper final
    {
    public:

      typedef ::llvm::SmallVector< ::clang::Stmt*, 16U >  StatementVectorType;  //!< Type definition for a vector of statements.

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


    /** \brief  Helper class which encapsulates a HIPAcc kernel and simplifies the parameter handling. */
    class HipaccHelper final
    {
    public:

      /** \brief  Enumeration of internal parameters of HIPAcc images. */
      enum class ImageParamType
      {
        Buffer,   //!< Refers to the data buffer of an image.
        Width,    //!< Refers to the width of an image.
        Height,   //!< Refers to the height of an image.
        Stride    //!< Refers to the stride of an image (i.e. the offset between vertically adjacent pixels).
      };

    private:

      ::clang::FunctionDecl *_pKernelFunction;    //!< A pointer to the translated kernel function declaration.
      HipaccKernel          *_pKernel;            //!< A pointer to the encapsulated HIPAcc kernel object.


      /** \brief    Looks up the index of a kernel parameter in the kernel function argument list. 
       *  \param    crstrParamName  The name of the kernel parameter whose index shall be retrieved.
       *  \return   If successful, the index of the kernel parameter, and <b>-1</b> otherwise. */
      int                         _FindKernelParamIndex( const std::string &crstrParamName );

      /** \brief  Returns a reference to the current clang AST context. */
      inline ::clang::ASTContext& _GetASTContext()  { return _pKernelFunction->getASTContext(); }

    public:

      /** \brief  Constructor.
       *  \param  pKernelFunction   A pointer to the translated kernel function declaration object.
       *  \param  pKernel           A pointer to the HIPAcc kernel object which shall be encapsulated.*/
      HipaccHelper(FunctionDecl *pKernelFunction, HipaccKernel *pKernel) : _pKernelFunction(pKernelFunction), _pKernel(pKernel)  {}

      HipaccHelper(const HipaccHelper &) = delete;
      HipaccHelper& operator=(const HipaccHelper &) = delete;


      /** \brief  Returns the name of the horizontal global ID. */
      inline static std::string GlobalIdX()   { return "gid_x"; }
      /** \brief  Returns the name of the vertical global ID. */
      inline static std::string GlobalIdY()   { return "gid_y"; }


      /** \brief  Returns a pointer to the translated kernel function declaration. */
      inline ::clang::FunctionDecl* GetKernelFunction()  { return _pKernelFunction; }


      /** \brief    Looks up the image access type for a specific kernel function parameter.
       *  \param    crstrParamName  The name of the parameter to look up.
       *  \return   The image access type if the parameter refers to a HIPAcc image, and <b>UNDEFINED</b> otherwise. */
      MemoryAccess    GetImageAccess(const std::string &crstrParamName);

      /** \brief    Returns the HIPAcc image accessor object for a specific kernel function parameter.
       *  \param    crstrParamName  The name of the parameter to look up.
       *  \return   The image accessor if the parameter refers to a HIPAcc image, and <b>nullptr</b> otherwise. */
      HipaccAccessor* GetImageFromMapping(const std::string &crstrParamName);

      /** \brief    Returns the HIPAcc mask object for a specific kernel function parameter.
       *  \param    crstrParamName  The name of the parameter to look up.
       *  \return   The mask object if the parameter refers to a HIPAcc mask, and <b>nullptr</b> otherwise. */
      HipaccMask*     GetMaskFromMapping(const std::string &crstrParamName);


      /** \brief    Returns the specified internal parameter declaration of an HIPAcc image.
       *  \param    crstrImageName  The name of the image whose parameter shall be looked up.
       *  \param    eParamType      The type of the parameter declaration which shall be returned.
       *  \return   If successful, the declaration reference expression for the image parameter, and <b>nullptr</b> otherwise. */
      ::clang::DeclRefExpr* GetImageParameterDecl(const std::string &crstrImageName, ImageParamType eParamType);


      /** \brief  Creates an expression object which defines the upper bound of the horizontal iteration space (if an offset is specified, it will be included). */
      ::clang::Expr* GetIterationSpaceLimitX();
      /** \brief  Creates an expression object which defines the upper bound of the vertical iteration space (if an offset is specified, it will be included). */
      ::clang::Expr* GetIterationSpaceLimitY();


      /** \brief  Checks, if a kernel parameter is marked as being used by the kernel.
       *  \param  crstrParamName  The name of the parameter in question. */
      inline bool IsParamUsed(const std::string &crstrParamName)    { return _pKernel->getUsed(crstrParamName); }

      /** \brief  Marks a kernel parameter as being used by the kernel.
       *  \param  crstrParamName  The name of the parameter which shall be marked as used. */
      inline void MarkParamUsed(const std::string &crstrParamName)  { _pKernel->setUsed(crstrParamName); }
    };


  public:

    /** \brief    The code generator for x86-CPUs.
     *  \extends  CodeGeneratorBaseImplT */
    class CodeGenerator final : public CodeGeneratorBaseImplT< CompilerSwitchTypeEnum >
    {
    private:

      typedef CodeGeneratorBaseImplT< CompilerSwitchTypeEnum >  BaseType;                 //!< The type of the base class.
      typedef BaseType::CompilerSwitchInfoType                  CompilerSwitchInfoType;   //!< The type of the switch information class for this code generator.

      /** \brief    The specific descriptor class for this code generator.
       *  \extends  CodeGeneratorBaseImplT::CodeGeneratorDescriptorBase. */
      class Descriptor final : public BaseType::CodeGeneratorDescriptorBase
      {
      public:
        /** \brief  Initializes the fields of the base class. */
        Descriptor();
      };


      /** \brief  Helper class which extracts the inner loop body of a kernel function into an own sub-function. */
      class KernelSubFunctionBuilder final
      {
      public:

        typedef std::pair< ::clang::FunctionDecl*, ::clang::CallExpr* >   DeclCallPairType;   //!< Type definition for a sub-function declaration and call expression pair.


      private:

        ::clang::ASTContext                           &_rASTContext;      //!< A reference to the currently used ASTContext.
        ::llvm::SmallVector< ::clang::QualType, 16 >  _vecArgumentTypes;  //!< A vector containing the qualified types of the sub-function arguments.
        ::llvm::SmallVector< std::string, 16 >        _vecArgumentNames;  //!< A vector containing the names of the sub-function arguments.
        ::llvm::SmallVector< ::clang::Expr*, 16 >     _vecCallParams;     //!< A vector containing the declaration reference expression used for the sub-function call.


        KernelSubFunctionBuilder(const KernelSubFunctionBuilder &) = delete;
        KernelSubFunctionBuilder& operator=(const KernelSubFunctionBuilder &) = delete;


        /** \brief  Helper function, which checks whether a specific variable name is being used in a statement tree.
         *  \param  crstrVariableName   A constant reference to the name of variable, whose usage shall be checked.
         *  \param  pStatement          A pointer to the root of the statement tree, which shall be parsed. */
        static bool _IsVariableUsed(const std::string &crstrVariableName, ::clang::Stmt *pStatement);


      public:

        /** \brief  Standard constructor.
         *  \param  rASTContext   A reference to the  currently used ASTContext. */
        inline KernelSubFunctionBuilder(::clang::ASTContext &rASTContext) : _rASTContext(rASTContext) {}


        /** \brief  Adds a new parameter to the list of sub-function arguments.
         *  \param  pCallParam  A declaration reference expression for the new argument. */
        void AddCallParameter(::clang::DeclRefExpr *pCallParam);

        /** \brief  Imports all parameters from a function declaration, which are being used in a specific statement tree.
         *  \param  pRootFunctionDecl   A pointer to the declaration object for the function whose parameters shall be imported.
         *  \param  pSubFunctionBody    A pointer to the statement tree, which shall be parsed for the parameter references. */
        void ImportUsedParameters(::clang::FunctionDecl *pRootFunctionDecl, ::clang::Stmt *pSubFunctionBody);

        /** \brief  Creates a new sub-function declaration and call expression pair.
         *  \param  strFunctionName   The name of the new sub-function.
         *  \param  crResultType      The qualified result type of the new sub-function. */
        DeclCallPairType  CreateFuntionDeclarationAndCall(std::string strFunctionName, const ::clang::QualType &crResultType);
      };


      /** \brief  Helper class which translates declarations of and accesses to HIPAcc images. */
      class ImageAccessTranslator
      {
      public:

        typedef std::pair< ::clang::VarDecl*, ::clang::VarDecl* >   ImageLinePosDeclPairType;   //!< \brief   Type definition for the "current line" and "current pixel" pointer declaration pair.
                                                                                                //!< \details The first entry is the "line" declaration and the second one is the "pixel" declaration.

      private:

        HipaccHelper          &_rHipaccHelper;    //!< A reference to the HIPAcc helper object which encapsulates the kernel.
        ClangASTHelper        _ASTHelper;         //!< The AST helper object.
        ::clang::DeclRefExpr  *_pDRGidX;          //!< A pointer to horizontal global ID declaration reference of the kernel.
        ::clang::DeclRefExpr  *_pDRGidY;          //!< A pointer to vertical global ID declaration reference of the kernel.


        /** \brief  Returns a list of all array subscript expressions which describe an access to the specified HIPAcc image.
         *  \param  crstrImageName  The name of the image whose access shall be found.
         *  \param  pStatement      The root of the statement tree which shall be parsed for an image access. */
        static std::list< ::clang::ArraySubscriptExpr* > _FindImageAccesses(const std::string &crstrImageName, ::clang::Stmt *pStatement);

        /** \brief  Translates one 2-dimensional global access to a HIPAcc image a into local 1-dimensional image access.
         *  \param  crstrImageName    The name of the HIPAcc image which is being accessed.
         *  \param  pImageAccessRoot  A pointer to the root expression of the image access. */
        void _LinearizeImageAccess(const std::string &crstrImageName, ::clang::ArraySubscriptExpr *pImageAccessRoot);

        /** \brief    Subtracts a variable from an expression tree.
         *  \details  This function tries to simplify the resulting expression if possible. If the resulting expression would evaluate to
         *            <b>zero</b>, a <b>nullptr</b> is returned.
         *  \param    pExpression     A pointer to the root of the expression tree.
         *  \param    pDRSubtrahend   A pointer to the declaration reference of the variable which shall be subtracted from the expression tree.
         *  \return   The resulting modified expression tree or <b>nullptr</b>. */
        ::clang::Expr* _SubtractReference(::clang::Expr *pExpression, ::clang::DeclRefExpr *pDRSubtrahend);

        /** \brief  Tries to remove <b>exactly one</b> additive reference to a variable from a expression tree.
         *  \param  pExpression       A pointer to the root of the expression tree.
         *  \param  strStripVarName   The name of the variable which shall be removed from the expression tree.
         *  \return <b>True</b>, if the reference could be removed, and <b>false</b> otherwise. */
        bool _TryRemoveReference(::clang::Expr *pExpression, std::string strStripVarName);


      public:

        /** \brief  Constructor.
         *  \param  rHipaccHelper   A reference to the HIPAcc helper object which encapsulates the kernel. */
        ImageAccessTranslator(HipaccHelper &rHipaccHelper);


        /** \brief  Creates variable declarations for pointers to the current line and the current pixel of an image.
         *  \param  strImageName  The name of the image for which the "current line" and "current pixel" pointer shall be declared. */
        ImageLinePosDeclPairType CreateImageLineAndPosDecl(std::string strImageName);

        /** \brief  Translates the 2-dimensional global image accesses inside a kernel function into local 1-dimensional image accesses.
         *  \param  rHipaccHelper   A reference to the HIPAcc helper object which encapsulates the kernel. */
        void TranslateImageAccesses();
      };


      /** \brief  Create as clang statement for an iteration space "for"-loop.
       *  \param  rAstHelper    A reference to the current AST helper object.
       *  \param  pLoopCounter  A pointer to the declaration reference object for the loop counter variable (e.g. gid_x).
       *  \param  pUpperLimit   A pointer to the expression object which defines the upper bound of the iteration space (this value is exclusive).
       *  \param  pLoopBody     A pointer to the statement object which represents the loop body (if it is not a compound statement, it will be wrapped into one). */
      static ::clang::ForStmt* _CreateIterationSpaceLoop(ClangASTHelper &rAstHelper, ::clang::DeclRefExpr *pLoopCounter, ::clang::Expr *pUpperLimit, ::clang::Stmt *pLoopBody);

      /** \brief  Returns the declaration string of an image buffer parameter for the kernel function declarator.
       *  \param  strName               The name of the image buffer variable.
       *  \param  pHipaccMemoryObject   A pointer to the <b>HipaccMemory</b> object representing the image to be declared.
       *  \param  bConstPointer         Determines, whether the image buffer shall be treated as read-only.
       *  \param  bTranslate            Specifies, whether the image declaration shall be translated to a native pixel pointer. */
      static std::string _GetImageDeclarationString(std::string strName, HipaccMemory *pHipaccMemoryObject, bool bConstPointer = false, bool bTranslate = false);

      /** \brief    Formats a function declaration for a specific kernel into a string.
       *  \param    pKernelFunction       A pointer to the AST object declaring the kernel function.
       *  \param    rHipaccHelper         A reference to the HIPAcc helper object which encapsulates the kernel.
       *  \param    bCheckUsage           Specifies, whether the function parameters shall be checked for being used.
       *  \param    bTranslateImageDecls  Specifies, whether the declaration of the HIPAcc images shall be translated to native pixel pointers.
       *  \remarks  This function translates HIPAcc image declarations to the corresponding memory declarations. */
      std::string _FormatFunctionHeader(FunctionDecl *pFunctionDecl, HipaccHelper &rHipaccHelper, bool bCheckUsage = true, bool bTranslateImageDecls = false);


    private:

      bool    _bVectorizeKernel;    //!< Specifies, whether the kernel function shall be vectorized.

    protected:

      /** \name CodeGeneratorBaseImplT members */
      //@{

      virtual size_t _HandleSwitch(CompilerSwitchTypeEnum eSwitch, CommonDefines::ArgumentVectorType &rvecArguments, size_t szCurrentIndex) override;

      //@}

    public:

      /** \brief  Constructor.
       *  \param  pCompilerOptions  A pointer to the global compiler options object. */
      CodeGenerator(::clang::hipacc::CompilerOptions *pCompilerOptions);


      /** \name ICodeGenerator members */
      //@{

      virtual bool PrintKernelFunction(FunctionDecl *pKernelFunction, HipaccKernel *pKernel, llvm::raw_ostream &rOutputStream) final override;

      //@}
    };
  };
} // end namespace Backend
} // end namespace hipacc
} // end namespace clang


#endif  // _BACKEND_CPU_X86_H_

// vim: set ts=2 sw=2 sts=2 et ai:

