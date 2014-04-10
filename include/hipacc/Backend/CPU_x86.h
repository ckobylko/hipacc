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
#include <utility>

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


      /** \brief  Returns the declaration string of an image buffer parameter for the kernel function declarator.
       *  \param  strName               The name of the image buffer variable.
       *  \param  pHipaccMemoryObject   A pointer to the <b>HipaccMemory</b> object representing the image to be declared.
       *  \param  bConstPointer         Determines, whether the image buffer shall be treated as read-only.*/
      static std::string _GetImageDeclarationString(std::string strName, HipaccMemory *pHipaccMemoryObject, bool bConstPointer = false);

      /** \brief    Formats a function declaration for a specific kernel into a string.
       *  \param    pKernelFunction   A pointer to the AST object declaring the kernel function.
       *  \param    pKernel           A pointer to the <b>HipaccKernel</b> object containing semantical meta-information about the kernel.
       *  \param    bCheckUsage       Specifies, whether the function parameters shall be checked for being used.
       *  \remarks  This function translates HIPAcc image declarations to the corresponding memory declarations. */
      std::string _FormatFunctionHeader(FunctionDecl *pFunctionDecl, HipaccKernel *pKernel, bool bCheckUsage = true);

      /** \brief    Wraps a clang statement into a compound statement.
       *  \param    rContext    A reference to the currently used ASTContext.
       *  \param    pStatement  A pointer to the clang statement, which shall be encapsulated into an compound statement
       *  \return   The newly created compound statement, which contains the input statement as its only child. */
      static ::clang::CompoundStmt* _WrapInCompoundStatement(::clang::ASTContext &rContext, ::clang::Stmt *pStatement);


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

