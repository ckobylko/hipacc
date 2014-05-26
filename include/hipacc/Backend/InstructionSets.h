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

//===--- InstructionSets.h - Definition of known vector instruction sets. ------------===//
//
// This file contains definitions of known vector instruction sets.
//
//===---------------------------------------------------------------------------------===//

#ifndef _BACKEND_INSTRUCTION_SETS_H_
#define _BACKEND_INSTRUCTION_SETS_H_

#include "BackendExceptions.h"
#include "ClangASTHelper.h"
#include "VectorizationAST.h"
#include <map>
#include <memory>
#include <string>
#include <type_traits>
#include <utility>

//#define VERBOSE_INIT_MODE 1   // Uncomment this for a print-out of the inited intrinsic functions

#ifdef VERBOSE_INIT_MODE
#include "llvm/Support/raw_ostream.h"
#endif


namespace clang
{
namespace hipacc
{
namespace Backend
{
namespace Vectorization
{
  typedef AST::BaseClasses::TypeInfo::KnownTypes                          VectorElementTypes;
  typedef AST::Expressions::UnaryOperator::UnaryOperatorType              UnaryOperatorType;
  typedef AST::Expressions::ArithmeticOperator::ArithmeticOperatorType    ArithmeticOperatorType;
  typedef AST::Expressions::RelationalOperator::RelationalOperatorType    RelationalOperatorType;
  typedef AST::VectorSupport::CheckActiveElements::CheckType              ActiveElementsCheckType;


  class InstructionSetExceptions final
  {
  public:

    class IndexOutOfRange : public RuntimeErrorException
    {
    private:

      typedef RuntimeErrorException   BaseType;

      static std::string _ConvertLimit(std::uint32_t uiUpperLimit);

    protected:

      IndexOutOfRange(std::string strMethodType, VectorElementTypes eElementType, std::uint32_t uiUpperLimit);
    };

    class ExtractIndexOutOfRange final : public IndexOutOfRange
    {
    private:

      typedef IndexOutOfRange   BaseType;

    public:

      inline ExtractIndexOutOfRange(VectorElementTypes eElementType, std::uint32_t uiUpperLimit)  : BaseType("extraction", eElementType, uiUpperLimit)   {}
    };

    class InsertIndexOutOfRange final : public IndexOutOfRange
    {
    private:

      typedef IndexOutOfRange   BaseType;

    public:

      inline InsertIndexOutOfRange(VectorElementTypes eElementType, std::uint32_t uiUpperLimit)   : BaseType("insertion", eElementType, uiUpperLimit)  {}
    };
  };


  class InstructionSetBase
  {
  protected:

    typedef std::pair< std::string, ::clang::FunctionDecl* >      IntrinsicInfoPairType;

    template < typename IntrinsicIDType >   using IntrinsicMapTemplateType = std::map< IntrinsicIDType, IntrinsicInfoPairType >;

  private:

    typedef std::map< std::string, ClangASTHelper::FunctionDeclarationVectorType >   FunctionDeclMapType;

    ClangASTHelper        _ASTHelper;
    FunctionDeclMapType   _mapKnownFuncDecls;

    std::string           _strIntrinsicPrefix;

  protected:

    inline ClangASTHelper& _GetASTHelper()   { return _ASTHelper; }


    ::clang::CastExpr* _CreatePointerCast(::clang::Expr *pPointerRef, const ::clang::QualType &crNewPointerType);

    ::clang::CastExpr* _CreateValueCast(::clang::Expr *pValueRef, const ::clang::QualType &crNewValueType, ::clang::CastKind eCastKind);

    ::clang::QualType _GetClangType(VectorElementTypes eType);

    ClangASTHelper::FunctionDeclarationVectorType _GetFunctionDecl(std::string strFunctionName);


    template < typename IntrinsicIDType >
    inline ::clang::CallExpr* _CreateFunctionCall(const IntrinsicMapTemplateType< IntrinsicIDType > &crIntrinMap, IntrinsicIDType eIntrinID, const ClangASTHelper::ExpressionVectorType &crvecArguments)
    {
      auto itIntrinEntry = crIntrinMap.find(eIntrinID);
      if (itIntrinEntry == crIntrinMap.end())
      {
        throw InternalErrorException("The specified intrinsic is unknown!");
      }

      ::clang::FunctionDecl *pIntrinsicDecl = itIntrinEntry->second.second;
      if (pIntrinsicDecl == nullptr)
      {
        throw InternalErrorException(std::string("The intrinsic \"") + _strIntrinsicPrefix + itIntrinEntry->second.first + std::string("\" has not been initialized!"));
      }

      return _ASTHelper.CreateFunctionCall( pIntrinsicDecl, crvecArguments );
    }

    template < typename IntrinsicIDType >
    inline ::clang::Expr* _CreatePrefixedUnaryOp(const IntrinsicMapTemplateType< IntrinsicIDType > &crIntrinMap, IntrinsicIDType eIntrinID, VectorElementTypes eElementType, ::clang::Expr *pVectorRef)
    {
      ClangASTHelper::ExpressionVectorType vecArgs;
      vecArgs.push_back( pVectorRef );
      vecArgs.push_back( CreateOnesVector(eElementType, false) );

      ::clang::Expr *pPrefixOp = _CreateFunctionCall(crIntrinMap, eIntrinID, vecArgs);

      return _GetASTHelper().CreateParenthesisExpression( _GetASTHelper().CreateBinaryOperator( pVectorRef, pPrefixOp, BO_Assign, pVectorRef->getType() ) );
    }

    template < typename IntrinsicIDType >
    inline ::clang::QualType _GetFunctionReturnType(const IntrinsicMapTemplateType< IntrinsicIDType > &crIntrinMap, IntrinsicIDType eIntrinID)
    {
      auto itIntrinEntry = crIntrinMap.find(eIntrinID);
      if (itIntrinEntry == crIntrinMap.end())
      {
        throw InternalErrorException("The specified intrinsic is unknown!");
      }

      ::clang::FunctionDecl *pIntrinsicDecl = itIntrinEntry->second.second;
      if (pIntrinsicDecl == nullptr)
      {
        throw InternalErrorException(std::string("The intrinsic \"") + itIntrinEntry->second.first + std::string("\" has not been initialized!"));
      }

      return pIntrinsicDecl->getResultType();
    }

    template < typename IntrinsicIDType >
    inline void _InitIntrinsic(IntrinsicMapTemplateType< IntrinsicIDType > &rIntrinMap, IntrinsicIDType eIntrinID, std::string strIntrinName)
    {
      rIntrinMap[eIntrinID] = IntrinsicInfoPairType(_strIntrinsicPrefix + strIntrinName, nullptr);
    }

    template < typename IntrinsicIDType >
    inline void _LookupIntrinsics(IntrinsicMapTemplateType< IntrinsicIDType > &rIntrinMap, std::string strInstructionSetName)
    {
      #ifdef VERBOSE_INIT_MODE
      llvm::errs() << "\n\nIntrinsic functions for instruction set \"" << strInstructionSetName << "\" (" << rIntrinMap.size() << " methods):\n";
      #endif

      for (typename IntrinsicMapTemplateType< IntrinsicIDType >::iterator itIntrinsic = rIntrinMap.begin(); itIntrinsic != rIntrinMap.end(); itIntrinsic++)
      {
        IntrinsicInfoPairType &rIntrinsicInfo = itIntrinsic->second;

        auto vecFunctions = _GetFunctionDecl(rIntrinsicInfo.first);

        if (vecFunctions.size() != static_cast<size_t>(1))
        {
          throw InternalErrorException(string("Found ambiguous entry for intrinsic function \"") + rIntrinsicInfo.first + string("\" !"));
        }

        rIntrinsicInfo.second = vecFunctions.front();

        #ifdef VERBOSE_INIT_MODE
        llvm::errs() << "\n" << rIntrinsicInfo.second->getResultType().getAsString() << " " << rIntrinsicInfo.second->getNameAsString() << "(";
        for (unsigned int uiParam = 0; uiParam < rIntrinsicInfo.second->getNumParams(); ++uiParam)
        {
          ParmVarDecl *pParam = rIntrinsicInfo.second->getParamDecl(uiParam);
          llvm::errs() << pParam->getType().getAsString() << " " << pParam->getNameAsString();
          if ((uiParam + 1) < rIntrinsicInfo.second->getNumParams())
          {
            llvm::errs() << ", ";
          }
        }
        llvm::errs() << ")";
        #endif
      }

      #ifdef VERBOSE_INIT_MODE
      llvm::errs() << "\n\n";
      #endif
    }


  private:
    /** \name Clang bugfixing helper methods */
    //@{

    void _CreateIntrinsicDeclaration( std::string strFunctionName, const ::clang::QualType &crReturnType, const ClangASTHelper::QualTypeVectorType &crvecArgTypes,
                                      const ClangASTHelper::StringVectorType &crvecArgNames );

    void _CreateIntrinsicDeclaration( std::string strFunctionName, const ::clang::QualType &crReturnType, const ::clang::QualType &crArgType1, std::string strArgName1,
                                      const ::clang::QualType &crArgType2, std::string strArgName2 );

    void _CreateIntrinsicDeclaration( std::string strFunctionName, const ::clang::QualType &crReturnType, const ::clang::QualType &crArgType1, std::string strArgName1,
                                      const ::clang::QualType &crArgType2, std::string strArgName2, const ::clang::QualType &crArgType3, std::string strArgName3 );


    /** \brief  Returns the return type of a known non-ambiguous function declaration.
     *  \param  strFuntionName  The name of the functions whose return type shall be retrieved. */
    ::clang::QualType _GetFunctionReturnType(std::string strFuntionName);

    //@}

  protected:
    /** \name Clang bugfixing methods */
    //@{

    /** \brief  Creates all required missing intrinsic function declarations for the SSE instruction set (Clang header are incomplete). */
    void _CreateMissingIntrinsicsSSE();

    /** \brief  Creates all required missing intrinsic function declarations for the SSE2 instruction set (Clang header are incomplete). */
    void _CreateMissingIntrinsicsSSE2();

    /** \brief  Creates all required missing intrinsic function declarations for the SSE4.1 instruction set (Clang header are incomplete). */
    void _CreateMissingIntrinsicsSSE4_1();

    //@}


  protected:

    InstructionSetBase(::clang::ASTContext &rAstContext, std::string strFunctionNamePrefix = "");

  public:

    template < class InstructionSetType >
    inline static std::shared_ptr< InstructionSetType > Create(::clang::ASTContext &rAstContext)
    {
      static_assert( std::is_base_of< InstructionSetBase, InstructionSetType >::value, "The requested instruction set is not derived from class \"InstructionSetBase\" !" );

      return std::shared_ptr< InstructionSetType >( new InstructionSetType(rAstContext) );
    }


    virtual ~InstructionSetBase()
    {
      _mapKnownFuncDecls.clear();
    }

  public:

    /** \name Instruction set abstraction methods */
    //@{

    inline ::clang::Expr* CreateOnesVector(VectorElementTypes eElementType)
    {
      return CreateOnesVector(eElementType, false);
    }

    inline ::clang::Expr* CreateVector(VectorElementTypes eElementType, const ClangASTHelper::ExpressionVectorType &crvecElements)
    {
      return CreateVector(eElementType, crvecElements, false);
    }

    inline  size_t GetVectorElementCount(VectorElementTypes eElementType) const    { return GetVectorWidthBytes() / AST::BaseClasses::TypeInfo::GetTypeSize(eElementType); }

    virtual ::clang::QualType GetVectorType(VectorElementTypes eElementType) = 0;
    virtual size_t            GetVectorWidthBytes() const = 0;

    virtual ::clang::Expr* ArithmeticOperator(VectorElementTypes eElementType, ArithmeticOperatorType eOpType, ::clang::Expr *pExprLHS, ::clang::Expr *pExprRHS) = 0;
    virtual ::clang::Expr* BroadCast(VectorElementTypes eElementType, ::clang::Expr *pBroadCastValue) = 0;
    virtual ::clang::Expr* CheckActiveElements(VectorElementTypes eMaskElementType, ActiveElementsCheckType eCheckType, ::clang::Expr *pMaskExpr) = 0;
    virtual ::clang::Expr* CreateOnesVector(VectorElementTypes eElementType, bool bNegative) = 0;
    virtual ::clang::Expr* CreateVector(VectorElementTypes eElementType, const ClangASTHelper::ExpressionVectorType &crvecElements, bool bReversedOrder) = 0;
    virtual ::clang::Expr* CreateZeroVector(VectorElementTypes eElementType) = 0;
    virtual ::clang::Expr* ExtractElement(VectorElementTypes eElementType, ::clang::Expr *pVectorRef, std::uint32_t uiIndex) = 0;
    virtual ::clang::Expr* InsertElement(VectorElementTypes eElementType, ::clang::Expr *pVectorRef, ::clang::Expr *pElementValue, std::uint32_t uiIndex) = 0;
    virtual ::clang::Expr* LoadVector(VectorElementTypes eElementType, ::clang::Expr *pPointerRef) = 0;
    virtual ::clang::Expr* RelationalOperator(VectorElementTypes eElementType, RelationalOperatorType eOpType, ::clang::Expr *pExprLHS, ::clang::Expr *pExprRHS) = 0;
    virtual ::clang::Expr* StoreVector(VectorElementTypes eElementType, ::clang::Expr *pPointerRef, ::clang::Expr *pVectorValue) = 0;
    virtual ::clang::Expr* UnaryOperator(VectorElementTypes eElementType, UnaryOperatorType eOpType, ::clang::Expr *pSubExpr) = 0;

    //@}
  };

  typedef std::shared_ptr< InstructionSetBase >   InstructionSetBasePtr;


  class InstructionSetSSE : public InstructionSetBase
  {
  private:

    friend class InstructionSetBase;

    enum class IntrinsicsSSEEnum
    {
      AddFloat,
      AndFloat,
      AndNotFloat,
      BroadCastFloat,
      CompareEqualFloat,
      CompareGreaterEqualFloat,
      CompareGreaterThanFloat,
      CompareLessEqualFloat,
      CompareLessThanFloat,
      CompareNotEqualFloat,
      CompareNotGreaterEqualFloat,
      CompareNotGreaterThanFloat,
      CompareNotLessEqualFloat,
      CompareNotLessThanFloat,
      DivideFloat,
      ExtractLowestFloat,
      InsertLowestFloat,
      LoadFloat,
      MaxFloat,
      MinFloat,
      MoveMaskFloat,
      MultiplyFloat,
      OrFloat,
      ReciprocalFloat,
      ReciprocalSqrtFloat,
      SetFloat,
      SetReverseFloat,
      SetZeroFloat,
      ShuffleFloat,
      SqrtFloat,
      StoreFloat,
      SubtractFloat,
      XorFloat
    };


    typedef InstructionSetBase::IntrinsicMapTemplateType< IntrinsicsSSEEnum >   IntrinsicMapType;


  private:

    IntrinsicMapType    _mapIntrinsicsSSE;


    inline ::clang::CallExpr* _CreateFunctionCall(IntrinsicsSSEEnum eIntrinID, const ClangASTHelper::ExpressionVectorType &crvecArguments)
    {
      return InstructionSetBase::_CreateFunctionCall(_mapIntrinsicsSSE, eIntrinID, crvecArguments);
    }

    inline ::clang::CallExpr* _CreateFunctionCall(IntrinsicsSSEEnum eIntrinID)
    {
      return _CreateFunctionCall(eIntrinID, ClangASTHelper::ExpressionVectorType());
    }

    inline ::clang::CallExpr* _CreateFunctionCall(IntrinsicsSSEEnum eIntrinID, ::clang::Expr *pArg1)
    {
      ClangASTHelper::ExpressionVectorType vecArguments;

      vecArguments.push_back(pArg1);

      return _CreateFunctionCall(eIntrinID, vecArguments);
    }

    inline ::clang::CallExpr* _CreateFunctionCall(IntrinsicsSSEEnum eIntrinID, ::clang::Expr *pArg1, ::clang::Expr *pArg2)
    {
      ClangASTHelper::ExpressionVectorType vecArguments;

      vecArguments.push_back(pArg1);
      vecArguments.push_back(pArg2);

      return _CreateFunctionCall(eIntrinID, vecArguments);
    }

    inline ::clang::CallExpr* _CreateFunctionCall(IntrinsicsSSEEnum eIntrinID, ::clang::Expr *pArg1, ::clang::Expr *pArg2, ::clang::Expr *pArg3)
    {
      ClangASTHelper::ExpressionVectorType vecArguments;

      vecArguments.push_back(pArg1);
      vecArguments.push_back(pArg2);
      vecArguments.push_back(pArg3);

      return _CreateFunctionCall(eIntrinID, vecArguments);
    }

    inline ::clang::Expr* _CreatePrefixedUnaryOp(IntrinsicsSSEEnum eIntrinID, VectorElementTypes eElementType, ::clang::Expr *pVectorRef)
    {
      return InstructionSetBase::_CreatePrefixedUnaryOp(_mapIntrinsicsSSE, eIntrinID, eElementType, pVectorRef);
    }

    inline ::clang::QualType _GetFunctionReturnType(IntrinsicsSSEEnum eIntrinID)
    {
      return InstructionSetBase::_GetFunctionReturnType(_mapIntrinsicsSSE, eIntrinID);
    }


    inline void _InitIntrinsic(IntrinsicsSSEEnum eIntrinType, std::string strIntrinName)
    {
      InstructionSetBase::_InitIntrinsic(_mapIntrinsicsSSE, eIntrinType, strIntrinName);
    }

    void _InitIntrinsicsMap();

    inline void _LookupIntrinsics()
    {
      InstructionSetBase::_LookupIntrinsics(_mapIntrinsicsSSE, "SSE");
    }


    void _CheckElementType(VectorElementTypes eElementType) const;

    template <class ExceptionType>
    inline void _CheckIndex(VectorElementTypes eElementType, std::uint32_t uiIndex) const
    {
      uint32_t uiUpperLimit = GetVectorElementCount(eElementType) - 1;

      if (uiIndex > uiUpperLimit)
      {
        throw ExceptionType(eElementType, uiUpperLimit);
      }
    }

  protected:

    inline void _CheckExtractIndex(VectorElementTypes eElementType, std::uint32_t uiIndex) const  { _CheckIndex< InstructionSetExceptions::ExtractIndexOutOfRange >(eElementType, uiIndex); }
    inline void _CheckInsertIndex(VectorElementTypes eElementType, std::uint32_t uiIndex) const   { _CheckIndex< InstructionSetExceptions::InsertIndexOutOfRange  >(eElementType, uiIndex); }

    static inline std::string _GetIntrinsicPrefix() { return "_mm_"; }


    InstructionSetSSE(::clang::ASTContext &rAstContext);

  public:

    virtual ~InstructionSetSSE()
    {
      _mapIntrinsicsSSE.clear();
    }


    /** \name Instruction set abstraction methods */
    //@{

    virtual ::clang::QualType GetVectorType(VectorElementTypes eElementType) override;
    virtual size_t            GetVectorWidthBytes() const final override   { return static_cast< size_t >(16); }

    virtual ::clang::Expr* ArithmeticOperator(VectorElementTypes eElementType, ArithmeticOperatorType eOpType, ::clang::Expr *pExprLHS, ::clang::Expr *pExprRHS) override;
    virtual ::clang::Expr* BroadCast(VectorElementTypes eElementType, ::clang::Expr *pBroadCastValue) override;
    virtual ::clang::Expr* CheckActiveElements(VectorElementTypes eMaskElementType, ActiveElementsCheckType eCheckType, ::clang::Expr *pMaskExpr) override;
    virtual ::clang::Expr* CreateOnesVector(VectorElementTypes eElementType, bool bNegative) override;
    virtual ::clang::Expr* CreateVector(VectorElementTypes eElementType, const ClangASTHelper::ExpressionVectorType &crvecElements, bool bReversedOrder) override;
    virtual ::clang::Expr* CreateZeroVector(VectorElementTypes eElementType) override;
    virtual ::clang::Expr* ExtractElement(VectorElementTypes eElementType, ::clang::Expr *pVectorRef, std::uint32_t uiIndex) override;
    virtual ::clang::Expr* InsertElement(VectorElementTypes eElementType, ::clang::Expr *pVectorRef, ::clang::Expr *pElementValue, std::uint32_t uiIndex) override;
    virtual ::clang::Expr* LoadVector(VectorElementTypes eElementType, ::clang::Expr *pPointerRef) override;
    virtual ::clang::Expr* RelationalOperator(VectorElementTypes eElementType, RelationalOperatorType eOpType, ::clang::Expr *pExprLHS, ::clang::Expr *pExprRHS) override;
    virtual ::clang::Expr* StoreVector(VectorElementTypes eElementType, ::clang::Expr *pPointerRef, ::clang::Expr *pVectorValue) override;
    virtual ::clang::Expr* UnaryOperator(VectorElementTypes eElementType, UnaryOperatorType eOpType, ::clang::Expr *pSubExpr) override;

    //@}
  };

  class InstructionSetSSE2 : public InstructionSetSSE
  {
  private:

    friend class InstructionSetBase;
    typedef InstructionSetSSE   BaseType;


    enum class IntrinsicsSSE2Enum
    {
      AddDouble,                    AddInt8,                AddInt16,                AddInt32,                AddInt64,
      AndDouble,                    AndInteger,             AndNotDouble,            AndNotInteger,
      BroadCastDouble,              BroadCastInt8,          BroadCastInt16,          BroadCastInt32,          BroadCastInt64,
      CastDoubleToFloat,            CastDoubleToInteger,    CastFloatToDouble,       CastFloatToInteger,      CastIntegerToDouble, CastIntegerToFloat,
      CompareEqualDouble,           CompareEqualInt8,       CompareEqualInt16,       CompareEqualInt32,
      CompareGreaterEqualDouble,
      CompareGreaterThanDouble,     CompareGreaterThanInt8, CompareGreaterThanInt16, CompareGreaterThanInt32,
      CompareLessEqualDouble,
      CompareLessThanDouble,        CompareLessThanInt8,    CompareLessThanInt16,    CompareLessThanInt32,
      CompareNotEqualDouble,
      CompareNotGreaterEqualDouble,
      CompareNotGreaterThanDouble,
      CompareNotLessEqualDouble,
      CompareNotLessThanDouble,
      ConvertDoubleFloat,           ConvertDoubleInt32,     ConvertFloatDouble,      ConvertFloatInt32,       ConvertInt32Double,  ConvertInt32Float,
      DivideDouble,
      ExtractInt16,                 ExtractLowestDouble,    ExtractLowestInt32,      ExtractLowestInt64,
      InsertInt16,                  InsertLowestDouble,
      LoadDouble,                   LoadInteger,
      MaxDouble,                    MaxUInt8,               MaxInt16,
      MinDouble,                    MinUInt8,               MinInt16,
      MoveMaskDouble,               MoveMaskInt8,
      MultiplyDouble,               MultiplyInt16,
      OrDouble,                     OrInteger,
      PackInt16ToInt8,              PackInt16ToUInt8,       PackInt32ToInt16,
      SetDouble,                    SetInt8,                SetInt16,                SetInt32,                SetInt64,
      SetReverseDouble,             SetReverseInt8,         SetReverseInt16,         SetReverseInt32,
      SetZeroDouble,                SetZeroInteger,
      ShiftLeftInt16,               ShiftLeftInt32,         ShiftLeftVectorBytes,
      ShiftRightInt16,              ShiftRightInt32,        ShiftRightVectorBytes,
      ShuffleDouble,                ShuffleInt16High,       ShuffleInt16Low,         ShuffleInt32,
      SqrtDouble,
      StoreDouble,                  StoreInteger,           StoreConditionalInteger,
      SubtractDouble,               SubtractInt8,           SubtractInt16,           SubtractInt32,           SubtractInt64,
      XorDouble,                    XorInteger
    };

    typedef InstructionSetBase::IntrinsicMapTemplateType< IntrinsicsSSE2Enum >  IntrinsicMapType;


  private:

    IntrinsicMapType    _mapIntrinsicsSSE2;


    inline ::clang::CallExpr* _CreateFunctionCall(IntrinsicsSSE2Enum eIntrinID, const ClangASTHelper::ExpressionVectorType &crvecArguments)
    {
      return InstructionSetBase::_CreateFunctionCall(_mapIntrinsicsSSE2, eIntrinID, crvecArguments);
    }

    inline ::clang::CallExpr* _CreateFunctionCall(IntrinsicsSSE2Enum eIntrinID)
    {
      return _CreateFunctionCall(eIntrinID, ClangASTHelper::ExpressionVectorType());
    }

    inline ::clang::CallExpr* _CreateFunctionCall(IntrinsicsSSE2Enum eIntrinID, ::clang::Expr *pArg1)
    {
      ClangASTHelper::ExpressionVectorType vecArguments;

      vecArguments.push_back(pArg1);

      return _CreateFunctionCall(eIntrinID, vecArguments);
    }

    inline ::clang::CallExpr* _CreateFunctionCall(IntrinsicsSSE2Enum eIntrinID, ::clang::Expr *pArg1, ::clang::Expr *pArg2)
    {
      ClangASTHelper::ExpressionVectorType vecArguments;

      vecArguments.push_back(pArg1);
      vecArguments.push_back(pArg2);

      return _CreateFunctionCall(eIntrinID, vecArguments);
    }

    inline ::clang::CallExpr* _CreateFunctionCall(IntrinsicsSSE2Enum eIntrinID, ::clang::Expr *pArg1, ::clang::Expr *pArg2, ::clang::Expr *pArg3)
    {
      ClangASTHelper::ExpressionVectorType vecArguments;

      vecArguments.push_back(pArg1);
      vecArguments.push_back(pArg2);
      vecArguments.push_back(pArg3);

      return _CreateFunctionCall(eIntrinID, vecArguments);
    }

    inline ::clang::QualType _GetFunctionReturnType(IntrinsicsSSE2Enum eIntrinID)
    {
      return InstructionSetBase::_GetFunctionReturnType(_mapIntrinsicsSSE2, eIntrinID);
    }


    inline void _InitIntrinsic(IntrinsicsSSE2Enum eIntrinType, std::string strIntrinName)
    {
      InstructionSetBase::_InitIntrinsic(_mapIntrinsicsSSE2, eIntrinType, strIntrinName);
    }

    void _InitIntrinsicsMap();

    inline void _LookupIntrinsics()
    {
      InstructionSetBase::_LookupIntrinsics(_mapIntrinsicsSSE2, "SSE2");
    }


  private:

    ::clang::Expr* _CompareInt64(VectorElementTypes eElementType, ::clang::Expr *pExprLHS, ::clang::Expr *pExprRHS, ::clang::BinaryOperatorKind eOpKind);
    ::clang::Expr* _InsertElementDouble(::clang::Expr *pVectorRef, ::clang::Expr *pBroadCastedValue, std::uint32_t uiIndex);
    ::clang::Expr* _RelationalOpInteger(VectorElementTypes eElementType, RelationalOperatorType eOpType, ::clang::Expr *pExprLHS, ::clang::Expr *pExprRHS);

  protected:

    InstructionSetSSE2(::clang::ASTContext &rAstContext);

  public:

    virtual ~InstructionSetSSE2()
    {
      _mapIntrinsicsSSE2.clear();
    }


    /** \name Instruction set abstraction methods */
    //@{

    virtual ::clang::QualType GetVectorType(VectorElementTypes eElementType) final override;

    virtual ::clang::Expr* BroadCast(VectorElementTypes eElementType, ::clang::Expr *pBroadCastValue) final override;
    virtual ::clang::Expr* CheckActiveElements(VectorElementTypes eMaskElementType, ActiveElementsCheckType eCheckType, ::clang::Expr *pMaskExpr) final override;
    virtual ::clang::Expr* CreateOnesVector(VectorElementTypes eElementType, bool bNegative) final override;
    virtual ::clang::Expr* CreateVector(VectorElementTypes eElementType, const ClangASTHelper::ExpressionVectorType &crvecElements, bool bReversedOrder) final override;
    virtual ::clang::Expr* CreateZeroVector(VectorElementTypes eElementType) final override;
    virtual ::clang::Expr* ExtractElement(VectorElementTypes eElementType, ::clang::Expr *pVectorRef, std::uint32_t uiIndex) override;
    virtual ::clang::Expr* InsertElement(VectorElementTypes eElementType, ::clang::Expr *pVectorRef, ::clang::Expr *pElementValue, std::uint32_t uiIndex) override;
    virtual ::clang::Expr* LoadVector(VectorElementTypes eElementType, ::clang::Expr *pPointerRef) override;
    virtual ::clang::Expr* StoreVector(VectorElementTypes eElementType, ::clang::Expr *pPointerRef, ::clang::Expr *pVectorValue) final override;
    virtual ::clang::Expr* RelationalOperator(VectorElementTypes eElementType, RelationalOperatorType eOpType, ::clang::Expr *pExprLHS, ::clang::Expr *pExprRHS) override;

    //@}
  };

  class InstructionSetSSE3 : public InstructionSetSSE2
  {
  private:

    friend class InstructionSetBase;
    typedef InstructionSetSSE2    BaseType;


    enum class IntrinsicsSSE3Enum
    {
      LoadInteger
    };

    typedef InstructionSetBase::IntrinsicMapTemplateType< IntrinsicsSSE3Enum >  IntrinsicMapType;


  private:

    IntrinsicMapType    _mapIntrinsicsSSE3;


    inline ::clang::CallExpr* _CreateFunctionCall(IntrinsicsSSE3Enum eIntrinID, const ClangASTHelper::ExpressionVectorType &crvecArguments)
    {
      return InstructionSetBase::_CreateFunctionCall(_mapIntrinsicsSSE3, eIntrinID, crvecArguments);
    }

    inline ::clang::CallExpr* _CreateFunctionCall(IntrinsicsSSE3Enum eIntrinID, ::clang::Expr *pArg1)
    {
      ClangASTHelper::ExpressionVectorType vecArguments;

      vecArguments.push_back(pArg1);

      return _CreateFunctionCall(eIntrinID, vecArguments);
    }

    inline ::clang::QualType _GetFunctionReturnType(IntrinsicsSSE3Enum eIntrinID)
    {
      return InstructionSetBase::_GetFunctionReturnType(_mapIntrinsicsSSE3, eIntrinID);
    }


    inline void _InitIntrinsic(IntrinsicsSSE3Enum eIntrinType, std::string strIntrinName)
    {
      InstructionSetBase::_InitIntrinsic(_mapIntrinsicsSSE3, eIntrinType, strIntrinName);
    }

    void _InitIntrinsicsMap();

    inline void _LookupIntrinsics()
    {
      InstructionSetBase::_LookupIntrinsics(_mapIntrinsicsSSE3, "SSE3");
    }


  protected:

    InstructionSetSSE3(::clang::ASTContext &rAstContext);

  public:

    virtual ~InstructionSetSSE3()
    {
      _mapIntrinsicsSSE3.clear();
    }


    /** \name Instruction set abstraction methods */
    //@{

    virtual ::clang::Expr* ExtractElement(VectorElementTypes eElementType, ::clang::Expr *pVectorRef, std::uint32_t uiIndex) override;
    virtual ::clang::Expr* InsertElement(VectorElementTypes eElementType, ::clang::Expr *pVectorRef, ::clang::Expr *pElementValue, std::uint32_t uiIndex) override;
    virtual ::clang::Expr* LoadVector(VectorElementTypes eElementType, ::clang::Expr *pPointerRef) final override;
    virtual ::clang::Expr* RelationalOperator(VectorElementTypes eElementType, RelationalOperatorType eOpType, ::clang::Expr *pExprLHS, ::clang::Expr *pExprRHS) override;

    //@}
  };

  class InstructionSetSSSE3 : public InstructionSetSSE3
  {
  private:

    friend class InstructionSetBase;
    typedef InstructionSetSSE3    BaseType;


    enum class IntrinsicsSSSE3Enum
    {
      AbsoluteInt8, AbsoluteInt16, AbsoluteInt32,
      ShuffleInt8,
      SignInt8,     SignInt16,     SignInt32
    };

    typedef InstructionSetBase::IntrinsicMapTemplateType< IntrinsicsSSSE3Enum >  IntrinsicMapType;


  private:

    IntrinsicMapType    _mapIntrinsicsSSSE3;

    inline void _InitIntrinsic(IntrinsicsSSSE3Enum eIntrinType, std::string strIntrinName)
    {
      InstructionSetBase::_InitIntrinsic(_mapIntrinsicsSSSE3, eIntrinType, strIntrinName);
    }

    void _InitIntrinsicsMap();

    inline void _LookupIntrinsics()
    {
      InstructionSetBase::_LookupIntrinsics(_mapIntrinsicsSSSE3, "SSSE3");
    }


  protected:

    InstructionSetSSSE3(::clang::ASTContext &rAstContext);

  public:

    virtual ~InstructionSetSSSE3()
    {
      _mapIntrinsicsSSSE3.clear();
    }


    /** \name Instruction set abstraction methods */
    //@{

    virtual ::clang::Expr* ExtractElement(VectorElementTypes eElementType, ::clang::Expr *pVectorRef, std::uint32_t uiIndex) override;
    virtual ::clang::Expr* InsertElement(VectorElementTypes eElementType, ::clang::Expr *pVectorRef, ::clang::Expr *pElementValue, std::uint32_t uiIndex) override;
    virtual ::clang::Expr* RelationalOperator(VectorElementTypes eElementType, RelationalOperatorType eOpType, ::clang::Expr *pExprLHS, ::clang::Expr *pExprRHS) override;

    //@}
  };

  class InstructionSetSSE4_1 : public InstructionSetSSSE3
  {
  private:

    friend class InstructionSetBase;
    typedef InstructionSetSSSE3    BaseType;


    enum class IntrinsicsSSE4_1Enum
    {
      BlendDouble,        BlendFloat,         BlendInteger,
      CompareEqualInt64,
      ConvertInt8Int16,   ConvertInt8Int32,   ConvertInt8Int64,
      ConvertInt16Int32,  ConvertInt16Int64,  ConvertInt32Int64,
      ConvertUInt8Int16,  ConvertUInt8Int32,  ConvertUInt8Int64,
      ConvertUInt16Int32, ConvertUInt16Int64, ConvertUInt32Int64,
      ExtractInt8,        ExtractInt32,       ExtractInt64,
      InsertFloat,        InsertInt8,         InsertInt32,        InsertInt64,
      MaxInt8,            MaxInt32,           MaxUInt16,          MaxUInt32,
      MinInt8,            MinInt32,           MinUInt16,          MinUInt32,
      MultiplyInt32,
      PackInt32ToUInt16,
      TestControl
    };

    typedef InstructionSetBase::IntrinsicMapTemplateType< IntrinsicsSSE4_1Enum >  IntrinsicMapType;


  private:

    IntrinsicMapType    _mapIntrinsicsSSE4_1;


    inline ::clang::CallExpr* _CreateFunctionCall(IntrinsicsSSE4_1Enum eIntrinID, const ClangASTHelper::ExpressionVectorType &crvecArguments)
    {
      return InstructionSetBase::_CreateFunctionCall(_mapIntrinsicsSSE4_1, eIntrinID, crvecArguments);
    }

    inline ::clang::CallExpr* _CreateFunctionCall(IntrinsicsSSE4_1Enum eIntrinID, ::clang::Expr *pArg1, ::clang::Expr *pArg2)
    {
      ClangASTHelper::ExpressionVectorType vecArguments;

      vecArguments.push_back(pArg1);
      vecArguments.push_back(pArg2);

      return _CreateFunctionCall(eIntrinID, vecArguments);
    }

    inline ::clang::CallExpr* _CreateFunctionCall(IntrinsicsSSE4_1Enum eIntrinID, ::clang::Expr *pArg1, ::clang::Expr *pArg2, ::clang::Expr *pArg3)
    {
      ClangASTHelper::ExpressionVectorType vecArguments;

      vecArguments.push_back(pArg1);
      vecArguments.push_back(pArg2);
      vecArguments.push_back(pArg3);

      return _CreateFunctionCall(eIntrinID, vecArguments);
    }


    inline void _InitIntrinsic(IntrinsicsSSE4_1Enum eIntrinType, std::string strIntrinName)
    {
      InstructionSetBase::_InitIntrinsic(_mapIntrinsicsSSE4_1, eIntrinType, strIntrinName);
    }

    void _InitIntrinsicsMap();

    inline void _LookupIntrinsics()
    {
      InstructionSetBase::_LookupIntrinsics(_mapIntrinsicsSSE4_1, "SSE4.1");
    }


    ::clang::Expr* _ExtractElement(VectorElementTypes eElementType, IntrinsicsSSE4_1Enum eIntrinType, ::clang::Expr *pVectorRef, std::uint32_t uiIndex);
    ::clang::Expr* _InsertElement(VectorElementTypes eElementType, IntrinsicsSSE4_1Enum eIntrinType, ::clang::Expr *pVectorRef, ::clang::Expr *pElementValue, std::uint32_t uiIndex);


  protected:

    InstructionSetSSE4_1(::clang::ASTContext &rAstContext);


  public:

    virtual ~InstructionSetSSE4_1()
    {
      _mapIntrinsicsSSE4_1.clear();
    }


    /** \name Instruction set abstraction methods */
    //@{

    virtual ::clang::Expr* ExtractElement(VectorElementTypes eElementType, ::clang::Expr *pVectorRef, std::uint32_t uiIndex) final override;
    virtual ::clang::Expr* InsertElement(VectorElementTypes eElementType, ::clang::Expr *pVectorRef, ::clang::Expr *pElementValue, std::uint32_t uiIndex) final override;
    virtual ::clang::Expr* RelationalOperator(VectorElementTypes eElementType, RelationalOperatorType eOpType, ::clang::Expr *pExprLHS, ::clang::Expr *pExprRHS) override;

    //@}
  };

  class InstructionSetSSE4_2 final : public InstructionSetSSE4_1
  {
  private:

    friend class InstructionSetBase;
    typedef InstructionSetSSE4_1    BaseType;


    enum class IntrinsicsSSE4_2Enum
    {
      CompareGreaterThanInt64
    };

    typedef InstructionSetBase::IntrinsicMapTemplateType< IntrinsicsSSE4_2Enum >  IntrinsicMapType;


  private:

    IntrinsicMapType    _mapIntrinsicsSSE4_2;


    inline ::clang::CallExpr* _CreateFunctionCall(IntrinsicsSSE4_2Enum eIntrinID, const ClangASTHelper::ExpressionVectorType &crvecArguments)
    {
      return InstructionSetBase::_CreateFunctionCall(_mapIntrinsicsSSE4_2, eIntrinID, crvecArguments);
    }

    inline ::clang::CallExpr* _CreateFunctionCall(IntrinsicsSSE4_2Enum eIntrinID, ::clang::Expr *pArg1, ::clang::Expr *pArg2)
    {
      ClangASTHelper::ExpressionVectorType vecArguments;

      vecArguments.push_back(pArg1);
      vecArguments.push_back(pArg2);

      return _CreateFunctionCall(eIntrinID, vecArguments);
    }


    inline void _InitIntrinsic(IntrinsicsSSE4_2Enum eIntrinType, std::string strIntrinName)
    {
      InstructionSetBase::_InitIntrinsic(_mapIntrinsicsSSE4_2, eIntrinType, strIntrinName);
    }

    void _InitIntrinsicsMap();

    inline void _LookupIntrinsics()
    {
      InstructionSetBase::_LookupIntrinsics(_mapIntrinsicsSSE4_2, "SSE4.2");
    }


  private:

    InstructionSetSSE4_2(::clang::ASTContext &rAstContext);

  public:

    virtual ~InstructionSetSSE4_2()
    {
      _mapIntrinsicsSSE4_2.clear();
    }


    /** \name Instruction set abstraction methods */
    //@{

    virtual ::clang::Expr* RelationalOperator(VectorElementTypes eElementType, RelationalOperatorType eOpType, ::clang::Expr *pExprLHS, ::clang::Expr *pExprRHS) final override;

    //@}
  };
} // end namespace Vectorization
} // end namespace Backend
} // end namespace hipacc
} // end namespace clang


#ifdef VERBOSE_INIT_MODE
#undef VERBOSE_INIT_MODE
#endif


#endif  // _BACKEND_INSTRUCTION_SETS_H_

// vim: set ts=2 sw=2 sts=2 et ai:

