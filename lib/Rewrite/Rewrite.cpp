//
// Copyright (c) 2012, University of Erlangen-Nuremberg
// Copyright (c) 2012, Siemens AG
// Copyright (c) 2010, ARM Limited
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

//===--- Rewrite.cpp - Mapping the DSL (AST nodes) to the runtime ---------===//
//
// This file implements functionality for mapping the DSL to the HIPAcc runtime.
//
//===----------------------------------------------------------------------===//

#include "hipacc/Rewrite/Rewrite.h"


// Re-route POSIX functions for Visual Studio
#ifdef _MSC_VER

#include <io.h>

#define fsync(fd) _commit(fd)
#define open(filename, oflag, pmode) _open(filename, oflag, pmode)
#define popen(command, mode) _popen(command, mode)
#define pclose(file) _pclose(file)

#endif


using namespace clang;
using namespace hipacc;
using namespace ASTNode;


namespace {
class Rewrite : public ASTConsumer,  public RecursiveASTVisitor<Rewrite> {
  private:
    // Clang internals
    CompilerInstance &CI;
    ASTContext &Context;
    DiagnosticsEngine &Diags;
    llvm::raw_ostream &Out;
    bool dump;
    SourceManager *SM;
    Rewriter TextRewriter;
    Rewriter::RewriteOptions TextRewriteOptions;

    // HIPACC instances
    CompilerOptions &compilerOptions;
    HipaccDevice targetDevice;
    hipacc::Builtin::Context builtins;
    CreateHostStrings stringCreator;

    // compiler known/built-in C++ classes
    CompilerKnownClasses compilerClasses;

    // mapping between AST nodes and internal class representation
    llvm::DenseMap<RecordDecl *, HipaccKernelClass *> KernelClassDeclMap;
    llvm::DenseMap<ValueDecl *, HipaccAccessor *> AccDeclMap;
    llvm::DenseMap<ValueDecl *, HipaccBoundaryCondition *> BCDeclMap;
    llvm::DenseMap<ValueDecl *, HipaccImage *> ImgDeclMap;
    llvm::DenseMap<ValueDecl *, HipaccPyramid *> PyrDeclMap;
    llvm::DenseMap<ValueDecl *, HipaccIterationSpace *> ISDeclMap;
    llvm::DenseMap<ValueDecl *, HipaccKernel *> KernelDeclMap;
    llvm::DenseMap<ValueDecl *, HipaccMask *> MaskDeclMap;

    // store interpolation methods required for CUDA
    SmallVector<std::string, 16> InterpolationDefinitionsGlobal;

    // pointer to main function
    FunctionDecl *mainFD;
    FileID mainFileID;
    unsigned int literalCount;
    unsigned int isLiteralCount;

  public:
    Rewrite(CompilerInstance &CI, CompilerOptions &options, llvm::raw_ostream*
        o=nullptr, bool dump=false) :
      CI(CI),
      Context(CI.getASTContext()),
      Diags(CI.getASTContext().getDiagnostics()),
      Out(o? *o : llvm::outs()),
      dump(dump),
      compilerOptions(options),
      targetDevice(options),
      builtins(CI.getASTContext()),
      stringCreator(CreateHostStrings(options)),
      compilerClasses(CompilerKnownClasses()),
      mainFD(nullptr),
      literalCount(0),
      isLiteralCount(0)
    {}

    void HandleTranslationUnit(ASTContext &Context);
    bool HandleTopLevelDecl(DeclGroupRef D);

    bool VisitCXXRecordDecl(CXXRecordDecl *D);
    bool VisitDeclStmt(DeclStmt *D);
    bool VisitFunctionDecl(FunctionDecl *D);
    bool VisitCXXOperatorCallExpr(CXXOperatorCallExpr *E);
    bool VisitBinaryOperator(BinaryOperator *E);
    bool VisitCXXMemberCallExpr(CXXMemberCallExpr *E);
    bool VisitCallExpr (CallExpr *E);

    //bool shouldVisitTemplateInstantiations() const { return true; }

  private:
    void Initialize(ASTContext &Context) {
      SM = &Context.getSourceManager();

      // get the ID and start/end of the main file.
      mainFileID = SM->getMainFileID();
      TextRewriter.setSourceMgr(Context.getSourceManager(),
          Context.getLangOpts());
      TextRewriteOptions.RemoveLineIfEmpty = true;
    }

    void setKernelConfiguration(HipaccKernelClass *KC, HipaccKernel *K);
    void printReductionFunction(HipaccKernelClass *KC, HipaccKernel *K,
        PrintingPolicy Policy, llvm::raw_ostream *OS);
    void printKernelFunction(FunctionDecl *D, HipaccKernelClass *KC,
        HipaccKernel *K, std::string file, bool emitHints);
};
}
ASTConsumer *CreateRewrite(CompilerInstance &CI, CompilerOptions &options,
    llvm::raw_ostream *out) {
  return new Rewrite(CI, options, out);
}


ASTConsumer *HipaccRewriteAction::CreateASTConsumer(CompilerInstance &CI,
    StringRef InFile) {
  if (llvm::raw_ostream *OS = CI.createDefaultOutputFile(false, InFile)) {
    return CreateRewrite(CI, options, OS);
  }

  return nullptr;
}


void Rewrite::HandleTranslationUnit(ASTContext &Context) {
  assert(compilerClasses.Coordinate && "Coordinate class not found!");
  assert(compilerClasses.Image && "Image class not found!");
  assert(compilerClasses.BoundaryCondition && "BoundaryCondition class not found!");
  assert(compilerClasses.AccessorBase && "AccessorBase class not found!");
  assert(compilerClasses.Accessor && "Accessor class not found!");
  assert(compilerClasses.AccessorNN && "AccessorNN class not found!");
  assert(compilerClasses.AccessorLF && "AccessorLF class not found!");
  assert(compilerClasses.AccessorCF && "AccessorCF class not found!");
  assert(compilerClasses.AccessorL3 && "AccessorL3 class not found!");
  assert(compilerClasses.IterationSpaceBase && "IterationSpaceBase class not found!");
  assert(compilerClasses.IterationSpace && "IterationSpace class not found!");
  assert(compilerClasses.ElementIterator && "ElementIterator class not found!");
  assert(compilerClasses.Kernel && "Kernel class not found!");
  assert(compilerClasses.Mask && "Mask class not found!");
  assert(compilerClasses.Domain && "Domain class not found!");
  assert(compilerClasses.Pyramid && "Pyramid class not found!");
  assert(compilerClasses.HipaccEoP && "HipaccEoP class not found!");

  StringRef MainBuf = SM->getBufferData(mainFileID);
  const char *mainFileStart = MainBuf.begin();
  const char *mainFileEnd = MainBuf.end();
  SourceLocation locStart = SM->getLocForStartOfFile(mainFileID);

  size_t includeLen = strlen("include");
  size_t hipaccHdrLen = strlen("hipacc.hpp");
  size_t usingLen = strlen("using");
  size_t namespaceLen = strlen("namespace");
  size_t hipaccLen = strlen("hipacc");

  // loop over the whole file, looking for includes
  for (const char *bufPtr = mainFileStart; bufPtr < mainFileEnd; ++bufPtr) {
    if (*bufPtr == '#') {
      const char *startPtr = bufPtr;
      if (++bufPtr == mainFileEnd)
        break;
      while (*bufPtr == ' ' || *bufPtr == '\t')
        if (++bufPtr == mainFileEnd)
          break;
      if (!strncmp(bufPtr, "include", includeLen)) {
        const char *endPtr = bufPtr + includeLen;
        while (*endPtr == ' ' || *endPtr == '\t')
          if (++endPtr == mainFileEnd)
            break;
        if (*endPtr == '"') {
          if (!strncmp(endPtr+1, "hipacc.hpp", hipaccHdrLen)) {
            endPtr = strchr(endPtr+1, '"');
            // remove hipacc include
            SourceLocation includeLoc =
              locStart.getLocWithOffset(startPtr-mainFileStart);
            TextRewriter.RemoveText(includeLoc, endPtr-startPtr+1,
                TextRewriteOptions);
            bufPtr += endPtr-startPtr;
          }
        }
      }
    }
    if (*bufPtr == 'u') {
      const char *startPtr = bufPtr;
      if (!strncmp(bufPtr, "using", usingLen)) {
        const char *endPtr = bufPtr + usingLen;
        while (*endPtr == ' ' || *endPtr == '\t')
          if (++endPtr == mainFileEnd)
            break;
        if (*endPtr == 'n') {
          if (!strncmp(endPtr, "namespace", namespaceLen)) {
            endPtr += namespaceLen;
            while (*endPtr == ' ' || *endPtr == '\t')
              if (++endPtr == mainFileEnd)
                break;
            if (*endPtr == 'h') {
              if (!strncmp(endPtr, "hipacc", hipaccLen)) {
                endPtr = strchr(endPtr+1, ';');
                // remove using namespace line
                SourceLocation includeLoc =
                  locStart.getLocWithOffset(startPtr-mainFileStart);
                TextRewriter.RemoveText(includeLoc, endPtr-startPtr+1,
                    TextRewriteOptions);
                bufPtr += endPtr-startPtr;
              }
            }
          }
        }
      }
    }
  }


  // add include files for CUDA
  std::string newStr;

  // get include header string, including a header twice is fine
  stringCreator.writeHeaders(newStr);

  // add interpolation include and define interpolation functions for CUDA
  if (compilerOptions.emitCUDA() && InterpolationDefinitionsGlobal.size()) {
    newStr += "#include \"hipacc_cuda_interpolate.hpp\"\n";

    // sort definitions and remove duplicate definitions
    std::sort(InterpolationDefinitionsGlobal.begin(),
        InterpolationDefinitionsGlobal.end());
    InterpolationDefinitionsGlobal.erase(
        std::unique(InterpolationDefinitionsGlobal.begin(),
          InterpolationDefinitionsGlobal.end()),
        InterpolationDefinitionsGlobal.end());

    // add interpolation definitions
    for (size_t i=0, e=InterpolationDefinitionsGlobal.size(); i!=e; ++i) {
      newStr += InterpolationDefinitionsGlobal.data()[i];
    }
    newStr += "\n";
  }

  // include .cu or .h files for normal kernels
  switch (compilerOptions.getTargetCode()) {
    case TARGET_C:
      for (auto it=KernelDeclMap.begin(), ei=KernelDeclMap.end(); it!=ei; ++it)
      {
        HipaccKernel *Kernel = it->second;

        newStr += "#include \"";
        newStr += Kernel->getFileName();
        newStr += ".cc\"\n";
      }
      break;
    case TARGET_CUDA:
      if (!compilerOptions.exploreConfig()) {
        for (auto it=KernelDeclMap.begin(), ei=KernelDeclMap.end(); it!=ei;
                ++it) {
          HipaccKernel *Kernel = it->second;

          newStr += "#include \"";
          newStr += Kernel->getFileName();
          newStr += ".cu\"\n";
        }
      }
      break;
    case TARGET_Renderscript:
    case TARGET_Filterscript:
      for (auto it=KernelDeclMap.begin(), ei=KernelDeclMap.end(); it!=ei; ++it)
      {
        HipaccKernel *Kernel = it->second;

        newStr += "#include \"ScriptC_";
        newStr += Kernel->getFileName();
        newStr += ".h\"\n";
      }
      break;
    default:
      break;
  }


  // write constant memory declarations
  if (compilerOptions.emitCUDA()) {
    for (auto it=MaskDeclMap.begin(), ei=MaskDeclMap.end(); it!=ei; ++it) {
      HipaccMask *Mask = it->second;
      if (Mask->isPrinted()) continue;

      SmallVector<HipaccKernel *, 16> kernels = Mask->getKernels();
      for (size_t i=0; i<kernels.size(); ++i) {
        HipaccKernel *K = kernels[i];

        if (i) newStr += "\n" + stringCreator.getIndent();

        newStr += "__device__ __constant__ ";
        newStr += Mask->getTypeStr();
        newStr += " " + Mask->getName() + K->getName();
        newStr += "[" + Mask->getSizeYStr() + "][" + Mask->getSizeXStr() +
          "];\n";
      }
    }
  }
  // rewrite header section
  TextRewriter.InsertTextBefore(locStart, newStr);


  // initialize CUDA/OpenCL
  assert(mainFD && "no main found!");

  CompoundStmt *CS = dyn_cast<CompoundStmt>(mainFD->getBody());
  assert(CS->size() && "CompoundStmt has no statements.");

  std::string initStr;

  // get initialization string for run-time
  stringCreator.writeInitialization(initStr);

  // load OpenCL kernel files and compile the OpenCL kernels
  if (!compilerOptions.exploreConfig()) {
    for (auto it=KernelDeclMap.begin(), ei=KernelDeclMap.end(); it!=ei; ++it) {
      HipaccKernel *Kernel = it->second;

      stringCreator.writeKernelCompilation(Kernel, initStr);
    }
    initStr += "\n" + stringCreator.getIndent();
  }

  // write Mask transfers to Symbol in CUDA
  if (compilerOptions.emitCUDA()) {
    for (auto it=MaskDeclMap.begin(), ei=MaskDeclMap.end(); it!=ei; ++it) {
      HipaccMask *Mask = it->second;

      if (!compilerOptions.exploreConfig()) {
        std::string newStr;
        if (Mask->hasCopyMask()) {
          stringCreator.writeMemoryTransferDomainFromMask(Mask,
              Mask->getCopyMask(), newStr);
        } else {
          stringCreator.writeMemoryTransferSymbol(Mask, Mask->getHostMemName(),
              HOST_TO_DEVICE, newStr);
        }

        TextRewriter.InsertTextBefore(Mask->getDecl()->getLocStart(), newStr);
      }
    }
  }

  // insert initialization before first statement
  auto BI = CS->body_begin();
  Stmt *S = *BI;
  TextRewriter.InsertTextBefore(S->getLocStart(), initStr);

  // insert memory release calls before last statement (return-statement)
  auto RBI = CS->body_rbegin();
  S = *RBI;
  // release all images
  for (auto it=ImgDeclMap.begin(), ei=ImgDeclMap.end(); it!=ei; ++it) {
    HipaccImage *Img = it->second;
    std::string releaseStr;

    stringCreator.writeMemoryRelease(Img, releaseStr);
    TextRewriter.InsertTextBefore(S->getLocStart(), releaseStr);
  }
  // release all non-const masks
  for (auto it=MaskDeclMap.begin(), ei=MaskDeclMap.end(); it!=ei; ++it) {
    HipaccMask *Mask = it->second;
    std::string releaseStr;

    if (!compilerOptions.emitCUDA() && !Mask->isConstant()) {
      stringCreator.writeMemoryRelease(Mask, releaseStr);
      TextRewriter.InsertTextBefore(S->getLocStart(), releaseStr);
    }
  }
  // release all pyramids
  for (auto it=PyrDeclMap.begin(), ei=PyrDeclMap.end(); it!=ei; ++it) {
    HipaccPyramid *Pyramid = it->second;
    std::string releaseStr;

    stringCreator.writeMemoryRelease(Pyramid, releaseStr, true);
    TextRewriter.InsertTextBefore(S->getLocStart(), releaseStr);
  }

  // get buffer of main file id. If we haven't changed it, then we are done.
  if (const RewriteBuffer *RewriteBuf =
      TextRewriter.getRewriteBufferFor(mainFileID)) {
    Out << std::string(RewriteBuf->begin(), RewriteBuf->end());
  } else {
    llvm::errs() << "No changes to input file, something went wrong!\n";
  }
  Out.flush();
}


bool Rewrite::HandleTopLevelDecl(DeclGroupRef DGR) {
  for (auto I = DGR.begin(), E = DGR.end(); I != E; ++I) {
    Decl *D = *I;

    if (compilerClasses.HipaccEoP) {
      // skip late template class instantiations when templated class instances
      // are created. this is the case if the expansion location is not within
      // the main file
      if (SM->getFileID(SM->getExpansionLoc(D->getLocation()))!=mainFileID)
        continue;
    }
    TraverseDecl(D);
  }

  return true;
}


bool Rewrite::VisitCXXRecordDecl(CXXRecordDecl *D) {
  // return if this is no Class definition
  if (!D->hasDefinition()) return true;

  // a) look for compiler known classes and remember them
  // b) look for user defined kernel classes derived from those stored in
  //    step a). If such a class is found:
  //    - create a mapping between kernel class constructor variables and
  //      kernel parameters and store that mapping.
  //    - analyze image memory access patterns for later usage.

  if (D->getTagKind() == TTK_Class && D->isCompleteDefinition()) {
    DeclContext *DC = D->getEnclosingNamespaceContext();
    if (DC->isNamespace()) {
      NamespaceDecl *NS = dyn_cast<NamespaceDecl>(DC);
      if (NS->getNameAsString() == "hipacc") {
        if (D->getNameAsString() == "Coordinate")
          compilerClasses.Coordinate = D;
        if (D->getNameAsString() == "Image") compilerClasses.Image = D;
        if (D->getNameAsString() == "BoundaryCondition")
          compilerClasses.BoundaryCondition = D;
        if (D->getNameAsString() == "AccessorBase")
          compilerClasses.AccessorBase = D;
        if (D->getNameAsString() == "Accessor") compilerClasses.Accessor = D;
        if (D->getNameAsString() == "AccessorNN")
          compilerClasses.AccessorNN = D;
        if (D->getNameAsString() == "AccessorLF")
          compilerClasses.AccessorLF = D;
        if (D->getNameAsString() == "AccessorCF")
          compilerClasses.AccessorCF = D;
        if (D->getNameAsString() == "AccessorL3")
          compilerClasses.AccessorL3 = D;
        if (D->getNameAsString() == "IterationSpaceBase")
          compilerClasses.IterationSpaceBase = D;
        if (D->getNameAsString() == "IterationSpace")
          compilerClasses.IterationSpace = D;
        if (D->getNameAsString() == "ElementIterator")
          compilerClasses.ElementIterator = D;
        if (D->getNameAsString() == "Kernel") compilerClasses.Kernel = D;
        if (D->getNameAsString() == "Mask") compilerClasses.Mask = D;
        if (D->getNameAsString() == "Domain") compilerClasses.Domain = D;
        if (D->getNameAsString() == "Pyramid") compilerClasses.Pyramid = D;
        if (D->getNameAsString() == "HipaccEoP") compilerClasses.HipaccEoP = D;
      }
    }

    if (!compilerClasses.HipaccEoP) return true;

    HipaccKernelClass *KC = nullptr;

    for (auto I=D->bases_begin(), E=D->bases_end(); I!=E; ++I) {
      // found user kernel class
      if (compilerClasses.isTypeOfTemplateClass(I->getType(),
            compilerClasses.Kernel)) {
        KC = new HipaccKernelClass(D->getNameAsString());
        KernelClassDeclMap[D] = KC;
        // remove user kernel class (semicolon doesn't count to SourceRange)
        SourceLocation startLoc = D->getLocStart();
        SourceLocation endLoc = D->getLocEnd();
        const char *startBuf = SM->getCharacterData(startLoc);
        const char *endBuf = SM->getCharacterData(endLoc);
        const char *semiPtr = strchr(endBuf, ';');
        TextRewriter.RemoveText(startLoc, semiPtr-startBuf+1, TextRewriteOptions);

        break;
      }
    }

    if (!KC) return true;

    // find constructor
    CXXConstructorDecl *CCD = nullptr;
    for (auto I=D->ctor_begin(), E=D->ctor_end(); I!=E; ++I) {
      CXXConstructorDecl *CCDI = *I;

      if (CCDI->isCopyOrMoveConstructor()) continue;

      CCD = CCDI;
    }
    assert(CCD && "Couldn't find user kernel class constructor!");


    // iterate over constructor initializers
    for (auto I=CCD->param_begin(), E=CCD->param_end(); I!=E; ++I) {
      ParmVarDecl *PVD = *I;

      // constructor initializer represent the parameters for the kernel. Match
      // constructor parameter with constructor initializer since the order may
      // differ, e.g.
      // kernel(int a, int b) : b(a), a(b) {}
      for (auto II=CCD->init_begin(), EE=CCD->init_end(); II!=EE; ++II) {
        CXXCtorInitializer *CBOMI =*II;
        QualType QT;

        // CBOMI->isMemberInitializer()
        if (isa<DeclRefExpr>(CBOMI->getInit()->IgnoreParenCasts())) {
          DeclRefExpr *DRE =
            dyn_cast<DeclRefExpr>(CBOMI->getInit()->IgnoreParenCasts());

          if (DRE->getDecl() == PVD) {
            FieldDecl *FD = CBOMI->getMember();

            // reference to Image variable ?
            if (compilerClasses.isTypeOfTemplateClass(FD->getType(),
                  compilerClasses.Image)) {
              QT = compilerClasses.getFirstTemplateType(FD->getType());
              KC->addImgArg(FD, QT, FD->getName());
              //KC->addArg(nullptr, Context.IntTy, FD->getNameAsString() + "_width");
              //KC->addArg(nullptr, Context.IntTy, FD->getNameAsString() + "_height");
              //KC->addArg(nullptr, Context.IntTy, FD->getNameAsString() + "_stride");

              break;
            }

            // reference to Accessor variable ?
            if (compilerClasses.isTypeOfTemplateClass(FD->getType(),
                  compilerClasses.Accessor)) {
              QT = compilerClasses.getFirstTemplateType(FD->getType());
              KC->addImgArg(FD, QT, FD->getName());
              //KC->addArg(nullptr, Context.IntTy, FD->getNameAsString() + "_width");
              //KC->addArg(nullptr, Context.IntTy, FD->getNameAsString() + "_height");
              //KC->addArg(nullptr, Context.IntTy, FD->getNameAsString() + "_stride");

              break;
            }

            // reference to Mask variable ?
            if (compilerClasses.isTypeOfTemplateClass(FD->getType(),
                  compilerClasses.Mask)) {
              QT = compilerClasses.getFirstTemplateType(FD->getType());
              KC->addMaskArg(FD, QT, FD->getName());

              break;
            }

            // reference to Domain variable ?
            if (compilerClasses.isTypeOfClass(FD->getType(),
                                              compilerClasses.Domain)) {
              QT = Context.UnsignedCharTy;
              KC->addMaskArg(FD, QT, FD->getName());

              break;
            }

            // normal variable
            KC->addArg(FD, FD->getType(), FD->getName());

            break;
          }
        }

        // CBOMI->isBaseInitializer()
        if (isa<CXXConstructExpr>(CBOMI->getInit())) {
          CXXConstructExpr *CCE = dyn_cast<CXXConstructExpr>(CBOMI->getInit());
          assert(CCE->getNumArgs() == 1 &&
              "Kernel base class constructor requires exactly one argument!");

          if (isa<DeclRefExpr>(CCE->getArg(0))) {
            DeclRefExpr *DRE = dyn_cast<DeclRefExpr>(CCE->getArg(0));
            if (DRE->getDecl() == PVD) {
              QT = compilerClasses.getFirstTemplateType(PVD->getType());
              KC->addISArg(nullptr, QT, "Output");
              //KC->addArg(nullptr, Context.IntTy, "is_width");
              //KC->addArg(nullptr, Context.IntTy, "is_height");
              //KC->addArg(nullptr, Context.IntTy, "is_stride");

              break;
            }
          }
        }
      }
    }

    // search for kernel and reduce functions
    for (auto I=D->method_begin(), E=D->method_end(); I!=E; ++I) {
      CXXMethodDecl *FD = *I;

      // kernel function
      if (FD->getNameAsString() == "kernel") {
        // set kernel method
        KC->setKernelFunction(FD);

        // define analysis context used for different checkers
        AnalysisDeclContext AC(/* AnalysisDeclContextManager */ 0, FD);
        KernelStatistics::setAnalysisOptions(AC);

        // create kernel analysis pass, execute it and store it to kernel class
        KernelStatistics *stats = KernelStatistics::create(AC, D->getName(),
            compilerClasses);
        KC->setKernelStatistics(stats);

        continue;
      }

      // reduce function
      if (FD->getNameAsString() == "reduce") {
        // set reduce method
        KC->setReduceFunction(FD);

        continue;
      }
    }
  }

  return true;
}


bool Rewrite::VisitDeclStmt(DeclStmt *D) {
  if (!compilerClasses.HipaccEoP) return true;

  // a) convert Image declarations into memory allocations, e.g.
  //    Image<int> IN(width, height);
  //    =>
  //    int *IN = hipaccCreateMemory<int>(nullptr, width, height, &stride, padding);
  // b) convert Pyramid declarations into pyramid creation, e.g.
  //    Pyramid<int> P(IN, 3);
  //    =>
  //    Pyramid P = hipaccCreatePyramid<int>(IN, 3);
  // c) save BoundaryCondition declarations, e.g.
  //    BoundaryCondition<int> BcIN(IN, 5, 5, BOUNDARY_MIRROR);
  // d) save Accessor declarations, e.g.
  //    Accessor<int> AccIN(BcIN);
  // e) save Mask declarations, e.g.
  //    Mask<float> M(stencil);
  // f) save Domain declarations, e.g.
  //    Domain D(3, 3)
  //    Domain D(dom)
  //    Domain D(M)
  // g) save user kernel declarations, and replace it by kernel compilation
  //    for OpenCL, e.g.
  //    AddKernel K(IS, IN, OUT, 23);
  //    - create CUDA/OpenCL kernel AST by replacing accesses to Image data by
  //      global memory access and by replacing references to class member
  //      variables by kernel parameter variables.
  //    - print the CUDA/OpenCL kernel to a file.
  // h) save IterationSpace declarations, e.g.
  //    IterationSpace<int> VIS(OUT, width, height);
  for (auto DI=D->decl_begin(), DE=D->decl_end(); DI!=DE; ++DI) {
    Decl *SD = *DI;

    if (SD->getKind() == Decl::Var) {
      VarDecl *VD = dyn_cast<VarDecl>(SD);

      // found Image decl
      if (compilerClasses.isTypeOfTemplateClass(VD->getType(),
            compilerClasses.Image)) {
        CXXConstructExpr *CCE = dyn_cast<CXXConstructExpr>(VD->getInit());
        assert(CCE->getNumArgs() == 2 && "Image definition requires exactly two arguments!");

        HipaccImage *Img = new HipaccImage(Context, VD,
            compilerClasses.getFirstTemplateType(VD->getType()));

        std::string newStr;

        // get the text string for the image width
        std::string widthStr;
        llvm::raw_string_ostream WS(widthStr);
        CCE->getArg(0)->printPretty(WS, 0, PrintingPolicy(CI.getLangOpts()));

        if (compilerOptions.emitC()) {
          // check if the parameter can be resolved to a constant
          unsigned int DiagIDConstant =
            Diags.getCustomDiagID(DiagnosticsEngine::Error,
                "Constant expression for %0 parameter of Image %1 required (C/C++ only).");
          if (!CCE->getArg(0)->isEvaluatable(Context)) {
            Diags.Report(CCE->getArg(0)->getExprLoc(), DiagIDConstant)
              << "width" << Img->getName();
          }
          if (!CCE->getArg(1)->isEvaluatable(Context)) {
            Diags.Report(CCE->getArg(1)->getExprLoc(), DiagIDConstant)
              << "height" << Img->getName();
          }
          Img->setSizeX(CCE->getArg(0)->EvaluateKnownConstInt(Context).getSExtValue());
          Img->setSizeY(CCE->getArg(1)->EvaluateKnownConstInt(Context).getSExtValue());
        }


        // get the text string for the image height
        std::string heightStr;
        llvm::raw_string_ostream HS(heightStr);
        CCE->getArg(1)->printPretty(HS, 0, PrintingPolicy(CI.getLangOpts()));

        // create memory allocation string
        stringCreator.writeMemoryAllocation(VD->getName(),
            compilerClasses.getFirstTemplateType(VD->getType()).getAsString(),
            WS.str(), HS.str(), newStr, targetDevice);

        // rewrite Image definition
        // get the start location and compute the semi location.
        SourceLocation startLoc = D->getLocStart();
        const char *startBuf = SM->getCharacterData(startLoc);
        const char *semiPtr = strchr(startBuf, ';');
        TextRewriter.ReplaceText(startLoc, semiPtr-startBuf+1, newStr);

        // store Image definition
        ImgDeclMap[VD] = Img;

        break;
      }

      // found Pyramid decl
      if (compilerClasses.isTypeOfTemplateClass(VD->getType(),
            compilerClasses.Pyramid)) {
        CXXConstructExpr *CCE = dyn_cast<CXXConstructExpr>(VD->getInit());
        assert(CCE->getNumArgs() == 2 &&
               "Pyramid definition requires exactly two arguments!");

        HipaccPyramid *Pyr = new HipaccPyramid(Context, VD,
            compilerClasses.getFirstTemplateType(VD->getType()));

        std::string newStr;

        // get the text string for the pyramid image
        std::string imageStr;
        llvm::raw_string_ostream IS(imageStr);
        CCE->getArg(0)->printPretty(IS, 0, PrintingPolicy(CI.getLangOpts()));

        // get the text string for the pyramid depth
        std::string depthStr;
        llvm::raw_string_ostream DS(depthStr);
        CCE->getArg(1)->printPretty(DS, 0, PrintingPolicy(CI.getLangOpts()));

        // create memory allocation string
        stringCreator.writePyramidAllocation(VD->getName(),
            compilerClasses.getFirstTemplateType(VD->getType()).getAsString(),
            IS.str(), DS.str(), newStr);

        // rewrite Pyramid definition
        // get the start location and compute the semi location.
        SourceLocation startLoc = D->getLocStart();
        const char *startBuf = SM->getCharacterData(startLoc);
        const char *semiPtr = strchr(startBuf, ';');
        TextRewriter.ReplaceText(startLoc, semiPtr-startBuf+1, newStr);

        // store Pyramid definition
        PyrDeclMap[VD] = Pyr;

        break;
      }

      // found BoundaryCondition decl
      if (compilerClasses.isTypeOfTemplateClass(VD->getType(),
            compilerClasses.BoundaryCondition)) {
        assert(isa<CXXConstructExpr>(VD->getInit()) &&
            "Expected BoundaryCondition definition (CXXConstructExpr).");
        CXXConstructExpr *CCE = dyn_cast<CXXConstructExpr>(VD->getInit());

        HipaccBoundaryCondition *BC = nullptr;
        HipaccImage *Img = nullptr;
        HipaccPyramid *Pyr = nullptr;

        // check if the first argument is an Image
        if (isa<DeclRefExpr>(CCE->getArg(0))) {
          DeclRefExpr *DRE = dyn_cast<DeclRefExpr>(CCE->getArg(0));

          // get the Image from the DRE if we have one
          if (ImgDeclMap.count(DRE->getDecl())) {
            Img = ImgDeclMap[DRE->getDecl()];
            BC = new HipaccBoundaryCondition(Img, VD);
          }
        }

        // check if the first argument is a Pyramid call
        if (isa<CXXOperatorCallExpr>(CCE->getArg(0)) &&
            isa<DeclRefExpr>(dyn_cast<CXXOperatorCallExpr>(
                CCE->getArg(0))->getArg(0))) {
          CXXOperatorCallExpr *COCE =
            dyn_cast<CXXOperatorCallExpr>(CCE->getArg(0));
          DeclRefExpr *DRE = dyn_cast<DeclRefExpr>(COCE->getArg(0));

          // get the Pyramid from the DRE if we have one
          if (PyrDeclMap.count(DRE->getDecl())) {
            Pyr = PyrDeclMap[DRE->getDecl()];
            BC = new HipaccBoundaryCondition(Pyr, VD);

            // add call expression to pyramid argument
            unsigned int DiagIDConstant =
              Diags.getCustomDiagID(DiagnosticsEngine::Error,
                  "Missing integer literal in Pyramid %0 call expression.");
            if (!COCE->getArg(1)->isEvaluatable(Context)) {
              Diags.Report(COCE->getArg(1)->getExprLoc(), DiagIDConstant)
                << Pyr->getName();
            }
            BC->setPyramidIndex(
                COCE->getArg(1)->EvaluateKnownConstInt(Context).toString(10));
          }
        }

        assert((Img || Pyr) && "Expected first argument of BoundaryCondition "
                               "to be Image or Pyramid call.");

        // get text string for arguments, argument order is:
        // img|pyramid-call, size_x, size_y, mode
        // img|pyramid-call, size, mode
        // img|pyramid-call, mask, mode
        // img|pyramid-call, size_x, size_y, mode, const_val
        // img|pyramid-call, size, mode, const_val
        // img|pyramid-call, mask, mode, const_val
        unsigned int DiagIDConstant =
          Diags.getCustomDiagID(DiagnosticsEngine::Error,
              "Constant expression or Mask object for %ordinal0 parameter to BoundaryCondition %1 required.");
        unsigned int DiagIDNoMode =
          Diags.getCustomDiagID(DiagnosticsEngine::Error,
              "Boundary handling mode for BoundaryCondition %0 required.");
        unsigned int DiagIDWrongMode =
          Diags.getCustomDiagID(DiagnosticsEngine::Error,
              "Wrong boundary handling mode for BoundaryCondition %0 specified.");
        unsigned int DiagIDMode =
          Diags.getCustomDiagID(DiagnosticsEngine::Error,
              "Boundary handling constant for BoundaryCondition %0 required.");


        unsigned int found_size = 0;
        bool found_mode = false;
        // get kernel window size
        for (size_t i=1, e=CCE->getNumArgs(); i!=e; ++i) {
          if (found_size == 0) {
            // check if the parameter is a Mask reference
            if (isa<DeclRefExpr>(CCE->getArg(i)->IgnoreParenCasts())) {
              DeclRefExpr *DRE =
                dyn_cast<DeclRefExpr>(CCE->getArg(i)->IgnoreParenCasts());

              // get the Mask from the DRE if we have one
              if (MaskDeclMap.count(DRE->getDecl())) {
                HipaccMask *Mask = MaskDeclMap[DRE->getDecl()];
                BC->setSizeX(Mask->getSizeX());
                BC->setSizeY(Mask->getSizeY());
                found_size++;
                found_size++;

                continue;
              }
            }

            // check if the parameter can be resolved to a constant
            if (!CCE->getArg(i)->isEvaluatable(Context)) {
              Diags.Report(CCE->getArg(i)->getExprLoc(), DiagIDConstant)
                << (int)i+1 << VD->getName();
            }
            BC->setSizeX(CCE->getArg(i)->EvaluateKnownConstInt(Context).getSExtValue());
            found_size++;
          } else {
            // check if the parameter specifies the boundary mode
            if (isa<DeclRefExpr>(CCE->getArg(i)->IgnoreParenCasts())) {
              DeclRefExpr *DRE =
                dyn_cast<DeclRefExpr>(CCE->getArg(i)->IgnoreParenCasts());
              // boundary mode found
              if (DRE->getDecl()->getKind() == Decl::EnumConstant &&
                  DRE->getDecl()->getType().getAsString() ==
                  "enum hipacc::hipaccBoundaryMode") {
                int64_t mode =
                  CCE->getArg(i)->EvaluateKnownConstInt(Context).getSExtValue();
                switch (mode) {
                  case BOUNDARY_UNDEFINED:
                  case BOUNDARY_CLAMP:
                  case BOUNDARY_REPEAT:
                  case BOUNDARY_MIRROR:
                    BC->setBoundaryHandling((BoundaryMode)mode);
                    break;
                  case BOUNDARY_CONSTANT:
                    BC->setBoundaryHandling((BoundaryMode)mode);
                    if (CCE->getNumArgs() != i+2) {
                      Diags.Report(CCE->getArg(i)->getExprLoc(), DiagIDMode) <<
                        VD->getName();
                    }
                    // check if the parameter can be resolved to a constant
                    if (!CCE->getArg(i+1)->isEvaluatable(Context)) {
                      Diags.Report(CCE->getArg(i)->getExprLoc(), DiagIDConstant)
                        << (int)i+2 << VD->getName();
                    } else {
                      Expr::EvalResult val;
                      CCE->getArg(i+1)->EvaluateAsRValue(val, Context);
                      BC->setConstVal(val.Val, Context);
                      i++;
                    }
                    break;
                  default:
                    BC->setBoundaryHandling(BOUNDARY_UNDEFINED);
                    llvm::errs() << "invalid boundary handling mode specified, using default mode!\n";
                }
                found_mode = true;

                // if only size is specified, set size_x and size_y to size
                if (found_size == 1) {
                  BC->setSizeY(BC->getSizeX());
                  found_size++;
                }

                continue;
              }
            }

            if (found_size >= 2) {
              if (found_mode) {
                Diags.Report(CCE->getArg(i)->getExprLoc(), DiagIDWrongMode) <<
                  VD->getName();
              } else {
                Diags.Report(CCE->getArg(i)->getExprLoc(), DiagIDNoMode) <<
                  VD->getName();
              }
            }

            // check if the parameter can be resolved to a constant
            if (!CCE->getArg(i)->isEvaluatable(Context)) {
              Diags.Report(CCE->getArg(i)->getExprLoc(), DiagIDConstant)
                << (int)i+1 << VD->getName();
            }
            BC->setSizeY(CCE->getArg(i)->EvaluateKnownConstInt(Context).getSExtValue());
            found_size++;
          }
        }

        // store BoundaryCondition
        BCDeclMap[VD] = BC;

        // remove BoundaryCondition definition
        TextRewriter.RemoveText(D->getSourceRange());

        break;
      }

      // found Accessor decl
      if (compilerClasses.isTypeOfTemplateClass(VD->getType(),
            compilerClasses.Accessor) ||
          compilerClasses.isTypeOfTemplateClass(VD->getType(),
            compilerClasses.AccessorNN) ||
          compilerClasses.isTypeOfTemplateClass(VD->getType(),
            compilerClasses.AccessorLF) ||
          compilerClasses.isTypeOfTemplateClass(VD->getType(),
            compilerClasses.AccessorCF) ||
          compilerClasses.isTypeOfTemplateClass(VD->getType(),
            compilerClasses.AccessorL3)) {
        assert(VD->hasInit() && "Currently only Accessor definitions are supported, no declarations!");
        assert(isa<CXXConstructExpr>(VD->getInit()) &&
            "Currently only Accessor definitions are supported, no declarations!");
        CXXConstructExpr *CCE = dyn_cast<CXXConstructExpr>(VD->getInit());

        std::string newStr;
        HipaccAccessor *Acc = nullptr;
        HipaccBoundaryCondition *BC = nullptr;
        HipaccImage *Img = nullptr;
        HipaccPyramid *Pyr = nullptr;

        // check if the first argument is an Image
        if (isa<DeclRefExpr>(CCE->getArg(0))) {
          DeclRefExpr *DRE = dyn_cast<DeclRefExpr>(CCE->getArg(0));

          // get the BoundaryCondition from the DRE if we have one
          if (BCDeclMap.count(DRE->getDecl())) {
            BC = BCDeclMap[DRE->getDecl()];
          }

          // in case we have no BoundaryCondition, check if an Image is
          // specified and construct a BoundaryCondition
          if (!BC && ImgDeclMap.count(DRE->getDecl())) {
            Img = ImgDeclMap[DRE->getDecl()];
            BC = new HipaccBoundaryCondition(Img, VD);
            BC->setSizeX(1);
            BC->setSizeY(1);
            BC->setBoundaryHandling(BOUNDARY_CLAMP);

            // Fixme: store BoundaryCondition???
            BCDeclMap[VD] = BC;
          }
        }

        // check if the first argument is a Pyramid call
        if (isa<CXXOperatorCallExpr>(CCE->getArg(0)) &&
            isa<DeclRefExpr>(dyn_cast<CXXOperatorCallExpr>(
                CCE->getArg(0))->getArg(0))) {
          DeclRefExpr *DRE = dyn_cast<DeclRefExpr>(
              dyn_cast<CXXOperatorCallExpr>(CCE->getArg(0))->getArg(0));

          // get the Pyramid from the DRE if we have one
          if (PyrDeclMap.count(DRE->getDecl())) {
            Pyr = PyrDeclMap[DRE->getDecl()];
            BC = new HipaccBoundaryCondition(Pyr, VD);
            BC->setSizeX(1);
            BC->setSizeY(1);
            BC->setBoundaryHandling(BOUNDARY_CLAMP);

            // Fixme: store BoundaryCondition???
            BCDeclMap[VD] = BC;
          }
        }

        assert(BC && "Expected BoundaryCondition, Image or Pyramid call as "
                     "first argument to Accessor.");

        InterpolationMode mode;
        if (compilerClasses.isTypeOfTemplateClass(VD->getType(),
              compilerClasses.Accessor)) mode = InterpolateNO;
        else if (compilerClasses.isTypeOfTemplateClass(VD->getType(),
              compilerClasses.AccessorNN)) mode = InterpolateNN;
        else if (compilerClasses.isTypeOfTemplateClass(VD->getType(),
              compilerClasses.AccessorLF)) mode = InterpolateLF;
        else if (compilerClasses.isTypeOfTemplateClass(VD->getType(),
              compilerClasses.AccessorCF)) mode = InterpolateCF;
        else mode = InterpolateL3;

        Acc = new HipaccAccessor(BC, mode, VD);

        // get text string for arguments
        std::string Parms(Acc->getImage()->getName());

        if (Pyr) {
          // add call expression to pyramid argument
          IntegerLiteral *IL = nullptr;
          UnaryOperator *UO = nullptr;

          if (isa<IntegerLiteral>(dyn_cast<CXXOperatorCallExpr>(
                  CCE->getArg(0))->getArg(1))) {
            IL = dyn_cast<IntegerLiteral>(dyn_cast<CXXOperatorCallExpr>(
                     CCE->getArg(0))->getArg(1));
          } else if (isa<UnaryOperator>(dyn_cast<CXXOperatorCallExpr>(
                         CCE->getArg(0))->getArg(1))) {
            UO = dyn_cast<UnaryOperator>(dyn_cast<CXXOperatorCallExpr>(
                     CCE->getArg(0))->getArg(1));
            // only support unary operators '+' and '-'
            if (UO && (UO->getOpcode() == UO_Plus ||
                       UO->getOpcode() == UO_Minus)) {
              IL = dyn_cast<IntegerLiteral>(UO->getSubExpr());
            }
          }

          assert(IL && "Missing integer literal in pyramid call expression.");

          std::stringstream LSS;
          if (UO && UO->getOpcode() == UO_Minus) {
            LSS << "-";
          }
          LSS << *(IL->getValue().getRawData());
          Parms += "(" + LSS.str() + ")";
        } else {
          if (BC->isPyramid()) {
            // add call expression to pyramid argument (from boundary condition)
            Parms += "(" + BC->getPyramidIndex() + ")";
          }
        }

        // img|bc|pyramid-call
        // img|bc|pyramid-call, width, height, xf, yf
        if (CCE->getNumArgs()<4) Acc->setNoCrop();

        // get text string for arguments, argument order is:
        for (size_t i=1; i<CCE->getNumArgs(); ++i) {
          std::string Str;
          llvm::raw_string_ostream SS(Str);

          CCE->getArg(i)->printPretty(SS, 0, PrintingPolicy(CI.getLangOpts()));
          Parms += ", " + SS.str();
        }

        newStr += "HipaccAccessor " + Acc->getName() + "(" + Parms + ");";

        // replace Accessor decl by variables for width/height and offsets
        // get the start location and compute the semi location.
        SourceLocation startLoc = D->getLocStart();
        const char *startBuf = SM->getCharacterData(startLoc);
        const char *semiPtr = strchr(startBuf, ';');
        TextRewriter.ReplaceText(startLoc, semiPtr-startBuf+1, newStr);

        // store Accessor definition
        AccDeclMap[VD] = Acc;

        break;
      }

      // found IterationSpace decl
      if (compilerClasses.isTypeOfTemplateClass(VD->getType(),
            compilerClasses.IterationSpace)) {
        assert(isa<CXXConstructExpr>(VD->getInit()) &&
            "Expected IterationSpace definition (CXXConstructExpr).");
        CXXConstructExpr *CCE = dyn_cast<CXXConstructExpr>(VD->getInit());

        std::string newStr;
        std::stringstream LSS;
        LSS << isLiteralCount++;

        HipaccIterationSpace *IS = nullptr;
        HipaccImage *Img = nullptr;
        HipaccPyramid *Pyr = nullptr;

        // check if the first argument is an Image
        if (isa<DeclRefExpr>(CCE->getArg(0))) {
          DeclRefExpr *DRE = dyn_cast<DeclRefExpr>(CCE->getArg(0));

          // get the Image from the DRE if we have one
          if (ImgDeclMap.count(DRE->getDecl())) {
            Img = ImgDeclMap[DRE->getDecl()];
            IS = new HipaccIterationSpace(Img, VD);
          }
        }

        // check if the first argument is a Pyramid call
        if (isa<CXXOperatorCallExpr>(CCE->getArg(0)) &&
            isa<DeclRefExpr>(dyn_cast<CXXOperatorCallExpr>(
                CCE->getArg(0))->getArg(0))) {
          DeclRefExpr *DRE = dyn_cast<DeclRefExpr>(
              dyn_cast<CXXOperatorCallExpr>(CCE->getArg(0))->getArg(0));

          // get the Pyramid from the DRE if we have one
          if (PyrDeclMap.count(DRE->getDecl())) {
            Pyr = PyrDeclMap[DRE->getDecl()];
            IS = new HipaccIterationSpace(Pyr, VD);
          }
        }

        assert((Img || Pyr) && "Expected first argument of IterationSpace to "
                               "be Image or Pyramid call.");

        // get text string for arguments
        std::string Parms(IS->getImage()->getName());

        if (Pyr) {
          // add call expression to pyramid argument
          IntegerLiteral *IL = nullptr;
          UnaryOperator *UO = nullptr;

          if (isa<IntegerLiteral>(dyn_cast<CXXOperatorCallExpr>(
                  CCE->getArg(0))->getArg(1))) {
            IL = dyn_cast<IntegerLiteral>(dyn_cast<CXXOperatorCallExpr>(
                     CCE->getArg(0))->getArg(1));
          } else if (isa<UnaryOperator>(dyn_cast<CXXOperatorCallExpr>(
                         CCE->getArg(0))->getArg(1))) {
            UO = dyn_cast<UnaryOperator>(dyn_cast<CXXOperatorCallExpr>(
                     CCE->getArg(0))->getArg(1));
            // only support unary operators '+' and '-'
            if (UO && (UO->getOpcode() == UO_Plus ||
                       UO->getOpcode() == UO_Minus)) {
              IL = dyn_cast<IntegerLiteral>(UO->getSubExpr());
            }
          }

          assert(IL && "Missing integer literal in pyramid call expression.");

          std::stringstream LSS;
          if (UO && UO->getOpcode() == UO_Minus) {
            LSS << "-";
          }
          LSS << *(IL->getValue().getRawData());
          Parms += "(" + LSS.str() + ")";
        }

        // img[, is_width, is_height[, offset_x, offset_y]]
        if (CCE->getNumArgs()<4) IS->setNoCrop();

        // get text string for arguments, argument order is:
        for (size_t i=1; i<CCE->getNumArgs(); ++i) {
          std::string Str;
          llvm::raw_string_ostream SS(Str);

          CCE->getArg(i)->printPretty(SS, 0, PrintingPolicy(CI.getLangOpts()));
          Parms += ", " + SS.str();
        }

        newStr += "HipaccAccessor " + IS->getName() + "(" + Parms + ");";

        // store IterationSpace
        ISDeclMap[VD] = IS;

        // replace iteration space decl by variables for width/height, and
        // offset
        // get the start location and compute the semi location.
        SourceLocation startLoc = D->getLocStart();
        const char *startBuf = SM->getCharacterData(startLoc);
        const char *semiPtr = strchr(startBuf, ';');
        TextRewriter.ReplaceText(startLoc, semiPtr-startBuf+1, newStr);

        break;
      }

      HipaccMask *Mask = nullptr;
      // found Mask decl
      if (compilerClasses.isTypeOfTemplateClass(VD->getType(),
            compilerClasses.Mask)) {
        assert(VD->hasInit() &&
               "Currently only Mask definitions are supported, no "
               "declarations!");
        assert(isa<CXXConstructExpr>(VD->getInit()) &&
               "Currently only Mask definitions are supported, no "
               "declarations!");

        CXXConstructExpr *CCE = dyn_cast<CXXConstructExpr>(VD->getInit());
        assert((CCE->getNumArgs() == 1) &&
               "Mask definition requires exactly one argument!");

        QualType QT = compilerClasses.getFirstTemplateType(VD->getType());
        Mask = new HipaccMask(VD, QT, HipaccMask::Mask);

        // get initializer
        DeclRefExpr *DRE = dyn_cast<DeclRefExpr>(CCE->getArg(0)->IgnoreImpCasts());
        assert(DRE && "Mask must be initialized using a variable");
        VarDecl *V = dyn_cast_or_null<VarDecl>(DRE->getDecl());
        assert(V && "Mask must be initialized using a variable");
        bool isMaskConstant = V->getType().isConstant(Context);

        // extract size_y and size_x from type
        const ConstantArrayType *Array =
          Context.getAsConstantArrayType(V->getType());
        Mask->setSizeY(Array->getSize().getSExtValue());
        Array = Context.getAsConstantArrayType(Array->getElementType());
        Mask->setSizeX(Array->getSize().getSExtValue());

        // loop over initializers and check if each initializer is a constant
        if (isMaskConstant && isa<InitListExpr>(V->getInit())) {
          InitListExpr *ILEY = dyn_cast<InitListExpr>(V->getInit());
          Mask->setInitList(ILEY);
          for (size_t y=0; y<ILEY->getNumInits(); ++y) {
            InitListExpr *ILEX = dyn_cast<InitListExpr>(ILEY->getInit(y));
            for (size_t x=0; x<ILEX->getNumInits(); ++x) {
              if (!ILEX->getInit(x)->isConstantInitializer(Context, false)) {
                isMaskConstant = false;
                break;
              }
            }
          }
        }
        Mask->setIsConstant(isMaskConstant);
        Mask->setHostMemName(V->getName());
      }

      HipaccMask *Domain = nullptr;
      // found Domain decl
      if (compilerClasses.isTypeOfClass(VD->getType(),
                                        compilerClasses.Domain)) {
        assert(VD->hasInit() &&
               "Currently only Domain definitions are supported, no "
               "declarations!");
        assert(isa<CXXConstructExpr>(VD->getInit()) &&
               "Currently only Domain definitions are supported, no "
               "declarations!");

        Domain = new HipaccMask(VD, Context.UnsignedCharTy,
                                            HipaccMask::Domain);

        CXXConstructExpr *CCE = dyn_cast<CXXConstructExpr>(VD->getInit());
        if (CCE->getNumArgs() == 1) {
          // get initializer
          DeclRefExpr *DRE =
              dyn_cast<DeclRefExpr>(CCE->getArg(0)->IgnoreImpCasts());
          assert(DRE && "Domain must be initialized using a variable");
          VarDecl *V = dyn_cast_or_null<VarDecl>(DRE->getDecl());
          assert(V && "Domain must be initialized using a variable");

          if (compilerClasses.isTypeOfTemplateClass(DRE->getType(),
                                                    compilerClasses.Mask)) {
            // copy from mask
            HipaccMask *Mask = MaskDeclMap[DRE->getDecl()];
            assert(Mask && "Mask to copy from was not declared");

            int size_x = Mask->getSizeX();
            int size_y = Mask->getSizeY();

            Domain->setSizeX(size_x);
            Domain->setSizeY(size_y);

            Domain->setIsConstant(Mask->isConstant());

            if (Mask->isConstant()) {
              for (int x = 0; x < size_x; ++x) {
                for (int y = 0; y < size_y; ++y) {
                  // copy values to compiler internal data structure
                  Expr::EvalResult val;
                  Mask->getInitExpr(x, y)->EvaluateAsRValue(val, Context);
                  if (val.Val.isInt()) {
                    Domain->setDomainDefined(x, y,
                        val.Val.getInt().getSExtValue() != 0);
                  } else if (val.Val.isFloat()) {
                    Domain->setDomainDefined(x, y,
                        !val.Val.getFloat().isZero());
                  } else {
                    assert(false && "Only builtin integer and floating point "
                                    "literals supported in copy Mask");
                  }
                }
              }
            } else {
              Domain->setCopyMask(Mask);
            }
          } else {
            // get from array
            bool isDomainConstant = V->getType().isConstant(Context);

            // extract size_y and size_x from type
            const ConstantArrayType *Array =
                Context.getAsConstantArrayType(V->getType());
            Domain->setSizeY(Array->getSize().getSExtValue());
            Array = Context.getAsConstantArrayType(Array->getElementType());
            Domain->setSizeX(Array->getSize().getSExtValue());

            // loop over initializers and check if each initializer is a
            // constant
            if (isDomainConstant && isa<InitListExpr>(V->getInit())) {
              InitListExpr *ILEY = dyn_cast<InitListExpr>(V->getInit());
              Domain->setInitList(ILEY);
              for (size_t y=0; y<ILEY->getNumInits(); ++y) {
                InitListExpr *ILEX = dyn_cast<InitListExpr>(ILEY->getInit(y));
                for (size_t x=0; x<ILEX->getNumInits(); ++x) {
                  if (!ILEX->getInit(x)->isConstantInitializer(Context, false)){
                    isDomainConstant = false;
                    break;
                  }
                  // copy values to compiler internal data structure
                  Expr *E = ILEX->getInit(x)->IgnoreParenCasts();
                  if (isa<IntegerLiteral>(E)) {
                    Domain->setDomainDefined(x, y,
                        dyn_cast<IntegerLiteral>(E)->getValue() != 0);
                  } else {
                    assert(false &&
                           "Expected integer literal in domain initializer");
                  }
                }
              }
            }
            Domain->setIsConstant(isDomainConstant);
            Domain->setHostMemName(V->getName());
          }
        } else if (CCE->getNumArgs() == 2) {
          unsigned int DiagIDConstant =
              Diags.getCustomDiagID(DiagnosticsEngine::Error,
                  "Constant expression for %ordinal0 parameter to %1 %2 "
                  "required.");

          // check if the parameters can be resolved to a constant
          Expr *Arg0 = CCE->getArg(0);
          if (!Arg0->isEvaluatable(Context)) {
            Diags.Report(Arg0->getExprLoc(), DiagIDConstant)
              << 1 << "Domain" << VD->getName();
          }
          Domain->setSizeX(Arg0->EvaluateKnownConstInt(Context).getSExtValue());

          Expr *Arg1 = CCE->getArg(1);
          if (!Arg1->isEvaluatable(Context)) {
            Diags.Report(Arg1->getExprLoc(), DiagIDConstant)
              << 2 << "Domain" << VD->getName();
          }
          Domain->setSizeY(Arg1->EvaluateKnownConstInt(Context).getSExtValue());
          Domain->setIsConstant(true);
        } else {
          assert(false && "Domain definition requires exactly two arguments "
              "type constant integer or a single argument of type uchar[][] or "
              "Mask!");
        }
      }

      if (Mask || Domain) {
        HipaccMask *Buf = Domain ? Domain : Mask;

        std::string newStr;
        if (!Buf->isConstant() && !compilerOptions.emitCUDA()) {
          // create Buffer for Mask
          stringCreator.writeMemoryAllocationConstant(Buf->getName(),
              Buf->getTypeStr(), Buf->getSizeXStr(), Buf->getSizeYStr(),
            newStr);

          if (Buf->hasCopyMask()) {
            // create Domain from Mask and upload to Buffer
            stringCreator.writeMemoryTransferDomainFromMask(Buf,
                Buf->getCopyMask(), newStr);
          } else {
            // upload Mask to Buffer
            stringCreator.writeMemoryTransferSymbol(Buf, Buf->getHostMemName(),
                HOST_TO_DEVICE, newStr);
          }
        }

        // replace Mask declaration by Buffer allocation
        // get the start location and compute the semi location.
        SourceLocation startLoc = D->getLocStart();
        const char *startBuf = SM->getCharacterData(startLoc);
        const char *semiPtr = strchr(startBuf, ';');
        TextRewriter.ReplaceText(startLoc, semiPtr-startBuf+1, newStr);

        // store Mask definition
        MaskDeclMap[VD] = Buf;

        break;
      }

      // found Kernel decl
      if (VD->getType()->getTypeClass() == Type::Record) {
        const RecordType *RT = cast<RecordType>(VD->getType());

        // get Kernel Class
        if (KernelClassDeclMap.count(RT->getDecl())) {
          HipaccKernelClass *KC = KernelClassDeclMap[RT->getDecl()];
          HipaccKernel *K = new HipaccKernel(Context, VD, KC, compilerOptions);
          KernelDeclMap[VD] = K;

          // remove kernel declaration
          TextRewriter.RemoveText(D->getSourceRange());

          // create map between Image or Accessor instances and kernel
          // variables; replace image instances by accessors with undefined
          // boundary handling
          assert(VD->hasInit() && "Currently only Kernel definitions are supported, no declarations!");
          assert(isa<CXXConstructExpr>(VD->getInit()) &&
              "Currently only Image definitions are supported, no declarations!");
          CXXConstructExpr *CCE = dyn_cast<CXXConstructExpr>(VD->getInit());

          unsigned int num_img = 0, num_mask = 0;
          SmallVector<FieldDecl *, 16> imgFields = KC->getImgFields();
          SmallVector<FieldDecl *, 16> maskFields = KC->getMaskFields();
          for (size_t i=0; i<CCE->getNumArgs(); ++i) {
            if (isa<DeclRefExpr>(CCE->getArg(i)->IgnoreParenCasts())) {
              DeclRefExpr *DRE =
                dyn_cast<DeclRefExpr>(CCE->getArg(i)->IgnoreParenCasts());

              // check if we have an Image
              if (ImgDeclMap.count(DRE->getDecl())) {
                unsigned int DiagIDImage =
                  Diags.getCustomDiagID(DiagnosticsEngine::Error,
                      "Images are not supported within kernels, use Accessors instead:");
                Diags.Report(DRE->getLocation(), DiagIDImage);
              }

              // check if we have an Accessor
              if (AccDeclMap.count(DRE->getDecl())) {
                K->insertMapping(imgFields.data()[num_img],
                    AccDeclMap[DRE->getDecl()]);
                num_img++;
                continue;
              }

              // check if we have a Mask or Domain
              if (MaskDeclMap.count(DRE->getDecl())) {
                K->insertMapping(maskFields.data()[num_mask],
                    MaskDeclMap[DRE->getDecl()]);
                num_mask++;
                continue;
              }

              // check if we have an IterationSpace
              if (ISDeclMap.count(DRE->getDecl())) {
                K->setIterationSpace(ISDeclMap[DRE->getDecl()]);
                continue;
              }
            }
          }

          // set kernel configuration
          setKernelConfiguration(KC, K);

          // kernel declaration
          FunctionDecl *kernelDecl = createFunctionDecl(Context,
              Context.getTranslationUnitDecl(), K->getKernelName(), Context.VoidTy,
              K->getArgTypes(Context, compilerOptions.getTargetCode()),
              K->getDeviceArgNames());

          // write CUDA/OpenCL kernel function to file clone old body,
          // replacing member variables
          ASTTranslate *Hipacc = new ASTTranslate(Context, kernelDecl, K, KC,
              builtins, compilerOptions);
          Stmt *kernelStmts =
            Hipacc->Hipacc(KC->getKernelFunction()->getBody());
          kernelDecl->setBody(kernelStmts);
          K->printStats();

          #ifdef USE_POLLY
          if (!compilerOptions.exploreConfig() && compilerOptions.emitC()) {
            llvm::errs() << "\nPassing the following function to Polly:\n";
            kernelDecl->print(llvm::errs(), Context.getPrintingPolicy());
            llvm::errs() << "\n";

            Polly *polly_analysis = new Polly(Context, CI, kernelDecl);
            polly_analysis->analyzeKernel();
          }
          #endif

          // write kernel to file
          printKernelFunction(kernelDecl, KC, K, K->getFileName(), true);

          break;
        }
      }
    }
  }

  return true;
}


bool Rewrite::VisitFunctionDecl(FunctionDecl *D) {
  if (D->isMain()) {
    assert(D->getBody() && "main function has no body.");
    assert(isa<CompoundStmt>(D->getBody()) && "CompoundStmt for main body expected.");
    mainFD = D;
  }

  return true;
}


bool Rewrite::VisitCXXOperatorCallExpr(CXXOperatorCallExpr *E) {
  if (!compilerClasses.HipaccEoP) return true;

  // convert overloaded operator 'operator=' function into memory transfer,
  // a) Img = host_array;
  // b) Pyr(x) = host_array;
  // c) Img = Img;
  // d) Img = Acc;
  // e) Img = Pyr(x);
  // f) Acc = Acc;
  // g) Acc = Img;
  // h) Acc = Pyr(x);
  // i) Pyr(x) = Img;
  // j) Pyr(x) = Acc;
  // k) Pyr(x) = Pyr(x);
  // l) Domain(x, y) = literal; (return type of ()-operator is DomainSetter)
  if (E->getOperator() == OO_Equal) {
    if (E->getNumArgs() != 2) return true;

    HipaccImage *ImgLHS = nullptr, *ImgRHS = nullptr;
    HipaccAccessor *AccLHS = nullptr, *AccRHS = nullptr;
    HipaccPyramid *PyrLHS = nullptr, *PyrRHS = nullptr;
    HipaccMask *DomLHS = nullptr;
    std::string PyrIdxLHS, PyrIdxRHS;
    unsigned int DomIdxX, DomIdxY;

    // check first parameter
    if (isa<DeclRefExpr>(E->getArg(0)->IgnoreParenCasts())) {
      DeclRefExpr *DRE_LHS =
        dyn_cast<DeclRefExpr>(E->getArg(0)->IgnoreParenCasts());

      // check if we have an Image at the LHS
      if (ImgDeclMap.count(DRE_LHS->getDecl())) {
        ImgLHS = ImgDeclMap[DRE_LHS->getDecl()];
      }
      // check if we have an Accessor at the LHS
      if (AccDeclMap.count(DRE_LHS->getDecl())) {
        AccLHS = AccDeclMap[DRE_LHS->getDecl()];
      }
    } else if (isa<CXXOperatorCallExpr>(E->getArg(0))) {
      CXXOperatorCallExpr *CE = dyn_cast<CXXOperatorCallExpr>(E->getArg(0));

      // check if we have an Pyramid or Domain call at the LHS
      if (isa<DeclRefExpr>(CE->getArg(0))) {
        DeclRefExpr *DRE = dyn_cast<DeclRefExpr>(CE->getArg(0));

        // get the Pyramid from the DRE if we have one
        if (PyrDeclMap.count(DRE->getDecl())) {
          PyrLHS = PyrDeclMap[DRE->getDecl()];

          // add call expression to pyramid argument
          unsigned int DiagIDConstant =
            Diags.getCustomDiagID(DiagnosticsEngine::Error,
                "Missing integer literal in Pyramid %0 call expression.");
          if (!CE->getArg(1)->isEvaluatable(Context)) {
            Diags.Report(CE->getArg(1)->getExprLoc(), DiagIDConstant)
              << PyrLHS->getName();
          }
          PyrIdxLHS =
            CE->getArg(1)->EvaluateKnownConstInt(Context).toString(10);
        } else if (MaskDeclMap.count(DRE->getDecl())) {
          DomLHS = MaskDeclMap[DRE->getDecl()];

          assert(DomLHS->isConstant() &&
                 "Setting domain values only supported for constant Domains");

          unsigned int DiagIDConstant =
            Diags.getCustomDiagID(DiagnosticsEngine::Error,
                "Integer expression in Domain %0 is non-const.");
          if (!CE->getArg(1)->isEvaluatable(Context)) {
            Diags.Report(CE->getArg(1)->getExprLoc(), DiagIDConstant)
              << DomLHS->getName();
          }
          if (!CE->getArg(2)->isEvaluatable(Context)) {
            Diags.Report(CE->getArg(2)->getExprLoc(), DiagIDConstant)
              << DomLHS->getName();
          }
          DomIdxX = DomLHS->getSizeX()/2 +
            CE->getArg(1)->EvaluateKnownConstInt(Context).getSExtValue();
          DomIdxY = DomLHS->getSizeY()/2 +
            CE->getArg(2)->EvaluateKnownConstInt(Context).getSExtValue();
        }
      }
    }

    // check second parameter
    if (isa<DeclRefExpr>(E->getArg(1)->IgnoreParenCasts())) {
      DeclRefExpr *DRE_RHS =
        dyn_cast<DeclRefExpr>(E->getArg(1)->IgnoreParenCasts());

      // check if we have an Image at the RHS
      if (ImgDeclMap.count(DRE_RHS->getDecl())) {
        ImgRHS = ImgDeclMap[DRE_RHS->getDecl()];
      }
      // check if we have an Accessor at the RHS
      if (AccDeclMap.count(DRE_RHS->getDecl())) {
        AccRHS = AccDeclMap[DRE_RHS->getDecl()];
      }
    } else if (isa<CXXOperatorCallExpr>(E->getArg(1))) {
      CXXOperatorCallExpr *CE = dyn_cast<CXXOperatorCallExpr>(E->getArg(1));

      // check if we have an Pyramid call at the RHS
      if (isa<DeclRefExpr>(CE->getArg(0))) {
        DeclRefExpr *DRE = dyn_cast<DeclRefExpr>(CE->getArg(0));

        // get the Pyramid from the DRE if we have one
        if (PyrDeclMap.count(DRE->getDecl())) {
          PyrRHS = PyrDeclMap[DRE->getDecl()];

          // add call expression to pyramid argument
          unsigned int DiagIDConstant =
            Diags.getCustomDiagID(DiagnosticsEngine::Error,
                "Missing integer literal in Pyramid %0 call expression.");
          if (!CE->getArg(1)->isEvaluatable(Context)) {
            Diags.Report(CE->getArg(1)->getExprLoc(), DiagIDConstant)
              << PyrRHS->getName();
          }
          PyrIdxRHS =
            CE->getArg(1)->EvaluateKnownConstInt(Context).toString(10);
        }
      }
    } else if (DomLHS) {
      // check for RHS literal to set domain value
      Expr *arg = E->getArg(1)->IgnoreImpCasts();

      assert(isa<IntegerLiteral>(arg) &&
             "RHS argument for setting specific domain value must be integer "
             "literal");

      // set domain value
      DomLHS->setDomainDefined(DomIdxX, DomIdxY,
          dyn_cast<IntegerLiteral>(arg)->getValue() != 0);

      SourceLocation startLoc = E->getLocStart();
      const char *startBuf = SM->getCharacterData(startLoc);
      const char *semiPtr = strchr(startBuf, ';');
      TextRewriter.RemoveText(startLoc, semiPtr-startBuf+1);

      return true;
    }

    if (ImgLHS || AccLHS || PyrLHS) {
      std::string newStr;

      if (ImgLHS && ImgRHS) {
        // Img1 = Img2;
        stringCreator.writeMemoryTransfer(ImgLHS, ImgRHS->getName(),
            DEVICE_TO_DEVICE, newStr);
      } else if (ImgLHS && AccRHS) {
        // Img1 = Acc2;
        stringCreator.writeMemoryTransferRegion("HipaccAccessor(" +
            ImgLHS->getName() + ")", AccRHS->getName(), newStr);
      } else if (ImgLHS && PyrRHS) {
        // Img1 = Pyr2(x2);
        stringCreator.writeMemoryTransfer(ImgLHS,
            PyrRHS->getName() + "(" + PyrIdxRHS + ")",
            DEVICE_TO_DEVICE, newStr);
      } else if (AccLHS && ImgRHS) {
        // Acc1 = Img2;
        stringCreator.writeMemoryTransferRegion(AccLHS->getName(),
            "HipaccAccessor(" + ImgRHS->getName() + ")", newStr);
      } else if (AccLHS && AccRHS) {
        // Acc1 = Acc2;
        stringCreator.writeMemoryTransferRegion(AccLHS->getName(),
            AccRHS->getName(), newStr);
      } else if (AccLHS && PyrRHS) {
        // Acc1 = Pyr2(x2);
        stringCreator.writeMemoryTransferRegion(AccLHS->getName(),
            "HipaccAccessor(" + PyrRHS->getName() + "(" + PyrIdxRHS + "))",
            newStr);
      } else if (PyrLHS && ImgRHS) {
        // Pyr1(x1) = Img2
        stringCreator.writeMemoryTransfer(PyrLHS, PyrIdxLHS, ImgRHS->getName(),
            DEVICE_TO_DEVICE, newStr);
      } else if (PyrLHS && AccRHS) {
        // Pyr1(x1) = Acc2
        stringCreator.writeMemoryTransferRegion(
            "HipaccAccessor(" + PyrLHS->getName() + "(" + PyrIdxLHS + "))",
            AccRHS->getName(), newStr);
      } else if (PyrLHS && PyrRHS) {
        // Pyr1(x1) = Pyr2(x2)
        stringCreator.writeMemoryTransfer(PyrLHS, PyrIdxLHS,
            PyrRHS->getName() + "(" + PyrIdxRHS + ")",
            DEVICE_TO_DEVICE, newStr);
      } else {
        bool write_pointer = true;
        // Img1 = Img2.getData();
        // Img1 = Pyr2(x2).getData();
        // Pyr1(x1) = Img2.getData();
        // Pyr1(x1) = Pyr2(x2).getData();
        if (isa<CXXMemberCallExpr>(E->getArg(1))) {
          CXXMemberCallExpr *MCE = dyn_cast<CXXMemberCallExpr>(E->getArg(1));

          // match only getData calls to Image instances
          if (MCE->getDirectCallee()->getNameAsString() == "getData") {
            if (isa<DeclRefExpr>(MCE->getImplicitObjectArgument())) {
              DeclRefExpr *DRE =
                dyn_cast<DeclRefExpr>(MCE->getImplicitObjectArgument());

              // check if we have an Image
              if (ImgDeclMap.count(DRE->getDecl())) {
                HipaccImage *Img = ImgDeclMap[DRE->getDecl()];

                if (PyrLHS) {
                  stringCreator.writeMemoryTransfer(PyrLHS, PyrIdxLHS,
                      Img->getName(), DEVICE_TO_DEVICE, newStr);
                } else {
                  stringCreator.writeMemoryTransfer(ImgLHS, Img->getName(),
                      DEVICE_TO_DEVICE, newStr);
                }
                write_pointer = false;
              }
            } else if (isa<CXXOperatorCallExpr>(
                           MCE->getImplicitObjectArgument())) {
              CXXOperatorCallExpr *CE = dyn_cast<CXXOperatorCallExpr>(
                                            MCE->getImplicitObjectArgument());

              // check if we have an Pyramid call
              if (isa<DeclRefExpr>(CE->getArg(0))) {
                DeclRefExpr *DRE = dyn_cast<DeclRefExpr>(CE->getArg(0));

                // get the Pyramid from the DRE if we have one
                if (PyrDeclMap.count(DRE->getDecl())) {
                  HipaccPyramid *Pyr = PyrDeclMap[DRE->getDecl()];

                  // add call expression to pyramid argument
                  unsigned int DiagIDConstant =
                    Diags.getCustomDiagID(DiagnosticsEngine::Error,
                        "Missing integer literal in Pyramid %0 call expression.");
                  if (!CE->getArg(1)->isEvaluatable(Context)) {
                    Diags.Report(CE->getArg(1)->getExprLoc(), DiagIDConstant)
                      << Pyr->getName();
                  }
                  std::string index =
                    CE->getArg(1)->EvaluateKnownConstInt(Context).toString(10);

                  if (PyrLHS) {
                    stringCreator.writeMemoryTransfer(PyrLHS, PyrIdxLHS,
                        Pyr->getName() + "(" + index + ")", DEVICE_TO_DEVICE,
                        newStr);
                  } else {
                    stringCreator.writeMemoryTransfer(ImgLHS, Pyr->getName() +
                        "(" + index + ")", DEVICE_TO_DEVICE, newStr);
                  }
                  write_pointer = false;
                }
              }
            }
          }
        }

        if (write_pointer) {
          // get the text string for the memory transfer src
          std::string dataStr;
          llvm::raw_string_ostream DS(dataStr);
          E->getArg(1)->printPretty(DS, 0, PrintingPolicy(CI.getLangOpts()));

          // create memory transfer string
          if (PyrLHS) {
            stringCreator.writeMemoryTransfer(PyrLHS, PyrIdxLHS, DS.str(),
                HOST_TO_DEVICE, newStr);
          } else {
            stringCreator.writeMemoryTransfer(ImgLHS, DS.str(), HOST_TO_DEVICE,
                newStr);
          }
        }
      }

      // rewrite Image assignment to memory transfer
      // get the start location and compute the semi location.
      SourceLocation startLoc = E->getLocStart();
      const char *startBuf = SM->getCharacterData(startLoc);
      const char *semiPtr = strchr(startBuf, ';');
      TextRewriter.ReplaceText(startLoc, semiPtr-startBuf+1, newStr);

      return true;
    }
  }

  return true;
}


bool Rewrite::VisitBinaryOperator(BinaryOperator *E) {
  if (!compilerClasses.HipaccEoP) return true;

  // convert Image assignments to a variable into memory transfer,
  // e.g. in_ptr = IN.getData();
  if (E->getOpcode() == BO_Assign && isa<CXXMemberCallExpr>(E->getRHS())) {
    CXXMemberCallExpr *MCE = dyn_cast<CXXMemberCallExpr>(E->getRHS());

    // match only getData calls to Image instances
    if (MCE->getDirectCallee()->getNameAsString() != "getData") return true;

    if (isa<DeclRefExpr>(MCE->getImplicitObjectArgument())) {
      DeclRefExpr *DRE =
        dyn_cast<DeclRefExpr>(MCE->getImplicitObjectArgument());

      // check if we have an Image
      if (ImgDeclMap.count(DRE->getDecl())) {
        HipaccImage *Img = ImgDeclMap[DRE->getDecl()];

        std::string newStr;

        // get the text string for the memory transfer dst
        std::string dataStr;
        llvm::raw_string_ostream DS(dataStr);
        E->getLHS()->printPretty(DS, 0, PrintingPolicy(CI.getLangOpts()));

        // create memory transfer string
        stringCreator.writeMemoryTransfer(Img, DS.str(), DEVICE_TO_HOST,
            newStr);

        // rewrite Image assignment to memory transfer
        // get the start location and compute the semi location.
        SourceLocation startLoc = E->getLocStart();
        const char *startBuf = SM->getCharacterData(startLoc);
        const char *semiPtr = strchr(startBuf, ';');
        TextRewriter.ReplaceText(startLoc, semiPtr-startBuf+1, newStr);
      }
    }
  }

  return true;
}


bool Rewrite::VisitCXXMemberCallExpr(CXXMemberCallExpr *E) {
  if (!compilerClasses.HipaccEoP) return true;

  // a) convert invocation of 'execute' member function into kernel launch, e.g.
  //    K.execute()
  //    therefore, we need the declaration of K in order to get the parameters
  //    and the IterationSpace for the CUDA/OpenCL kernel, e.g.
  //    AddKernel K(IS, IN, OUT, 23);
  //    IS -> kernel launch configuration
  //    IN, OUT, 23 -> kernel parameters
  //    Image width, height, and stride -> kernel parameters
  // b) convert getReducedData calls
  //    float min = MinReduction.getReducedData();
  // c) convert getWidth/getHeight calls

  if (E->getImplicitObjectArgument() &&
      isa<DeclRefExpr>(E->getImplicitObjectArgument()->IgnoreParenCasts())) {

    DeclRefExpr *DRE =
      dyn_cast<DeclRefExpr>(E->getImplicitObjectArgument()->IgnoreParenCasts());
    // match execute calls to user kernel instances
    if (!KernelDeclMap.empty() &&
        E->getDirectCallee()->getNameAsString() == "execute") {
      // get the user Kernel class
      if (KernelDeclMap.count(DRE->getDecl())) {
        HipaccKernel *K = KernelDeclMap[DRE->getDecl()];
        VarDecl *VD = K->getDecl();
        std::string newStr;

        // this was checked before, when the user class was parsed
        CXXConstructExpr *CCE = dyn_cast<CXXConstructExpr>(VD->getInit());
        assert(CCE->getNumArgs()==K->getKernelClass()->getNumArgs() &&
            "number of arguments doesn't match!");

        // set host argument names and retrieve literals stored to temporaries
        K->setHostArgNames(llvm::makeArrayRef(CCE->getArgs(),
              CCE->getNumArgs()), newStr, literalCount);

        //
        // TODO: handle the case when only reduce function is specified
        //
        // create kernel call string
        stringCreator.writeKernelCall(K->getKernelName(), K->getKernelClass(),
            K, newStr);

        // create reduce call string
        if (K->getKernelClass()->getReduceFunction()) {
          newStr += "\n" + stringCreator.getIndent();
          stringCreator.writeReductionDeclaration(K, newStr);
          stringCreator.writeReduceCall(K->getKernelClass(), K, newStr);
        }

        // rewrite kernel invocation
        // get the start location and compute the semi location.
        SourceLocation startLoc = E->getLocStart();
        const char *startBuf = SM->getCharacterData(startLoc);
        const char *semiPtr = strchr(startBuf, ';');
        TextRewriter.ReplaceText(startLoc, semiPtr-startBuf+1, newStr);
      }
    }
  }

  // getWidth/getHeight MemberExpr calls
  if (isa<MemberExpr>(E->getCallee())) {
    MemberExpr *ME = dyn_cast<MemberExpr>(E->getCallee());

    if (isa<DeclRefExpr>(ME->getBase()->IgnoreImpCasts())) {
      DeclRefExpr *DRE = dyn_cast<DeclRefExpr>(ME->getBase()->IgnoreImpCasts());
      std::string newStr;

      // get the Kernel from the DRE if we have one
      if (KernelDeclMap.count(DRE->getDecl())) {
        // match for supported member calls
        if (ME->getMemberNameInfo().getAsString() == "getReducedData") {
          HipaccKernel *K = KernelDeclMap[DRE->getDecl()];

          std::string newStr(K->getReduceStr());

          // replace member function invocation
          SourceRange range(E->getLocStart(), E->getLocEnd());
          TextRewriter.ReplaceText(range, newStr);

          return true;
        }
      }

      // get the Image from the DRE if we have one
      if (ImgDeclMap.count(DRE->getDecl())) {
        // match for supported member calls
        if (ME->getMemberNameInfo().getAsString() == "getWidth") {
          newStr = "width";
        } else if (ME->getMemberNameInfo().getAsString() == "getHeight") {
          newStr = "height";
        }
      }

      // get the Accessor from the DRE if we have one
      if (AccDeclMap.count(DRE->getDecl())) {
        // match for supported member calls
        if (ME->getMemberNameInfo().getAsString() == "getWidth") {
          newStr = "img.width";
        } else if (ME->getMemberNameInfo().getAsString() == "getHeight") {
          newStr = "img.height";
        }
      }

      if (!newStr.empty()) {
        // replace member function invocation
        SourceRange range(ME->getMemberLoc(), E->getLocEnd());
        TextRewriter.ReplaceText(range, newStr);
      }
    }
  }

  return true;
}


bool Rewrite::VisitCallExpr (CallExpr *E) {
  // rewrite function calls 'traverse' to 'hipaccTraverse'
  if (isa<ImplicitCastExpr>(E->getCallee())) {
    DeclRefExpr *DRE = dyn_cast<DeclRefExpr>(dyn_cast<ImplicitCastExpr>(
                           E->getCallee())->getSubExpr());
    if (DRE && DRE->getDecl()->getNameAsString() == "traverse") {
      SourceLocation startLoc = E->getLocStart();
      const char *startBuf = SM->getCharacterData(startLoc);
      const char *semiPtr = strchr(startBuf, '(');
      TextRewriter.ReplaceText(startLoc, semiPtr-startBuf, "hipaccTraverse");
    }
  }
  return true;
}


void Rewrite::setKernelConfiguration(HipaccKernelClass *KC, HipaccKernel *K) {
  #ifdef USE_JIT_ESTIMATE
  bool jit_compile = false;
  switch (compilerOptions.getTargetCode()) {
    default:
    case TARGET_C:
    case TARGET_OpenCLACC:
    case TARGET_OpenCLCPU:
    case TARGET_Renderscript:
    case TARGET_Filterscript:
      jit_compile = false;
      break;
    case TARGET_CUDA:
    case TARGET_OpenCLGPU:
      if (HipaccDevice(compilerOptions).isARMGPU()) {
        jit_compile = false;
      } else {
        jit_compile = true;
      }
      break;
  }

  if (!jit_compile || dump) {
    K->setDefaultConfig();
    return;
  }

  // write kernel file to estimate resource usage
  // kernel declaration for CUDA
  FunctionDecl *kernelDeclEst = createFunctionDecl(Context,
      Context.getTranslationUnitDecl(), K->getKernelName(), Context.VoidTy,
      K->getArgTypes(Context, compilerOptions.getTargetCode()),
      K->getDeviceArgNames());

  // create kernel body
  ASTTranslate *HipaccEst = new ASTTranslate(Context, kernelDeclEst, K, KC,
      builtins, compilerOptions, true);
  Stmt *kernelStmtsEst = HipaccEst->Hipacc(KC->getKernelFunction()->getBody());
  kernelDeclEst->setBody(kernelStmtsEst);

  // write kernel to file
  printKernelFunction(kernelDeclEst, KC, K, K->getFileName(), false);

  // compile kernel in order to get resource usage
  std::string command(K->getCompileCommand(compilerOptions.emitCUDA()) +
      K->getCompileOptions(K->getKernelName(), K->getFileName(),
        compilerOptions.emitCUDA()));

  int reg=0, lmem=0, smem=0, cmem=0;
  char line[FILENAME_MAX];
  SmallVector<std::string, 16> lines;
  FILE *fpipe;

  if (!(fpipe = (FILE *)popen(command.c_str(), "r"))) {
    perror("Problems with pipe");
    exit(EXIT_FAILURE);
  }

  std::string info;
  if (compilerOptions.emitCUDA()) {
    info = "ptxas info : Used %d registers";
  } else {
    if (targetDevice.isAMDGPU()) {
      info = "isa info : Used %d gprs, %d bytes lds, stack size: %d";
    } else {
      info = "ptxas info : Used %d registers";
    }
  }
  while (fgets(line, sizeof(char) * FILENAME_MAX, fpipe)) {
    lines.push_back(std::string(line));
    if (targetDevice.isAMDGPU()) {
      sscanf(line, info.c_str(), &reg, &smem, &lmem);
    } else {
      char *ptr = line;
      int num_read = 0, val1 = 0, val2 = 0;
      char mem_type = 'x';

      if (compilerOptions.getTargetDevice() >= FERMI_20) {
        // scan for stack size (shared memory)
        num_read = sscanf(ptr, "%d bytes %1c tack frame", &val1, &mem_type);

        if (num_read == 2 && mem_type == 's') {
          smem = val1;
          continue;
        }
      }

      num_read = sscanf(line, info.c_str(), &reg);
      if (!num_read) continue;

      while ((ptr = strchr(ptr, ','))) {
        ptr++;
        num_read = sscanf(ptr, "%d+%d bytes %1c mem", &val1, &val2, &mem_type);
        if (num_read == 3) {
          switch (mem_type) {
            default:
              llvm::errs() << "wrong memory specifier '" << mem_type
                           << "': " << ptr;
              break;
            case 'c':
              cmem += val1 + val2;
              break;
            case 'l':
              lmem += val1 + val2;
              break;
            case 's':
              smem += val1 + val2;
              break;
          }
        } else {
          num_read = sscanf(ptr, "%d bytes %1c mem", &val1, &mem_type);
          if (num_read == 2) {
            switch (mem_type) {
              default:
                llvm::errs() << "wrong memory specifier '" << mem_type
                             << "': " << ptr;
                break;
              case 'c':
                cmem += val1;
                break;
              case 'l':
                lmem += val1;
                break;
              case 's':
                smem += val1;
                break;
            }
          } else {
            llvm::errs() << "Unexpected memory usage specification: '" << ptr;
          }
        }
      }
    }
  }
  pclose(fpipe);

  if (reg == 0) {
    unsigned int DiagIDCompile =
      Diags.getCustomDiagID(DiagnosticsEngine::Warning,
          "Compiling kernel in file '%0.%1' failed, using default kernel configuration:\n%2");
    Diags.Report(DiagIDCompile)
      << K->getFileName() << (const char*)(compilerOptions.emitCUDA()?"cu":"cl")
      << command.c_str();
    for (size_t i=0, e=lines.size(); i!=e; ++i) {
      llvm::errs() << lines.data()[i];
    }
  } else {
    if (targetDevice.isAMDGPU()) {
      llvm::errs() << "Resource usage for kernel '" << K->getKernelName() << "'"
                   << ": " << reg << " gprs, "
                   << lmem << " bytes stack, "
                   << smem << " bytes lds\n";
    } else {
      llvm::errs() << "Resource usage for kernel '" << K->getKernelName() << "'"
                   << ": " << reg << " registers, "
                   << lmem << " bytes lmem, "
                   << smem << " bytes smem, "
                   << cmem << " bytes cmem\n";
    }
  }

  K->setResourceUsage(reg, lmem, smem, cmem);
  #else
  K->setDefaultConfig();
  #endif
}




void Rewrite::printReductionFunction(HipaccKernelClass *KC, HipaccKernel *K,
    PrintingPolicy Policy, llvm::raw_ostream *OS) {
  FunctionDecl *fun = KC->getReduceFunction();

  // preprocessor defines
  if (!compilerOptions.exploreConfig()) {
    *OS << "#define BS " << K->getNumThreadsReduce() << "\n"
        << "#define PPT " << K->getPixelsPerThreadReduce() << "\n";
  }
  if (K->getIterationSpace()->isCrop()) {
    *OS << "#define USE_OFFSETS\n";
  }
  switch (compilerOptions.getTargetCode()) {
    case TARGET_C:
      break;
    case TARGET_OpenCLACC:
    case TARGET_OpenCLCPU:
    case TARGET_OpenCLGPU:
      if (compilerOptions.useTextureMemory() &&
          compilerOptions.getTextureType()==Array2D) {
        *OS << "#define USE_ARRAY_2D\n";
      }
      *OS << "#include \"hipacc_ocl_red.hpp\"\n\n";
      break;
    case TARGET_CUDA:
      if (compilerOptions.useTextureMemory() &&
          compilerOptions.getTextureType()==Array2D) {
        *OS << "#define USE_ARRAY_2D\n";
      }
      *OS << "#include \"hipacc_cuda_red.hpp\"\n\n";
      break;
    case TARGET_Renderscript:
    case TARGET_Filterscript:
      *OS << "#pragma version(1)\n"
          << "#pragma rs java_package_name("
          << compilerOptions.getRSPackageName()
          << ")\n\n";
      if (compilerOptions.emitFilterscript()) {
        *OS << "#define FS\n";
      }
      *OS << "#define DATA_TYPE "
          << K->getIterationSpace()->getImage()->getTypeStr() << "\n"
          << "#include \"hipacc_rs_red.hpp\"\n\n";
      // input/output allocation definitions
      *OS << "rs_allocation _red_Input;\n";
      *OS << "rs_allocation _red_Output;\n";
      // offset specification
      if (K->getIterationSpace()->isCrop()) {
        *OS << "int _red_offset_x;\n";
        *OS << "int _red_offset_y;\n";
      }
      *OS << "int _red_stride;\n";
      *OS << "int _red_is_height;\n";
      *OS << "int _red_num_elements;\n";
      break;
  }


  // write kernel name and qualifiers
  switch (compilerOptions.getTargetCode()) {
    case TARGET_C:
    case TARGET_OpenCLACC:
    case TARGET_OpenCLCPU:
    case TARGET_OpenCLGPU:
      break;
    case TARGET_CUDA:
      *OS << "extern \"C\" {\n";
      *OS << "__device__ ";
      break;
    case TARGET_Renderscript:
    case TARGET_Filterscript:
      *OS << "static ";
      break;
  }
  *OS << "inline " << fun->getResultType().getAsString() << " "
      << K->getReduceName() << "(";
  // write kernel parameters
  size_t comma = 0;
  for (size_t i=0, e=fun->getNumParams(); i!=e; ++i) {
    std::string Name(fun->getParamDecl(i)->getNameAsString());

    // normal arguments
    if (comma++) *OS << ", ";
    QualType T = fun->getParamDecl(i)->getType();
    if (ParmVarDecl *Parm = dyn_cast<ParmVarDecl>(fun))
      T = Parm->getOriginalType();
    T.getAsStringInternal(Name, Policy);
    *OS << Name;
  }
  *OS << ") ";

  // print kernel body
  fun->getBody()->printPretty(*OS, 0, Policy, 0);

  // instantiate reduction
  switch (compilerOptions.getTargetCode()) {
    case TARGET_C:
      break;
    case TARGET_OpenCLACC:
    case TARGET_OpenCLCPU:
    case TARGET_OpenCLGPU:
      // 2D reduction
      *OS << "REDUCTION_OCL_2D(" << K->getReduceName() << "2D, "
          << fun->getResultType().getAsString() << ", "
          << K->getReduceName() << ", "
          << K->getIterationSpace()->getImage()->getImageReadFunction()
          << ")\n";
      // 1D reduction
      *OS << "REDUCTION_OCL_1D(" << K->getReduceName() << "1D, "
          << fun->getResultType().getAsString() << ", "
          << K->getReduceName() << ")\n";
      break;
    case TARGET_CUDA:
      // print 2D CUDA array definition - this is only required on FERMI and if
      // Array2D is selected, but doesn't harm otherwise
      *OS << "texture<" << fun->getResultType().getAsString()
          << ", cudaTextureType2D, cudaReadModeElementType> _tex"
          << K->getIterationSpace()->getImage()->getName() + K->getName() << ";\n\n";
      // 2D reduction
      if (compilerOptions.getTargetDevice()>=FERMI_20 &&
          !compilerOptions.exploreConfig()) {
        *OS << "__device__ unsigned int finished_blocks_" << K->getReduceName()
            << "2D = 0;\n\n";
        *OS << "REDUCTION_CUDA_2D_THREAD_FENCE(";
      } else {
        *OS << "REDUCTION_CUDA_2D(";
      }
      *OS << K->getReduceName() << "2D, "
          << fun->getResultType().getAsString() << ", "
          << K->getReduceName() << ", _tex"
          << K->getIterationSpace()->getImage()->getName() + K->getName() << ")\n";
      // 1D reduction
      if (compilerOptions.getTargetDevice() >= FERMI_20 &&
          !compilerOptions.exploreConfig()) {
        // no second step required
      } else {
        *OS << "REDUCTION_CUDA_1D(" << K->getReduceName() << "1D, "
            << fun->getResultType().getAsString() << ", "
            << K->getReduceName() << ")\n";
      }
      break;
    case TARGET_Renderscript:
    case TARGET_Filterscript:
      *OS << "REDUCTION_RS_2D(" << K->getReduceName() << "2D, "
          << fun->getResultType().getAsString() << ", ALL, "
          << K->getReduceName() << ")\n";
      // 1D reduction
      *OS << "REDUCTION_RS_1D(" << K->getReduceName() << "1D, "
          << fun->getResultType().getAsString() << ", ALL, "
          << K->getReduceName() << ")\n";
      break;
  }

  if (compilerOptions.emitCUDA()) {
    *OS << "}\n";
  }
  *OS << "#include \"hipacc_undef.hpp\"\n";

  *OS << "\n";
}


void Rewrite::printKernelFunction(FunctionDecl *D, HipaccKernelClass *KC,
    HipaccKernel *K, std::string file, bool emitHints) {
  PrintingPolicy Policy = Context.getPrintingPolicy();
  Policy.Indentation = 2;
  Policy.SuppressSpecifiers = false;
  Policy.SuppressTag = false;
  Policy.SuppressScope = false;
  Policy.ConstantArraySizeAsWritten = false;
  Policy.AnonymousTagLocations = true;
  Policy.PolishForDeclaration = false;

  switch (compilerOptions.getTargetCode()) {
    case TARGET_CUDA:
      Policy.LangOpts.CUDA = 1; break;
    case TARGET_OpenCLACC:
    case TARGET_OpenCLCPU:
    case TARGET_OpenCLGPU:
      Policy.LangOpts.OpenCL = 1; break;
    case TARGET_C:
    case TARGET_Renderscript:
    case TARGET_Filterscript:
      break;
  }

  int fd;
  std::string filename(file);
  std::string ifdef("_" + file + "_");
  switch (compilerOptions.getTargetCode()) {
    case TARGET_C:
      filename += ".cc";
      ifdef += "CC_"; break;
    case TARGET_CUDA:
      filename += ".cu";
      ifdef += "CU_"; break;
    case TARGET_OpenCLACC:
    case TARGET_OpenCLCPU:
    case TARGET_OpenCLGPU:
      filename += ".cl";
      ifdef += "CL_"; break;
    case TARGET_Renderscript:
      filename += ".rs";
      ifdef += "RS_"; break;
    case TARGET_Filterscript:
      filename += ".fs";
      ifdef += "FS_"; break;
  }

  // open file stream using own file descriptor. We need to call fsync() to
  // compile the generated code using nvcc afterwards.
  llvm::raw_ostream *OS = &llvm::errs();
  if (!dump) {
    while ((fd = open(filename.c_str(), O_WRONLY|O_CREAT|O_TRUNC, 0664)) < 0) {
      if (errno != EINTR) {
        std::string errorInfo("Error opening output file '" + filename + "'");
        perror(errorInfo.c_str());
      }
    }
    OS = new llvm::raw_fd_ostream(fd, false);
  }

  // write ifndef, ifdef
  std::transform(ifdef.begin(), ifdef.end(), ifdef.begin(), ::toupper);
  *OS << "#ifndef " + ifdef + "\n";
  *OS << "#define " + ifdef + "\n\n";

  // preprocessor defines
  switch (compilerOptions.getTargetCode()) {
    case TARGET_C:
    case TARGET_OpenCLACC:
    case TARGET_OpenCLCPU:
    case TARGET_OpenCLGPU:
      break;
    case TARGET_CUDA:
      *OS << "#include \"hipacc_types.hpp\"\n"
          << "#include \"hipacc_math_functions.hpp\"\n\n";
      break;
    case TARGET_Renderscript:
    case TARGET_Filterscript:
      *OS << "#pragma version(1)\n"
          << "#pragma rs java_package_name("
          << compilerOptions.getRSPackageName()
          << ")\n\n";
      break;
  }


  // interpolation includes & definitions
  bool inc=false;
  SmallVector<std::string, 16> InterpolationDefinitionsLocal;
  for (size_t i=0; i<K->getNumArgs(); i++) {
    FieldDecl *FD = K->getDeviceArgFields()[i];
    HipaccAccessor *Acc = K->getImgFromMapping(FD);

    if (!Acc || !K->getUsed(K->getDeviceArgNames()[i])) continue;

    if (Acc->getInterpolation()!=InterpolateNO) {
      if (!inc) {
        inc = true;
        switch (compilerOptions.getTargetCode()) {
          case TARGET_C:
            break;
          case TARGET_CUDA:
            *OS << "#include \"hipacc_cuda_interpolate.hpp\"\n\n";
            break;
          case TARGET_OpenCLACC:
          case TARGET_OpenCLCPU:
          case TARGET_OpenCLGPU:
            *OS << "#include \"hipacc_ocl_interpolate.hpp\"\n\n";
            break;
          case TARGET_Renderscript:
          case TARGET_Filterscript:
              *OS << "#include \"hipacc_rs_interpolate.hpp\"\n\n";
            break;
        }
      }

      // define required interpolation mode
      if (inc && Acc->getInterpolation()>InterpolateNN) {
        std::string function_name(ASTTranslate::getInterpolationName(Context,
              builtins, compilerOptions, K, Acc, border_variant()));
        std::string suffix("_" +
            builtins.EncodeTypeIntoStr(Acc->getImage()->getType(), Context));

        std::string resultStr;
        stringCreator.writeInterpolationDefinition(K, Acc, function_name,
            suffix, Acc->getInterpolation(), Acc->getBoundaryHandling(),
            resultStr);

        switch (compilerOptions.getTargetCode()) {
          case TARGET_C:
            break;
          case TARGET_CUDA:
          case TARGET_OpenCLACC:
          case TARGET_OpenCLCPU:
          case TARGET_OpenCLGPU:
          case TARGET_Renderscript:
          case TARGET_Filterscript:
            InterpolationDefinitionsLocal.push_back(resultStr);
            break;
        }

        resultStr.erase();
        stringCreator.writeInterpolationDefinition(K, Acc, function_name,
            suffix, InterpolateNO, BOUNDARY_UNDEFINED, resultStr);

        switch (compilerOptions.getTargetCode()) {
          case TARGET_C:
            break;
          case TARGET_CUDA:
          case TARGET_OpenCLACC:
          case TARGET_OpenCLCPU:
          case TARGET_OpenCLGPU:
          case TARGET_Renderscript:
          case TARGET_Filterscript:
            InterpolationDefinitionsLocal.push_back(resultStr);
            break;
        }
      }
    }
  }

  if (((compilerOptions.emitCUDA() && // CUDA, but no exploration or no hints
          (compilerOptions.exploreConfig() || !emitHints)) ||
        !compilerOptions.emitCUDA())  // or other targets
      && inc && InterpolationDefinitionsLocal.size()) {
    // sort definitions and remove duplicate definitions
    std::sort(InterpolationDefinitionsLocal.begin(),
        InterpolationDefinitionsLocal.end());
    InterpolationDefinitionsLocal.erase(std::unique(
          InterpolationDefinitionsLocal.begin(),
          InterpolationDefinitionsLocal.end()),
        InterpolationDefinitionsLocal.end());

    // add interpolation definitions
    while (InterpolationDefinitionsLocal.size()) {
      *OS << InterpolationDefinitionsLocal.pop_back_val();
    }
    *OS << "\n";
  } else {
    // emit interpolation definitions at the beginning at the file
    if (InterpolationDefinitionsLocal.size()) {
      while (InterpolationDefinitionsLocal.size()) {
        InterpolationDefinitionsGlobal.push_back(
            InterpolationDefinitionsLocal.pop_back_val());
      }
    }
  }

  // declarations of textures, surfaces, variables, etc.
  for (size_t i=0; i<K->getNumArgs(); ++i) {
    if (!K->getUsed(K->getDeviceArgNames()[i])) continue;

    FieldDecl *FD = K->getDeviceArgFields()[i];

    // output image declaration
    if (i==0) {
      switch (compilerOptions.getTargetCode()) {
        case TARGET_C:
        case TARGET_OpenCLACC:
        case TARGET_OpenCLCPU:
        case TARGET_OpenCLGPU:
          break;
        case TARGET_CUDA:
          // surface declaration
          if (compilerOptions.useTextureMemory() &&
              compilerOptions.getTextureType()==Array2D) {
            *OS << "surface<void, cudaSurfaceType2D> _surfOutput"
                << K->getName() << ";\n\n";
          }
          break;
        case TARGET_Renderscript:
        case TARGET_Filterscript:
          *OS << "rs_allocation Output;\n";
          break;
      }
      continue;
    }

    // global image declarations
    HipaccAccessor *Acc = K->getImgFromMapping(FD);
    if (Acc) {
      QualType T = Acc->getImage()->getType();

      switch (compilerOptions.getTargetCode()) {
        case TARGET_C:
        case TARGET_OpenCLACC:
        case TARGET_OpenCLCPU:
        case TARGET_OpenCLGPU:
          break;
        case TARGET_CUDA:
          // texture declaration
          if (KC->getImgAccess(FD) == READ_ONLY && K->useTextureMemory(Acc)) {
            // no texture declaration for __ldg() intrinsic
            if (K->useTextureMemory(Acc) == Ldg) break;
            *OS << "texture<";
            *OS << T.getAsString();
            switch (K->useTextureMemory(Acc)) {
              default:
              case Linear1D:
                *OS << ", cudaTextureType1D, cudaReadModeElementType> _tex";
                break;
              case Linear2D:
              case Array2D:
                *OS << ", cudaTextureType2D, cudaReadModeElementType> _tex";
                break;
            }
            *OS << FD->getNameAsString() << K->getName() << ";\n";
          }
          break;
        case TARGET_Renderscript:
        case TARGET_Filterscript:
          *OS << "rs_allocation " << FD->getNameAsString() << ";\n";
          break;
      }
      continue;
    }

    // constant memory declarations
    HipaccMask *Mask = K->getMaskFromMapping(FD);
    if (Mask) {
      if (Mask->isConstant()) {
        switch (compilerOptions.getTargetCode()) {
          case TARGET_OpenCLACC:
          case TARGET_OpenCLCPU:
          case TARGET_OpenCLGPU:
            *OS << "__constant ";
            break;
          case TARGET_CUDA:
            *OS << "__device__ __constant__ ";
            break;
          case TARGET_C:
          case TARGET_Renderscript:
          case TARGET_Filterscript:
            *OS << "static const ";
            break;
        }
        *OS << Mask->getTypeStr() << " " << Mask->getName() << K->getName() << "["
            << Mask->getSizeYStr() << "][" << Mask->getSizeXStr() << "] = {\n";

        // print Mask constant literals to 2D array
        for (size_t y=0; y<Mask->getSizeY(); ++y) {
          *OS << "        {";
          for (size_t x=0; x<Mask->getSizeX(); ++x) {
            Mask->getInitExpr(x, y)->printPretty(*OS, 0, Policy, 0);
            if (x<Mask->getSizeX()-1) {
              *OS << ", ";
            }
          }
          if (y<Mask->getSizeY()-1) {
            *OS << "},\n";
          } else {
            *OS << "}\n";
          }
        }
        *OS << "    };\n\n";
        Mask->setIsPrinted(true);
      } else {
        // emit declaration in CUDA and Renderscript
        // for other back ends, the mask will be added as kernel parameter
        switch (compilerOptions.getTargetCode()) {
          case TARGET_C:
          case TARGET_OpenCLACC:
          case TARGET_OpenCLCPU:
          case TARGET_OpenCLGPU:
            break;
          case TARGET_CUDA:
            *OS << "__device__ __constant__ " << Mask->getTypeStr() << " "
                << Mask->getName() << K->getName() << "[" << Mask->getSizeYStr()
                << "][" << Mask->getSizeXStr() << "];\n\n";
            Mask->setIsPrinted(true);
            break;
          case TARGET_Renderscript:
          case TARGET_Filterscript:
            *OS << "rs_allocation " << K->getDeviceArgNames()[i] << ";\n\n";
            Mask->setIsPrinted(true);
            break;
        }
      }
      continue;
    }

    // normal variables - Renderscript only
    if (0 != (compilerOptions.getTargetCode() & (TARGET_Renderscript |
                                                 TARGET_Filterscript))) {
      QualType QT = K->getArgTypes(Context, compilerOptions.getTargetCode())[i];
      QT.removeLocalConst();
      *OS << QT.getAsString() << " " << K->getDeviceArgNames()[i] << ";\n";
      continue;
    }
  }

  // extern scope for CUDA
  *OS << "\n";
  if (compilerOptions.emitCUDA()) {
    *OS << "extern \"C\" {\n";
  }

  // function definitions
  for (size_t i=0, e=K->getFunctionCalls().size(); i<e; ++i) {
    FunctionDecl *FD = K->getFunctionCalls()[i];

    switch (compilerOptions.getTargetCode()) {
      case TARGET_C:
      case TARGET_OpenCLACC:
      case TARGET_OpenCLCPU:
      case TARGET_OpenCLGPU:
        *OS << "inline "; break;
      case TARGET_Renderscript:
      case TARGET_Filterscript:
        *OS << "inline static "; break;
      case TARGET_CUDA:
        *OS << "__inline__ __device__ "; break;
    }
    FD->print(*OS, Policy);
  }

  // write kernel name and qualifiers
  switch (compilerOptions.getTargetCode()) {
    case TARGET_CUDA:
      *OS << "__global__ ";
      if (compilerOptions.exploreConfig() && emitHints) {
        *OS << "__launch_bounds__ (BSX_EXPLORE * BSY_EXPLORE) ";
      } else {
        *OS << "__launch_bounds__ (" << K->getNumThreadsX() << "*"
            << K->getNumThreadsY() << ") ";
      }
      break;
    case TARGET_OpenCLACC:
    case TARGET_OpenCLCPU:
    case TARGET_OpenCLGPU:
      if (compilerOptions.useTextureMemory() &&
          compilerOptions.getTextureType()==Array2D) {
        *OS << "__constant sampler_t " << D->getNameInfo().getAsString()
            << "Sampler = CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_NONE | "
            << " CLK_FILTER_NEAREST; \n\n";
      }
      *OS << "__kernel ";
      if (compilerOptions.exploreConfig() && emitHints) {
        *OS << "__attribute__((reqd_work_group_size(BSX_EXPLORE, BSY_EXPLORE, "
            << "1))) ";
      } else {
        *OS << "__attribute__((reqd_work_group_size(" << K->getNumThreadsX()
            << ", " << K->getNumThreadsY() << ", 1))) ";
      }
      break;
    case TARGET_C:
    case TARGET_Renderscript:
      break;
    case TARGET_Filterscript:
      *OS << K->getIterationSpace()->getAccessor()->getImage()->getTypeStr()
          << " __attribute__((kernel)) ";
      break;
  }
  if (!compilerOptions.emitFilterscript()) {
    *OS << "void ";
  }
  *OS << K->getKernelName();
  *OS << "(";

  // write kernel parameters
  size_t comma = 0;
  for (size_t i=0, e=D->getNumParams(); i!=e; ++i) {
    std::string Name(D->getParamDecl(i)->getNameAsString());
    FieldDecl *FD = K->getDeviceArgFields()[i];

    if (!K->getUsed(Name) &&
        !compilerOptions.emitFilterscript() &&
        !compilerOptions.emitRenderscript()) {
        // Proceed for Filterscript, because output image is never explicitly used
        continue;
    }

    QualType T = D->getParamDecl(i)->getType();
    T.removeLocalConst();
    T.removeLocalRestrict();

    // check if we have a Mask or Domain
    HipaccMask *Mask = K->getMaskFromMapping(FD);
    if (Mask) {
      switch (compilerOptions.getTargetCode()) {
        case TARGET_OpenCLACC:
        case TARGET_OpenCLCPU:
        case TARGET_OpenCLGPU:
          if (!Mask->isConstant()) {
            if (comma++) *OS << ", ";
            *OS << "__constant ";
            T.getAsStringInternal(Name, Policy);
            *OS << Name;
            *OS << " __attribute__ ((max_constant_size (" << Mask->getSizeXStr()
                << "*" << Mask->getSizeYStr() << "*sizeof("
                << Mask->getTypeStr() << "))))";
            }
          break;
        case TARGET_C:
          if (!Mask->isConstant()) {
            if (comma++) *OS << ", ";
            *OS << "const "
                << Mask->getTypeStr()
                << " " << Mask->getName() << K->getName()
                << "[" << Mask->getSizeYStr() << "]"
                << "[" << Mask->getSizeXStr() << "]";
          }
          break;
        case TARGET_CUDA:
          // mask/domain is declared as constant memory
          break;
        case TARGET_Renderscript:
        case TARGET_Filterscript:
          // mask/domain is declared as static memory
          break;
      }
      continue;
    }

    // check if we have an Accessor
    HipaccAccessor *Acc = K->getImgFromMapping(FD);
    MemoryAccess memAcc = UNDEFINED;
    if (i==0) { // first argument is always the output image
      bool doBreak = false;
      switch (compilerOptions.getTargetCode()) {
        case TARGET_C:
        case TARGET_OpenCLACC:
        case TARGET_OpenCLCPU:
        case TARGET_OpenCLGPU:
          break;
        case TARGET_CUDA:
          if (compilerOptions.useTextureMemory() &&
              compilerOptions.getTextureType()==Array2D) {
              continue;
          }
          break;
        case TARGET_Renderscript:
          // parameters are set separately for Renderscript
          // add parameters for dummy allocation and indices
          *OS << K->getIterationSpace()->getAccessor()->getImage()->getTypeStr()
              << " *_IS, uint32_t x, uint32_t y";
          doBreak = true;
          break;
        case TARGET_Filterscript:
          *OS << "uint32_t x, uint32_t y";
          doBreak = true;
          break;
      }
      if (doBreak) break;
      Acc = K->getIterationSpace()->getAccessor();
      memAcc = WRITE_ONLY;
    } else if (Acc) {
      memAcc = KC->getImgAccess(FD);
    }

    if (Acc) {
      switch (compilerOptions.getTargetCode()) {
        case TARGET_OpenCLACC:
        case TARGET_OpenCLCPU:
        case TARGET_OpenCLGPU:
          // __global keyword to specify memory location is only needed for OpenCL
          if (comma++) *OS << ", ";
          if (K->useTextureMemory(Acc)) {
            if (memAcc==WRITE_ONLY) {
              *OS << "__write_only image2d_t ";
            } else {
              *OS << "__read_only image2d_t ";
            }
          } else {
            *OS << "__global ";
            if (memAcc==READ_ONLY) *OS << "const ";
            *OS << T->getPointeeType().getAsString();
            *OS << " * restrict ";
          }
          *OS << Name;
          break;
        case TARGET_CUDA:
          if (K->useTextureMemory(Acc) && memAcc==READ_ONLY &&
              // parameter required for __ldg() intrinsic
              !(K->useTextureMemory(Acc) == Ldg)) {
            // no parameter is emitted for textures
            continue;
          } else {
            if (comma++) *OS << ", ";
            if (memAcc==READ_ONLY) *OS << "const ";
            *OS << T->getPointeeType().getAsString();
            *OS << " * __restrict__ ";
            *OS << Name;
          }
          break;
        case TARGET_C:
          if (comma++) *OS << ", ";
          if (memAcc==READ_ONLY) *OS << "const ";
          *OS << Acc->getImage()->getTypeStr()
              << " " << Name
              << "[" << Acc->getImage()->getSizeXStr() << "]"
              << "[" << Acc->getImage()->getSizeYStr() << "]";
          // alternative for Pencil:
          // *OS << "[static const restrict 2048][4096]";
          break;
        case TARGET_Renderscript:
        case TARGET_Filterscript:
          break;
      }
      continue;
    }

    // normal arguments
    if (comma++) *OS << ", ";
    T.getAsStringInternal(Name, Policy);
    *OS << Name;

    // default arguments ...
    if (Expr *Init = D->getParamDecl(i)->getInit()) {
      CXXConstructExpr *CCE = dyn_cast<CXXConstructExpr>(Init);
      if (!CCE || CCE->getConstructor()->isCopyConstructor()) {
        *OS << " = ";
      }
      Init->printPretty(*OS, 0, Policy, 0);
    }
  }
  *OS << ") ";

  // print kernel body
  D->getBody()->printPretty(*OS, 0, Policy, 0);
  if (compilerOptions.emitCUDA()) {
    *OS << "}\n";
  }
  *OS << "\n";

  if (KC->getReduceFunction()) {
    printReductionFunction(KC, K, Policy, OS);
  }

  *OS << "#endif //" + ifdef + "\n";
  *OS << "\n";
  OS->flush();
  if (!dump) {
    fsync(fd);
    close(fd);
  }
}

// vim: set ts=2 sw=2 sts=2 et ai:

