IF (MSVC)
    SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${LLVM_CXXFLAGS}")
ELSE()
    SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-rtti ${LLVM_CXXFLAGS}")
ENDIF()

SET(CLANG_USED_LIBS
    clangFrontendTool
    clangFrontend
    clangDriver
    clangSerialization
    clangCodeGen
    clangParse
    clangSema
    clangRewriteFrontend
    clangRewriteCore
    clangAnalysis
    clangEdit
    clangAST
    clangBasic
    clangLex)

SET(HIPACC_USED_LIBS
    Rewrite
    Backend
    CreateHostStrings
    ClassRepresentation
    ASTTranslate
    SIMDTypes)

IF(USE_POLLY)
    LIST(APPEND HIPACC_USED_LIBS Polly)
    ADD_LIBRARY(LLVMPolly SHARED IMPORTED)
    SET_TARGET_PROPERTIES(LLVMPolly
        PROPERTIES
        IMPORTED_LOCATION ${LLVM_LIBRARY_DIR}/LLVMPolly${CMAKE_SHARED_LIBRARY_SUFFIX})
ENDIF(USE_POLLY)

LIST(APPEND HIPACC_USED_LIBS
    KernelStatistics
    Builtins
    ASTNode)

IF(USE_POLLY)
    LIST(APPEND LLVM_MODULE_LIBS LLVMPollyAnalysis LLVMPollyCodeGen LLVMPollyExchange LLVMPolly)
ENDIF(USE_POLLY)

LINK_DIRECTORIES(${LLVM_LIBRARY_DIR})

SET(hipacc_SOURCES hipacc.cpp)
SET(hipacc_HEADERS hipacc.h "../include/hipacc/Config/CompilerOptions.h"
                            "${HIPACC_BINARY_DIR}/include/hipacc/Config/config.h")
ADD_EXECUTABLE(hipacc ${hipacc_SOURCES} ${hipacc_HEADERS})


TARGET_LINK_LIBRARIES(hipacc ${HIPACC_USED_LIBS} ${CLANG_USED_LIBS} ${LLVM_MODULE_LIBS} ${LLVM_LFLAGS} ${TIME_LINK})
ADD_DEPENDENCIES(hipacc ${HIPACC_USED_LIBS})

SET_TARGET_PROPERTIES(hipacc
    PROPERTIES
    LINKER_LANGUAGE CXX
    PREFIX "")

SET_TARGET_PROPERTIES(hipacc
    PROPERTIES
    VERSION ${HIPACC_VERSION})

INSTALL(TARGETS hipacc RUNTIME DESTINATION bin)

