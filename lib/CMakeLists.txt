IF (MSVC)
    SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${LLVM_CXXFLAGS}")
ELSE()
    SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-rtti ${LLVM_CXXFLAGS}")
ENDIF()

SET(HIPACC_RELATIVE_INCLUDE_DIR "../../include/hipacc")

ADD_SUBDIRECTORY(Analysis)
ADD_SUBDIRECTORY(AST)
ADD_SUBDIRECTORY(Backend)
ADD_SUBDIRECTORY(Device)
ADD_SUBDIRECTORY(DSL)
ADD_SUBDIRECTORY(Rewrite)
ADD_SUBDIRECTORY(Vectorization)

