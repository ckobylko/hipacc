CMAKE_MINIMUM_REQUIRED(VERSION 2.8)

#SET(CMAKE_VERBOSE_MAKEFILE on)

# provide only Debug and Release configurations
SET(CMAKE_CONFIGURATION_TYPES "Debug;Release" CACHE STRING "build config types" FORCE)
IF(NOT CMAKE_BUILD_TYPE)
    SET(CMAKE_BUILD_TYPE Debug CACHE STRING "build type: Debug or Release" FORCE)
ENDIF(NOT CMAKE_BUILD_TYPE)

IF(NOT IS_ABSOLUTE ${CMAKE_INSTALL_PREFIX})
    MESSAGE(FATAL_ERROR "CMAKE_INSTALL_PREFIX has to be an absolute path!")
ENDIF(NOT IS_ABSOLUTE ${CMAKE_INSTALL_PREFIX})


PROJECT(HIPACC)
SET(HIPACC_MAJOR_VERSION 0)
SET(HIPACC_MINOR_VERSION 6)
SET(HIPACC_PATCH_VERSION 1)
SET(HIPACC_VERSION ${HIPACC_MAJOR_VERSION}.${HIPACC_MINOR_VERSION}.${HIPACC_PATCH_VERSION})
SET(HIPACC_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR})
SET(HIPACC_BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR})

OPTION(USE_POLLY "Use Polly for analysis" OFF)
INCLUDE(CMakeDependentOption)
CMAKE_DEPENDENT_OPTION(USE_JIT_ESTIMATE "Compile kernels JIT to estimate resource usage" ON "NOT APPLE" OFF)


# Check MinGW setting
OPTION(USE_MINGW "Use MinGW include files instead of the ones from the installed compiler" OFF)

IF(MSVC)
    IF(NOT USE_MINGW)
        MESSAGE(FATAL_ERROR "HIPACC cannot parse the Visual Studio STL! You have to use MinGW instead -> Check USE_MINGW")
    ENDIF()
ENDIF(MSVC)


# get repository and revision
OPTION(CHECK_GIT_REPO_VERSION "Check GIT repository and version" ON)
IF(CHECK_GIT_REPO_VERSION)
    EXECUTE_PROCESS(COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/cmake/scripts/get_repository_path.sh
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        TIMEOUT 5
        RESULT_VARIABLE git_result
        OUTPUT_VARIABLE HIPACC_GIT_REPOSITORY)
    STRING(STRIP ${HIPACC_GIT_REPOSITORY} HIPACC_GIT_REPOSITORY)

    EXECUTE_PROCESS(COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/cmake/scripts/get_source_version.sh
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        TIMEOUT 5
        RESULT_VARIABLE git_result
        OUTPUT_VARIABLE HIPACC_GIT_VERSION)
    STRING(STRIP ${HIPACC_GIT_VERSION} HIPACC_GIT_VERSION)
ENDIF()

# add path for custom modules
SET(CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake/modules)
FIND_PACKAGE(LLVM REQUIRED)
INCLUDE_DIRECTORIES(${LLVM_INCLUDE_DIR})

FIND_PACKAGE(CUDA)
FIND_PACKAGE(OpenCL)
FIND_PACKAGE(RenderScript)

IF(USE_MINGW)
    FIND_PACKAGE(MinGW REQUIRED)

    IF(NOT MINGW_FOUND)
        MESSAGE(FATAL_ERROR "MinGW could not be found! HIPACC most probably will not function correctly.")
    ENDIF(NOT MINGW_FOUND)
ENDIF(USE_MINGW)


IF(NOT CUDA_FOUND AND NOT OPENCL_FOUND AND NOT RENDERSCRIPT_FOUND)
    MESSAGE(WARNING "Neither CUDA, OpenCL nor RenderScript found. At least one of them is required to use HIPACC properly.")
ENDIF(NOT CUDA_FOUND AND NOT OPENCL_FOUND AND NOT RENDERSCRIPT_FOUND)

IF(CUDA_FOUND)
    SET(NVCC_COMPILER "${CUDA_TOOLKIT_ROOT_DIR}/bin/nvcc")
ENDIF(CUDA_FOUND)

IF(OPENCL_FOUND)
    SET(OCL_COMPILER "${CMAKE_INSTALL_PREFIX}/bin/ocl_compile")
    MARK_AS_ADVANCED(CUDA_BUILD_CUBIN CUDA_BUILD_EMULATION CUDA_HOST_COMPILER
        CUDA_SDK_ROOT_DIR CUDA_TOOLKIT_ROOT_DIR CUDA_VERBOSE_BUILD)
ENDIF(OPENCL_FOUND)

MESSAGE(STATUS "Configuration summary:")
MESSAGE(STATUS "===")
MESSAGE(STATUS "USE_POLLY=${USE_POLLY}")
MESSAGE(STATUS "USE_JIT_ESTIMATE=${USE_JIT_ESTIMATE}")
MESSAGE(STATUS "===")


# platform specific fixes linker flags
IF(MSVC)
    # Correct syntax for LLVM libraries
    STRING(STRIP "${LLVM_MODULE_LIBS}" LLVM_MODULE_LIBS)
    STRING(REPLACE "-lLLVM" "LLVM" LLVM_MODULE_LIBS "${LLVM_MODULE_LIBS}")
    STRING(REPLACE " " ";" LLVM_MODULE_LIBS "${LLVM_MODULE_LIBS}")

    # Erase incompatible LLVM linker flags
    SET(LLVM_LFLAGS "")

    # Additional linker settings
    SET(CUDA_LINK "")
    SET(CUDA_COMP "")
    SET(TIME_LINK "")
ELSEIF(APPLE)
    SET(CUDA_LINK "-Xlinker -framework,CUDA")
    SET(CUDA_COMP "-ccbin ${CLANG_EXECUTABLE} -Xcompiler -arch -Xcompiler x86_64 -Xcompiler -stdlib=libstdc++")
    SET(TIME_LINK "")
ELSE()
    SET(CUDA_LINK "-lcuda -lcudart")
    SET(CUDA_COMP "")
    SET(TIME_LINK "-lrt -lm -lstdc++")

    OPTION(USE_NCURSES "Use NCurses library instead of Curses library" OFF)
    IF(USE_NCURSES)
        STRING(REPLACE " -lcurses" " -lncurses" LLVM_LFLAGS "${LLVM_LFLAGS}")
    ENDIF()
ENDIF()


# debug and release builds
STRING(TOUPPER "${CMAKE_BUILD_TYPE}" build)

# Set compiler-specific flags
IF(MSVC)
    # Disable generation of runtime type information
    STRING(REPLACE "/GR " "/GR- " LLVM_CXXFLAGS "${LLVM_CXXFLAGS}")

    # Remove invalid compiler flags
    STRING(REPLACE " -w14062" "" LLVM_CXXFLAGS "${LLVM_CXXFLAGS}")

    # Add required include directories
    IF(NOT LLVM_SOURCE_DIR)
        MESSAGE(FATAL_ERROR "LLVM_SOURCE_DIR has to be an absolute path!")
    ENDIF()
    IF(NOT LLVM_BUILD_DIR)
        MESSAGE(FATAL_ERROR "LLVM_BUILD_DIR has to be an absolute path!")
    ENDIF()
    SET(CLANG_INCLUDE_DIR "tools/clang/include")
    SET(LLVM_CXXFLAGS "/I${LLVM_BUILD_DIR}/${CLANG_INCLUDE_DIR} ${LLVM_CXXFLAGS}")
    SET(LLVM_CXXFLAGS "/I${LLVM_SOURCE_DIR}/${CLANG_INCLUDE_DIR} ${LLVM_CXXFLAGS}")


    IF(build STREQUAL "RELEASE")
        SET(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE}")
    ELSE()
        SET(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG}")
    ENDIF()
ELSE()
    SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

    # Add required include directories
    IF(NOT LLVM_SOURCE_DIR)
        MESSAGE(FATAL_ERROR "LLVM_SOURCE_DIR has to be an absolute path!")
    ENDIF()
    IF(NOT LLVM_BUILD_DIR)
        MESSAGE(FATAL_ERROR "LLVM_BUILD_DIR has to be an absolute path!")
    ENDIF()
    SET(CLANG_INCLUDE_DIR "tools/clang/include")
    SET(LLVM_CXXFLAGS "-I${LLVM_BUILD_DIR}/${CLANG_INCLUDE_DIR} ${LLVM_CXXFLAGS}")
    SET(LLVM_CXXFLAGS "-I${LLVM_SOURCE_DIR}/${CLANG_INCLUDE_DIR} ${LLVM_CXXFLAGS}")


    IF(build STREQUAL "RELEASE")
        SET(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE}")
    ELSE()
        SET(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -Wall -Wunused")
    ENDIF()
ENDIF()


# set include directory, add src directories
INCLUDE_DIRECTORIES(
    ${CMAKE_CURRENT_SOURCE_DIR}/include
    ${CMAKE_CURRENT_BINARY_DIR}/include)
ADD_SUBDIRECTORY(lib)
ADD_SUBDIRECTORY(compiler)
ADD_SUBDIRECTORY(tools)



# configure header files to pass some of the CMake settings to the source code
SET(RUNTIME_INCLUDES "${CMAKE_INSTALL_PREFIX}/include")
SET(DSL_INCLUDES "${CMAKE_INSTALL_PREFIX}/include/dsl")
CONFIGURE_FILE(
    "${HIPACC_SOURCE_DIR}/include/hipacc/Config/config.h.cmake"
    "${HIPACC_BINARY_DIR}/include/hipacc/Config/config.h")
CONFIGURE_FILE(
    "${HIPACC_SOURCE_DIR}/tests/Makefile.cmake"
    "${HIPACC_BINARY_DIR}/tests/Makefile")
CONFIGURE_FILE(
    "${HIPACC_SOURCE_DIR}/tests/Makefile_test.cmake"
    "${HIPACC_BINARY_DIR}/tests/Makefile_test")
CONFIGURE_FILE(
    "${HIPACC_SOURCE_DIR}/tests/Makefile_test_opencv.cmake"
    "${HIPACC_BINARY_DIR}/tests/Makefile_test_opencv")
CONFIGURE_FILE(
    "${HIPACC_SOURCE_DIR}/tests/CMakeLists.txt.cmake"
    "${HIPACC_BINARY_DIR}/tests/CMakeLists.txt"
    @ONLY)


# install dsl and runtime header files
FILE(GLOB dsl_headers "${CMAKE_CURRENT_SOURCE_DIR}/dsl/*.hpp")
FILE(GLOB runtime_headers "${CMAKE_CURRENT_SOURCE_DIR}/runtime/*.hpp")
INSTALL(FILES ${runtime_headers} DESTINATION include)
INSTALL(FILES ${dsl_headers} DESTINATION include/dsl)
# install tests
INSTALL(DIRECTORY tests
    DESTINATION ${CMAKE_INSTALL_PREFIX}
    PATTERN "*.cmake" EXCLUDE)
INSTALL(DIRECTORY ${HIPACC_BINARY_DIR}/tests/
    DESTINATION ${CMAKE_INSTALL_PREFIX}
    PATTERN "*Makefile_test*" EXCLUDE)

# install Makefiles for test cases
FILE(GLOB TEST_DIRS RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}/tests ${CMAKE_CURRENT_SOURCE_DIR}/tests/*)
FOREACH(DIR IN LISTS TEST_DIRS)
    IF(IS_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/tests/${DIR})
        IF(${DIR} MATCHES "opencv_*")
            INSTALL(FILES ${HIPACC_BINARY_DIR}/tests/Makefile_test_opencv
                DESTINATION tests/${DIR}
                RENAME Makefile)
        ELSE(${DIR} MATCHES "opencv_*")
            INSTALL(FILES ${HIPACC_BINARY_DIR}/tests/Makefile_test
                DESTINATION tests/${DIR}
                RENAME Makefile)
        ENDIF(${DIR} MATCHES "opencv_*")
    ENDIF(IS_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/tests/${DIR})
ENDFOREACH(DIR IN LISTS OPENCV_TEST_DIRS)

