SET(CURRENT_HEADER_DIR "${HIPACC_RELATIVE_INCLUDE_DIR}/Backend")

SET(Backend_SOURCES BackendConfigurationManager.cpp Renderscript.cpp)
SET(Backend_HEADERS "${CURRENT_HEADER_DIR}/AcceleratorDeviceBase.h"
                    "${CURRENT_HEADER_DIR}/AndroidBase.h"
	                "${CURRENT_HEADER_DIR}/BackendConfigurationManager.h"
	                "${CURRENT_HEADER_DIR}/BackendExceptions.h"
	                "${CURRENT_HEADER_DIR}/CodeGeneratorBaseImplT.h"
	                "${CURRENT_HEADER_DIR}/CommonDefines.h"
	                "${CURRENT_HEADER_DIR}/ICodeGenerator.h"
	                "${CURRENT_HEADER_DIR}/Renderscript.h")

ADD_LIBRARY(Backend ${Backend_SOURCES} ${Backend_HEADERS})

