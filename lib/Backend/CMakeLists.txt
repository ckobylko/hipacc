SET(CURRENT_HEADER_DIR "${HIPACC_RELATIVE_INCLUDE_DIR}/Backend")

SET(Backend_SOURCES BackendConfigurationManager.cpp)
SET(Backend_HEADERS "${CURRENT_HEADER_DIR}/BackendConfigurationManager.h"
	                "${CURRENT_HEADER_DIR}/BackendExceptions.h")

ADD_LIBRARY(Backend ${Backend_SOURCES} ${Backend_HEADERS})

