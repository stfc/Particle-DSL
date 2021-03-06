project(muMD Fortran)

cmake_minimum_required(VERSION 2.8)
set(VERSION_MAJOR "0")                                                                                                                                   
set(VERSION_MINOR "0")
set(VERSION_PATCH "1")
set(AUTHOR "Alin M Elena")
set(AUTHOR_DETAILS "alin@elena.space")
set(DESCRIPTION "muMD engine")
set(src_dir source) 
set(target_name muMD) 



set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake" "${CMAKE_SOURCE_DIR}/cmake/Modules")
if(COMMAND cmake_policy)
  cmake_policy(SET CMP0004 OLD)
  cmake_policy(SET CMP0037 OLD)
endif(COMMAND cmake_policy)

include(BuildOptions)
include(GNUInstallDirs)
include(FeatureSummary)

if(WITH_OPENMP)
   find_package(OpenMP REQUIRED)
   add_definitions(${OpenMP_Fortran_FLAGS})
   add_definitions(-D__OPENMP)
   set(OMP_LINK_FLAGS ${OpenMP_Fortran_FLAGS})
   message(STATUS "Build with OPENMP support")
 else(WITH_OPENMP)
   message(STATUS "Build without OPENMP support")
 endif(WITH_OPENMP)

 if (BUILDER)
  message(STATUS "${BUILDER} is the builder")
else()
  message(STATUS "default value for builder will be used")
endif()
message(STATUS "building on ${HOST}")

if(WITH_EXTRATIME)
   add_definitions("-DCHRONO")
   message(STATUS "Build with extra timing support")
   message(STATUS "Use only for development")
else()
   message(STATUS "Build without extra timing support")
endif()

######################
###documentation
if (DOCS)
   message(STATUS "Build Doxygen API Documentation")
  set(doc_dir doc)
  find_package(Doxygen REQUIRED)
endif()
######################
##set the output folder for libs and bins
set(LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/${CMAKE_INSTALL_LIBDIR})
set(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/${CMAKE_INSTALL_BINDIR})

add_subdirectory(source)

if(DOCS)
  configure_file(${CMAKE_SOURCE_DIR}/cmake/Doxyfile.cmake Doxyfile)
  add_custom_target(docs
    ${DOXYGEN_EXECUTABLE} ${PROJECT_BINARY_DIR}/Doxyfile)
  install(CODE "execute_process(COMMAND ${CMAKE_BUILD_TOOL} docs)")
  install(DIRECTORY ${CMAKE_BINARY_DIR}/doc/html/ DESTINATION ${CMAKE_INSTALL_DOCDIR})
endif(DOCS)

feature_summary(WHAT ALL FATAL_ON_MISSING_REQUIRED_PACKAGES)
