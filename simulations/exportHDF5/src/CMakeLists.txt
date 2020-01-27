cmake_minimum_required(VERSION 2.8)
project(ExportHDF5)

set(CMAKE_CXX_FLAGS  "${CMAKE_CXX_FLAGS} -Wall -O3 -pthread" )

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(WSTPLINKDIR "$ENV{MATHEMATICA_ROOT}/SystemFiles/Links/WSTP/DeveloperKit")

set(SYS "Linux-x86-64")
set(CADDSDIR "${WSTPLINKDIR}/${SYS}/CompilerAdditions")
set(WSPREP "${CADDSDIR}/wsprep")

set(WF_LIBS "WSTP64i4") # Set this to WSTP64i4 if using a 64-bit system
set(WF_LIB_DIR "${CADDSDIR}")
set(WF_INCLUDE_DIRS "${CADDSDIR}")


find_package(HDF5 COMPONENTS CXX REQUIRED)

#Create library build files
include_directories(
#  ${PROJECT_SOURCE_DIR}/include
  ${HDF5_INCLUDE_DIRS})

add_library(export_hdf5 SHARED export_hdf5.cpp)
target_link_libraries(export_hdf5 ${HDF5_LIBRARIES})
set_target_properties(export_hdf5 PROPERTIES COMPILE_FLAGS "-std=c++11")

add_custom_command(OUTPUT export_flimptm.c
  COMMAND ${WSPREP} ${PROJECT_SOURCE_DIR}/export_flimp.tm -o ${PROJECT_BINARY_DIR}/export_flimptm.c)

add_custom_command(OUTPUT export_flimp.o
  COMMAND ${CMAKE_CXX_COMPILER} -c -O3 -I${WF_INCLUDE_DIRS} -I${PROJECT_SOURCE_DIR} ${PROJECT_BINARY_DIR}/export_flimptm.c -o ${PROJECT_BINARY_DIR}/export_flimp.o
  DEPENDS export_flimptm.c)

add_custom_command(OUTPUT export_flimp
  COMMAND ${CMAKE_CXX_COMPILER} ${PROJECT_BINARY_DIR}/export_flimp.o -L${WF_LIB_DIR} -lWSTP64i4 -lm -lpthread -lrt -lstdc++ -ldl -luuid -L. -lexport_hdf5 -o export_flimp
  DEPENDS export_flimp.o)

add_custom_target(export_flimpt ALL
  DEPENDS export_flimp)
