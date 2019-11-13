cmake_minimum_required(VERSION 2.8)
project(track-select-trash)

set(CMAKE_CXX_FLAGS  "${CMAKE_CXX_FLAGS} -Wall -g -fopenmp" )

set(BUILD_SHARED_LIBS ON)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/Modules") 
list(APPEND CMAKE_MODULE_PATH "$ENV{ROOTSYS}/etc/cmake/")

find_package(MSMM REQUIRED)
find_package(Boost COMPONENTS system program_options iostreams filesystem REQUIRED)
find_package(HDF5 COMPONENTS CXX REQUIRED)

include_directories(
  ${PROJECT_SOURCE_DIR}/include
  ${MSMM_INCLUDE_DIRS}
  ${Boost_INCLUDE_DIR}
  ${HDF5_INCLUDE_DIRS})

add_executable(import-smi-random import-smi-random.cpp)
target_link_libraries(import-smi-random
  ${MSMM_LIBRARIES}
  ${HDF5_CXX_LIBRARIES})
set_target_properties(import-smi-random PROPERTIES
  COMPILE_FLAGS "-std=gnu++0x  -DGNUXX11 -DBOOST_NO_CXX11_SCOPED_ENUMS")


enable_testing()
