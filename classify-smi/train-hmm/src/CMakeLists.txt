cmake_minimum_required(VERSION 3.1)
find_package(TrackSelect)

include_directories(
  ${TrackSelect_INCLUDE_DIRS}
  )

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(WSTPLINKDIR "/usr/local/Wolfram/Mathematica/11.3/SystemFiles/Links/WSTP/DeveloperKit")
set(SYS "Linux-x86-64")
set(CADDSDIR "${WSTPLINKDIR}/${SYS}/CompilerAdditions")
set(WSPREP "${CADDSDIR}/wsprep")


set(WF_LIBS "WSTP64i4") # Set this to WSTP64i4 if using a 64-bit system
set(WF_LIB_DIR "${CADDSDIR}")
set(WF_INCLUDE_DIRS "${CADDSDIR}")

set(CMAKE_CXX_FLAGS  "${CMAKE_CXX_FLAGS} -Wall -O3 -pthread")

add_library(train_hmm SHARED train_hmm.cpp)
target_link_libraries(train_hmm ${TrackSelect_LIBRARIES})
set_target_properties(train_hmm PROPERTIES COMPILE_FLAGS "-std=c++11")


add_custom_command(OUTPUT fit_hmmtm.c
  COMMAND ${WSPREP} ${PROJECT_SOURCE_DIR}/fit_hmm.tm -o ${PROJECT_BINARY_DIR}/fit_hmmtm.c)

add_custom_command(OUTPUT fit_hmm.o
  COMMAND ${CMAKE_CXX_COMPILER} -c -O3 -I${WF_INCLUDE_DIRS} -I${PROJECT_SOURCE_DIR} ${PROJECT_BINARY_DIR}/fit_hmmtm.c -o ${PROJECT_BINARY_DIR}/fit_hmm.o
  DEPENDS fit_hmmtm.c)

add_custom_command(OUTPUT fit_hmm
  COMMAND ${CMAKE_CXX_COMPILER} ${PROJECT_BINARY_DIR}/fit_hmm.o -L${WF_LIB_DIR} -lWSTP64i4 -lm -lpthread -lrt -lstdc++ -ldl -luuid -L. -ltrain_hmm -o fit_hmm
  DEPENDS fit_hmm.o)

add_custom_target(fit_hmmt ALL
  DEPENDS fit_hmm)
