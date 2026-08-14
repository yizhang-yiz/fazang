cmake_minimum_required(VERSION 3.28)

set(CMAKE_Fortran_FLAGS_DEBUG "" CACHE STRING "" FORCE)
set(CMAKE_Fortran_FLAGS_RELEASE "" CACHE STRING "" FORCE)

# numeric options from nmtran & nonmem
set(fz_adstack_size "0" CACHE STRING "AD storage in Mbytes")
if (fz_adstack_size GREATER 2000)
  message(SEND_ERROR "fz_adstack_size not allowed to be over 2GB")
endif()

# set(CMAKE_Fortran_COMPILER "/home/zhang6_admin/intel/oneapi/compiler/latest/bin/ifx")
# set(CMAKE_C_COMPILER "/home/zhang6_admin/intel/oneapi/compiler/latest/bin/icx")
if(NOT DEFINED CMAKE_Fortran_COMPILER)
  set(CMAKE_Fortran_COMPILER gfortran)
endif()
if(NOT DEFINED CMAKE_C_COMPILER)
  set(CMAKE_C_COMPILER gcc)
endif()

project(fazang
  DESCRIPTION "Fazang: a Fortran lib for autodiff."
  LANGUAGES Fortran C)
set(PROJECT_VERSION 0.0.1)

add_compile_options($<$<AND:$<CONFIG:Debug>,$<PLATFORM_ID:Linux>>:-fsanitize=address>)
add_compile_options($<$<AND:$<CONFIG:Debug>,$<PLATFORM_ID:Linux>>:-fno-omit-frame-pointer>)
add_link_options($<$<AND:$<CONFIG:Debug>,$<PLATFORM_ID:Linux>>:-fsanitize=address>)
add_link_options($<$<AND:$<CONFIG:Debug>,$<PLATFORM_ID:Linux>>:-fno-omit-frame-pointer>)

if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  set(CMAKE_BUILD_TYPE "Release" CACHE STRING "Choose the type of build." FORCE)
endif()

list(APPEND CMAKE_MODULE_PATH "${CMAKE_BINARY_DIR};${CMAKE_CURRENT_SOURCE_DIR}/cmake")

include(compiler_flags)

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modules)

include(CheckIPOSupported)
check_ipo_supported(
  RESULT ipo_supported
  OUTPUT ipo_error
  LANGUAGES Fortran
)

set(fz_src
  OBJECT
  src/env.f90
  src/vari.f90 src/vari_op.f90 src/vari_prob.f90 src/var.f90
  src/fvari.f90 src/fvari_op.f90 src/fvar.f90
  src/eval.f90
  src/relation.f90
  src/vari_builder.f90
  src/fazang.f90
  )

add_library(fz STATIC ${fz_src})
target_link_libraries(fz PUBLIC fcompiler_flags)
target_include_directories(fz PUBLIC "${CMAKE_Fortran_MODULE_DIRECTORY};${PROJECT_SOURCE_DIR}/include")

add_executable(ftest /home/zhangyi6_admin/work/sandbox/temp/rosenbrock.f90)
target_link_libraries(ftest PUBLIC fz fcompiler_flags)
target_include_directories(ftest PUBLIC "${CMAKE_Fortran_MODULE_DIRECTORY};${PROJECT_SOURCE_DIR}/src;${PROJECT_SOURCE_DIR}/include")

add_executable(htest htest.f90)
target_link_libraries(htest PUBLIC fz fcompiler_flags)
target_include_directories(htest PUBLIC "${CMAKE_Fortran_MODULE_DIRECTORY};${PROJECT_SOURCE_DIR}/src;${PROJECT_SOURCE_DIR}/include")

if (ipo_supported)
  set_property(TARGET fz ftest htest
    PROPERTY INTERPROCEDURAL_OPTIMIZATION_RELEASE TRUE)
else()
  message(WARNING "Fortran IPO/LTO unavailable: ${ipo_error}")
endif()

if(test)
  include(CTest)
  add_subdirectory(tests)
elseif(model)
  cmake_path(GET model STEM fzexec)
  get_filename_component(model_path ${model} ABSOLUTE BASE_DIR ${CMAKE_BINARY_DIR})
  add_executable(${fzexec} ${model})
  target_link_options(${fzexec} PRIVATE $<$<Fortran_COMPILER_ID:GNU>:-fno-use-linker-plugin> $<$<OR:$<PLATFORM_ID:Linux>,$<PLATFORM_ID:Windows>>:-static>)
  target_link_libraries(${fzexec} fz)
else()
  message(WARNING "Missing building mode (test/model).")
endif()
