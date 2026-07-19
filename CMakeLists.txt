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

set(fz_src
  src/env.f90
  src/vari.f90)

add_library(fz ${fz_src})
target_link_libraries(fz PUBLIC fcompiler_flags)
target_include_directories(fz PUBLIC "${CMAKE_Fortran_MODULE_DIRECTORY};${PROJECT_SOURCE_DIR}/include")

add_executable(ftest ${CMAKE_CURRENT_SOURCE_DIR}/ftest.f90)
target_link_options(ftest PRIVATE $<$<Fortran_COMPILER_ID:GNU>:-fno-use-linker-plugin> $<$<OR:$<PLATFORM_ID:Linux>,$<PLATFORM_ID:Windows>>:-static>)
target_link_libraries(ftest fz)


# # resource
# add_library(resource_obj OBJECT ${resource_src})
# target_include_directories(resource_obj PUBLIC "${CMAKE_Fortran_MODULE_DIRECTORY}")
# target_link_libraries(resource_obj PUBLIC fcompiler_flags)
# # resource that requires c preprocesser directives
# add_library(resource_preproc OBJECT ${resource_preproc_src})
# target_include_directories(resource_preproc PUBLIC "${CMAKE_Fortran_MODULE_DIRECTORY}")
# target_link_libraries(resource_preproc PUBLIC resource_obj fcompiler_preproc_flags)
# add_nm_os_preproc(resource_preproc)
# add_dependencies(resource_preproc resource_obj)
# # isnan & termio are special
# add_library(isnan2 OBJECT resource/ISNAN2.f90)
# target_compile_options(isnan2 PRIVATE -O0 $<$<STREQUAL:${CMAKE_SYSTEM_PROCESSOR},x86_64>:-m64>)
# if (CMAKE_C_COMPILER_ID STREQUAL "IntelLLVM")
#   add_library(termio OBJECT IMPORTED) # empty placeholder for intel's icx
# else()				   # gcc or clang
#   add_library(termio OBJECT ${CMAKE_CURRENT_SOURCE_DIR}/resource/TERMIO.c)
#   add_nm_os_preproc(termio)
# endif()
# if(basic_pr_sizes) # default PRSIZES aka "-prdefault"
#   add_library(pr_global OBJECT resource/PRSIZES_DEFAULT.f90 resource/PRGLOBALP.f90)
#   target_include_directories(pr_global PUBLIC "${CMAKE_Fortran_MODULE_DIRECTORY}")
#   target_link_libraries(pr_global PUBLIC fcompiler_flags)
#   add_library(resource STATIC
#     $<TARGET_OBJECTS:resource_obj>
#     $<TARGET_OBJECTS:resource_preproc>
#     $<TARGET_OBJECTS:isnan2>
#     $<TARGET_OBJECTS:pr_global>
#     $<TARGET_OBJECTS:termio>)
# else()
#   add_library(resource STATIC
#     $<TARGET_OBJECTS:resource_obj>
#     $<TARGET_OBJECTS:resource_preproc>
#     $<TARGET_OBJECTS:isnan2>
#     $<TARGET_OBJECTS:termio>)
# endif()

# # MPI build
# if(mpi)
#   add_library(pnm_mpi OBJECT
#     ${CMAKE_CURRENT_SOURCE_DIR}/mpi/mpi_ling/PNM_MPI.f90)
#   find_package(MPI REQUIRED COMPONENTS Fortran)
#   if(MPI_Fortran_FOUND)
#     message(STATUS "MPI Fortran compiler: " ${MPI_Fortran_COMPILER})
#     message(STATUS "MPI Fortran libraries: " ${MPI_Fortran_LIBRARIES})
#     message(STATUS "MPI Fortran include: " ${MPI_Fortran_INCLUDE_DIRS})
#     message(STATUS "MPI Fortran module: " ${MPI_Fortran_MODULE_DIR})
#     target_link_libraries(pnm_mpi PUBLIC MPI::MPI_Fortran)
#     target_compile_definitions(pnm_mpi PRIVATE __nm_mpi__=1)
#   else()
#     message(FATAL_ERROR "MPI library not found")
#   endif()
# else()
#   add_library(pnm_mpi OBJECT
#     ${CMAKE_CURRENT_SOURCE_DIR}/mpi/PNM_MPID.f90)
# endif()
# target_compile_options(pnm_mpi PUBLIC -fno-range-check)
# target_include_directories(pnm_mpi PUBLIC "${CMAKE_Fortran_MODULE_DIRECTORY};${PROJECT_SOURCE_DIR}/resource")
# target_link_libraries(pnm_mpi PUBLIC fcompiler_flags resource)

# if(nmlib)
#   add_executable(nmtran_presort IMPORTED)
#   add_executable(nmtran IMPORTED)
#   add_executable(ddexpand IMPORTED)
#   foreach(target nmtran nmtran_presort ddexpand)
#     set_property(TARGET ${target} PROPERTY IMPORTED_LOCATION ${CMAKE_CURRENT_SOURCE_DIR}/bin/${target}${CMAKE_EXECUTABLE_SUFFIX})
#   endforeach()
#   add_library(cvode STATIC IMPORTED)
#   set_property(TARGET cvode PROPERTY IMPORTED_LOCATION ${CMAKE_CURRENT_SOURCE_DIR}/lib/${CMAKE_STATIC_LIBRARY_PREFIX}cvode${CMAKE_STATIC_LIBRARY_SUFFIX})
#   add_library(nm STATIC IMPORTED)
#   set_property(TARGET nm PROPERTY IMPORTED_LOCATION ${CMAKE_CURRENT_SOURCE_DIR}/lib/${CMAKE_STATIC_LIBRARY_PREFIX}nm${CMAKE_STATIC_LIBRARY_SUFFIX})
# else()
#   include(nm_src)
#   include(tr_src)

#   # tr (nmtran)
#   # static linked gcc executible is only for linux/windows, not MacOS
#   add_executable(nmtran_presort ${CMAKE_CURRENT_SOURCE_DIR}/util/nmtran_presort.f90)
#   target_link_options(nmtran_presort PRIVATE $<$<Fortran_COMPILER_ID:GNU>:-fno-use-linker-plugin> $<$<OR:$<PLATFORM_ID:Linux>,$<PLATFORM_ID:Windows>>:-static>)
#   add_executable(nmtran ${tr_src})
#   target_include_directories(nmtran PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY})
#   target_link_options(nmtran PRIVATE $<$<Fortran_COMPILER_ID:GNU>:-fno-use-linker-plugin> $<$<OR:$<PLATFORM_ID:Linux>,$<PLATFORM_ID:Windows>>:-static>)
#   target_link_libraries(nmtran PRIVATE fcompiler_flags resource)

#   # NONMEM MPI
#   add_library(parallel OBJECT ${CMAKE_CURRENT_SOURCE_DIR}/util/PARALLEL.f90)
#   target_link_libraries(parallel PRIVATE fcompiler_preproc_flags resource)
#   target_include_directories(parallel PRIVATE "${CMAKE_Fortran_MODULE_DIRECTORY};${CMAKE_CURRENT_SOURCE_DIR}/resource")
#   add_executable(ddexpand ${CMAKE_CURRENT_SOURCE_DIR}/util/ddexpand.f90)
#   target_include_directories(ddexpand PRIVATE "${CMAKE_Fortran_MODULE_DIRECTORY}")
#   target_link_options(ddexpand PRIVATE "$<$<Fortran_COMPILER_ID:GNU>:-fno-use-linker-plugin>")
#   add_nm_os_preproc(ddexpand)

#   # matrix exponential
#   add_library(matexp OBJECT ${pr_matexp_src})
#   target_compile_options(matexp PRIVATE -O3 $<$<STREQUAL:${CMAKE_SYSTEM_PROCESSOR},x86_64>:-m64>)
#   target_include_directories(matexp PRIVATE "${CMAKE_Fortran_MODULE_DIRECTORY};${CMAKE_CURRENT_SOURCE_DIR}/resource")

#   # cvode
#   add_library(lapack OBJECT pr/lapack/LAPACK.f90)
#   add_library(cvodef OBJECT ${cvode_f_src} $<TARGET_OBJECTS:lapack>)
#   target_link_libraries(cvodef PRIVATE fcompiler_flags)
#   if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
#     configure_file(pr/cvode/sundials/sundials_config_gf.h modules/sundials/sundials_config.h COPYONLY)
#   else()
#     configure_file(pr/cvode/sundials/sundials_config_if.h modules/sundials/sundials_config.h COPYONLY)
#   endif()
#   add_library(cvodec OBJECT ${cvode_c_src})
#   target_include_directories(cvodec PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/pr/cvode;${CMAKE_Fortran_MODULE_DIRECTORY}")
#   target_compile_options(cvodec PRIVATE -O3 -ffast-math -fcommon $<$<STREQUAL:${CMAKE_SYSTEM_PROCESSOR},x86_64>:-m64>)
#   add_library(cvode STATIC
#     $<TARGET_OBJECTS:cvodef>
#     $<TARGET_OBJECTS:cvodec>)

#   # nm
#   add_library(nm STATIC ${nm_src}
#     $<TARGET_OBJECTS:lapack>
#     $<TARGET_OBJECTS:matexp>
#     $<TARGET_OBJECTS:parallel>)
#   target_include_directories(nm PUBLIC "${CMAKE_Fortran_MODULE_DIRECTORY};${CMAKE_CURRENT_SOURCE_DIR}/resource")
#   target_link_libraries(nm PUBLIC fcompiler_flags resource)
# endif()

# include(model)		# use function

# if(tag)
#   include(pack)		# use function
#   build_nm_pack(${tag})
# endif()

# if(test)
#   include(CTest)
#   add_subdirectory(tests)
# elseif(model)
#   cmake_path(GET model STEM nmexec)
#   build_nm_model(${model} ${nmexec})
# else()
#   message(WARNING "Missing building mode (test/model). Configured for packaging.")
# endif()
