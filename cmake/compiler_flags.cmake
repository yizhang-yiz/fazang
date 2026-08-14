set(fcomp_flags "-O3" "-march=native" "-ffast-math" "-finit-integer=0" "-ffree-line-length-none" "$<$<STREQUAL:${CMAKE_SYSTEM_PROCESSOR},x86_64>:-m64>")

add_library(fcompiler_flags INTERFACE)
target_compile_options(fcompiler_flags INTERFACE
  ${fcomp_flags}
)
