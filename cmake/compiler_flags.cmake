set(fcomp_flags "-O3" "-ffast-math" "-fno-use-linker-plugin" "-finit-integer=0" "-ffpe-summary=none" "-ffree-line-length-none" "$<$<STREQUAL:${CMAKE_SYSTEM_PROCESSOR},x86_64>:-m64>")

add_library(fcompiler_flags INTERFACE)
target_compile_options(fcompiler_flags INTERFACE
  ${fcomp_flags}
)
