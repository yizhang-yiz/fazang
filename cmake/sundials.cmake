# use  -DCMAKE_PREFIX_PATH="$HOME/path/to/sundials-7.8.1"
if(cvodes)

  find_package(SUNDIALS CONFIG REQUIRED)

  if (TARGET SUNDIALS::cvodes_shared)
    set(cvodes_target SUNDIALS::cvodes_shared)
  elseif (TARGET SUNDIALS::cvodes_static)
    set(cvodes_target SUNDIALS::cvodes_static)
  elseif (TARGET SUNDIALS::cvodes)
    set(cvodes_target SUNDIALS::cvodes)
  else()
    message(FATAL_ERROR "No exported CVODES CMake target was found")
  endif()

  if (TARGET SUNDIALS::sundials_nvecserial_shared)
    set(nvec_target SUNDIALS::sundials_nvecserial_shared)
  elseif (TARGET SUNDIALS::sundials_nvecserial_static)
    set(nvec_target SUNDIALS::sundials_nvecserial_static)
  elseif (TARGET SUNDIALS::nvecserial)
    set(nvec_target SUNDIALS::nvecserial)
  else()
    message(FATAL_ERROR "No serial NVector target was found")
  endif()

  message(STATUS ${cvodes_target})
  message(STATUS ${nvec_target})

  add_executable(cvodes_example
    examples/fcvodes_example.f90
  )
  target_link_libraries(cvodes_example
    PRIVATE
    fz
    ${cvodes_target}
    ${nvec_target}
    SUNDIALS::core
    SUNDIALS::fcvodes_mod_shared
    SUNDIALS::fnvecserial_mod_shared
    SUNDIALS::fcore_mod_shared)

endif()
