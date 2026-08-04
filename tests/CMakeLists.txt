cmake_minimum_required(VERSION 3.28)

add_executable(op1 test_op1.f90 )
target_link_libraries(op1 PRIVATE fz)
add_test(NAME op1 COMMAND op1)

add_executable(op2 test_op2.f90 )
target_link_libraries(op2 PRIVATE fz)
add_test(NAME op2 COMMAND op2)

add_executable(vec test_vec.f90 )
target_link_libraries(vec PRIVATE fz)
add_test(NAME vec COMMAND vec)

add_executable(eval test_eval.f90 )
target_link_libraries(eval PRIVATE fz)
add_test(NAME eval COMMAND eval)

add_executable(dual test_dual.f90 )
target_link_libraries(dual PRIVATE fz)
add_test(NAME dual COMMAND dual)

add_executable(fvar_op1 test_fvar_op1.f90 )
target_link_libraries(fvar_op1 PRIVATE fz)
add_test(NAME fvar_op1 COMMAND fvar_op1)

add_executable(fvar_op2 test_fvar_op2.f90 )
target_link_libraries(fvar_op2 PRIVATE fz)
add_test(NAME fvar_op2 COMMAND fvar_op2)

add_executable(fvar_hessian test_fvar_hessian.f90 )
target_link_libraries(fvar_hessian PRIVATE fz)
add_test(NAME fvar_hessian COMMAND fvar_hessian)

function(add_fz_test test_name)
  add_executable(${test_name} test_${test_name}.f90)
  target_link_libraries(${test_name} PRIVATE fz)
  target_include_directories(${test_name} PUBLIC "${PROJECT_SOURCE_DIR}/src;${PROJECT_SOURCE_DIR}/include")
  add_test(NAME ${test_name} COMMAND ${test_name})
endfunction()

add_fz_test(new_op)
