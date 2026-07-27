cmake_minimum_required(VERSION 3.28)

add_executable(op1 op1.f90 )
target_link_libraries(op1 PRIVATE fz)
add_test(NAME op1 COMMAND op1)

add_executable(op2 op2.f90 )
target_link_libraries(op2 PRIVATE fz)
add_test(NAME op2 COMMAND op2)

add_executable(vec vec.f90 )
target_link_libraries(vec PRIVATE fz)
add_test(NAME vec COMMAND vec)

add_executable(eval eval.f90 )
target_link_libraries(eval PRIVATE fz)
add_test(NAME eval COMMAND eval)
