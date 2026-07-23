cmake_minimum_required(VERSION 3.28)

add_executable(op1 op1.f90 )
target_link_libraries(op1 PRIVATE fz)
add_test(NAME op1
    COMMAND op1)
