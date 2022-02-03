#include "test_macro.fi"

program alloc_tape_test
  use, intrinsic :: iso_fortran_env
  use iso_c_binding
  use fazang_test_mod
  use fazang_env_mod
  use fazang_vari_mod
  use fazang_var_mod
  use fazang_tape_mod
  use fazang_dot_product_mod

  implicit none

  integer(ik), parameter :: n = 4100
  integer :: i

  type(var) :: x(n), y(n), z(n)
  real(rk) :: a(n)=[(i, i = 1, n)]

  ! todo: check #ifdef FZ_INIT_STACK_LEN
  EXPECT_EQ(callstack % vari_stack_size(), 0)
  x = var(a)
  EXPECT_EQ(callstack % vari_stack_size(), 8 * init_vari_stack_size)
  EXPECT_EQ(callstack % head, n + 1)

  y = var(a)
  EXPECT_EQ(callstack % vari_stack_size(), 16 * init_vari_stack_size)
  z = dot_product(x, y)
  EXPECT_EQ(callstack % vari_stack_size(), 16 * init_vari_stack_size)
  EXPECT_EQ(callstack % head, 2 * n + 2)

  
end program alloc_tape_test
