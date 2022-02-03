#include "test_macro.fi"

program alloc_tape_test
  use, intrinsic :: iso_fortran_env
  use iso_c_binding
  use test_mod
  use env_mod
  use vari_mod
  use var_mod
  use tape_mod
  use dot_product_mod

  implicit none

  integer(ik), parameter :: n = 4100
  integer :: i, j, k

  type(var) :: x(n), y(n), z(n)
  real(rk) :: a(n)=[(i, i = 1, n)]

  EXPECT_EQ(callstack % vari_stack_size(), 0)
  x = var(a)
  ! write(*, *) "taki test: ", FZ_INIT_STACK_LEN
#ifdef FZ_INIT_STACK_LEN
  ! FZ_INIT_STACK_LEN = 4
#else
  EXPECT_EQ(callstack % vari_stack_size(), 8 * init_vari_stack_size)
#endif
  EXPECT_EQ(callstack % head, n + 1)

  ! y = var(a)
  ! EXPECT_EQ(callstack % vari_stack_size(), 16 * init_vari_stack_size)
  ! z = dot_product(x, y)
  ! EXPECT_EQ(callstack % vari_stack_size(), 16 * init_vari_stack_size)


  write(*, *) "taki test: ", callstack % stack % storage_size
  
end program alloc_tape_test
