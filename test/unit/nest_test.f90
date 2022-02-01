#include "test_macro.fi"

program nest_test
  use, intrinsic :: iso_fortran_env
  use iso_c_binding
  use test_mod
  use env_mod
  use var_mod
  use grad_mod
  use vari_mod, only : adstack, callstack, vari
  use tape_mod

  implicit none

  type(var) :: v(3), u(4), a, b

  EXPECT_EQ(callstack % stack % head, 1)
  v = var([1.d0, 2.d0, 3.d0])
  EXPECT_EQ(callstack % stack % head, 19)
  call begin_nested()
  EXPECT_EQ(callstack % stack % head, 19)
  u = var([1.d0, 2.d0, 3.d0, 4.d0])
  EXPECT_EQ(callstack % stack % head, 43)
  call begin_nested()
  EXPECT_EQ(callstack % stack % head, 43)
  b = var(443.d0)
  a = var(232.d0)
  EXPECT_EQ(callstack % stack % head, 55)
  call end_nested()
  EXPECT_EQ(callstack % stack % head, 43)
  EXPECT_DBL_EQ(a%val(), 0d0)
  EXPECT_DBL_EQ(b%val(), 0d0)
  call end_nested()  
  EXPECT_EQ(callstack % stack % head, 19)
  EXPECT_DBL_EQ(u%val(), ([0d0,0d0,0d0,0d0]))
  EXPECT_DBL_EQ(v%val(), ([1.d0, 2.d0, 3.d0]))
  
  ! should be a dummy call
  call end_nested()
  EXPECT_EQ(callstack % stack % head, 19)
  EXPECT_DBL_EQ(v%val(), ([1.d0, 2.d0, 3.d0]))

end program nest_test
