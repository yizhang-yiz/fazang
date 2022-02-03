#include "test_macro.fi"

program exp_test
  use, intrinsic :: iso_fortran_env
  use fazang_test_mod
  use fazang_vari_mod, only : vari, adstack, callstack, vari_at
  use fazang_grad_mod
  use fazang_exp_mod
  implicit none

  type(var) :: x, y1, y2, y3
  real(rk) :: z1, z2
  type(vari), pointer :: vp

  x = var(1.5d0)
  y1 = exp(x)
  call y1%grad()
  EXPECT_FLOAT_EQ(y1%val(), exp(1.5d0))
  EXPECT_FLOAT_EQ(y1%adj(), 1.0d0)
  EXPECT_FLOAT_EQ(x%adj(), exp(1.5d0))
  EXPECT_EQ(callstack%head, 3)

  z1 = x%val()
  z2 = exp(z1)
  EXPECT_FLOAT_EQ(y1%val(), z2)

  y3 = var(1.5d0)
  y2 = exp(y1)
  call callstack%set_zero_all_adj()
  EXPECT_FLOAT_EQ(y1%adj(), 0.0d0)
  EXPECT_FLOAT_EQ(x%adj(), 0.0d0)
  EXPECT_FLOAT_EQ(y2%val(), exp(exp(1.5d0)))
  
  vp => vari_at(y2%vi)
  call vp%init_dependent()
  call vp%chain()
  EXPECT_FLOAT_EQ(y3%adj(), 0.0d0)
  EXPECT_FLOAT_EQ(x%adj(), 0.0d0)
  EXPECT_FLOAT_EQ(y2%adj(), 1.0d0)
  EXPECT_FLOAT_EQ(y1%adj(), exp(exp(1.5d0)))

end program exp_test
