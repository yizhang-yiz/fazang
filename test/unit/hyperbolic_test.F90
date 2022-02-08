#include "test_macro.fi"

program hyperbolic_test
  use, intrinsic :: iso_fortran_env
  use fazang_test_mod
  use fazang_vari_mod, only : vari, adstack
  use fazang_grad_mod
  use fazang_arith_mod
  use fazang_var_mod
  use fazang_sinh_mod
  use fazang_cosh_mod
  use fazang_tanh_mod
  use fazang_asinh_mod
  use fazang_acosh_mod
  use fazang_atanh_mod
  use fazang_sub_mod
  implicit none

  type(var) :: x
  type(var) :: y
  real(rk) :: z(3)

  x = var(2.d0)
  y = sinh(x)
  EXPECT_DBL_EQ(y%val(), sinh(2.d0))
  call y%grad()
  EXPECT_DBL_EQ(x%adj(), cosh(2.d0))

  call set_zero_all_adj()
  y = cosh(x)
  EXPECT_DBL_EQ(y%val(), cosh(2.d0))
  call y%grad()
  EXPECT_DBL_EQ(x%adj(), sinh(2.d0))

  call set_zero_all_adj()
  y = tanh(x)
  EXPECT_DBL_EQ(y%val(), tanh(2.d0))
  call y%grad()
  EXPECT_DBL_EQ(x%adj(), 1.d0/cosh(2.d0)/cosh(2.d0))

  call set_zero_all_adj()
  y = asinh(x)
  EXPECT_DBL_EQ(y%val(), asinh(2.d0))
  call y%grad()
  EXPECT_DBL_EQ(x%adj(), 1.d0/sqrt(5.d0))

  call set_zero_all_adj()
  y = acosh(x)
  EXPECT_DBL_EQ(y%val(), acosh(2.d0))
  call y%grad()
  EXPECT_DBL_EQ(x%adj(), 1.d0/sqrt(3.d0))

  call set_zero_all_adj()
  x = 1.d0 / sqrt(2.d0)
  y = atanh(x)
  EXPECT_DBL_EQ(y%val(), atanh(1.d0/sqrt(2.d0)))
  call y%grad()
  EXPECT_DBL_EQ(x%adj(), 2.d0)

  ! intrinsic version should still work
  z = [sinh(2.d0), cosh(2.d0), tanh(2.d0)]
  z = [asinh(2.d0), asinh(2.d0), atanh(0.1d0)]

end program hyperbolic_test
