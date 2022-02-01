#include "test_macro.fi"

program pow_test
  use, intrinsic :: iso_fortran_env
  use test_mod
  use vari_mod, only : vari, adstack, callstack
  use env_mod
  use grad_mod
  use var_mod
  use pow_mod
  implicit none


  type(var) :: x, y, z
  real(rk) :: a, b, c
  a = 3.0d0

  x = var(2.d0)
  y = x**a
  EXPECT_DBL_EQ(y%val(), 8.d0)
  call y%grad()
  EXPECT_DBL_EQ(x%adj(), 12.d0)
  a = 2.d0
  y = x**a
  call set_zero_all_adj()
  call y%grad()
  EXPECT_DBL_EQ(x%adj(), 4.d0)
  a = 1.d0
  y = x**a
  call set_zero_all_adj()
  call y%grad()
  EXPECT_DBL_EQ(x%adj(), 1.d0)
  a = -1.d0
  y = x**a
  call set_zero_all_adj()
  call y%grad()
  EXPECT_DBL_EQ(x%adj(), -0.25d0)
  a = -2.d0
  y = x**a
  call set_zero_all_adj()
  call y%grad()
  EXPECT_DBL_EQ(x%adj(), -2.d0/8.d0)

  a = 3.d0
  b = log(a)
  y = a ** x
  call set_zero_all_adj()
  call y%grad()
  EXPECT_DBL_EQ(x%adj(), b * 9.d0)

  z = var(3.d0)
  y = z ** x
  call set_zero_all_adj()
  call y%grad()
  EXPECT_DBL_EQ(z%adj(), 2.d0 * 3.d0)
  EXPECT_DBL_EQ(x%adj(), log(3.d0) * 9.d0)

end program pow_test
