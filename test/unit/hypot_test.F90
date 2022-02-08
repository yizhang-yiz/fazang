#include "test_macro.fi"

program hypot_test
  use, intrinsic :: iso_fortran_env
  use fazang_test_mod
  use fazang_vari_mod, only : vari, adstack
  use fazang_grad_mod
  use fazang_arith_mod
  use fazang_var_mod
  use fazang_hypot_mod
  use fazang_sub_mod
  implicit none

  type(var) :: y(2)
  type(var) :: x
  real(rk) :: z(2) = [3.d0, 4.d0]

  y = var(z)
  x = hypot(y(1), y(2))
  EXPECT_DBL_EQ(x%val(), 5.d0)
  call x%grad()
  EXPECT_DBL_EQ(y%adj(), ([3.d0/5.d0, 4.d0/5.d0]))

  call set_zero_all_adj()
  x = hypot(y(1), z(2))
  EXPECT_DBL_EQ(x%val(), 5.d0)
  call x%grad()
  EXPECT_DBL_EQ(y%adj(), ([3.d0/5.d0, 0.d0]))

  call set_zero_all_adj()
  x = hypot(z(1), y(2))
  EXPECT_DBL_EQ(x%val(), 5.d0)
  call x%grad()
  EXPECT_DBL_EQ(y%adj(), ([0.d0, 4.d0/5.d0]))

end program hypot_test
