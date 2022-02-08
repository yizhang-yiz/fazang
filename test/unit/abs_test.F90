#include "test_macro.fi"

program abs_test
  use, intrinsic :: iso_fortran_env
  use fazang_test_mod
  use fazang_vari_mod, only : vari, adstack
  use fazang_grad_mod
  use fazang_arith_mod
  use fazang_var_mod
  use fazang_abs_mod
  use fazang_sub_mod
  implicit none

  type(var) :: x, y1, y2, y3, y4

  x = var(0.3d0)
  y1 = abs(x)
  EXPECT_DBL_EQ(y1%val(), 0.3d0)
  call y1%grad()
  EXPECT_DBL_EQ(x%adj(), 1.d0)

  call set_zero_all_adj()
  x = var(-0.3d0)
  y2 = abs(x)
  EXPECT_DBL_EQ(y2%val(), 0.3d0)
  call y2%grad()
  EXPECT_DBL_EQ(x%adj(), -1.d0)

  call set_zero_all_adj()
  x = var(0.d0)
  y3 = abs(x)
  EXPECT_DBL_EQ(y3%val(), 0.0d0)
  call y2%grad()
  EXPECT_DBL_EQ(x%adj(), 0.d0)

  call set_zero_all_adj()
  x = var(nan64)
  y4 = abs(x)
  EXPECT_TRUE(is_nan(y4%val()))
  call y4%grad()
  EXPECT_TRUE(is_nan(x%adj()))

end program abs_test
