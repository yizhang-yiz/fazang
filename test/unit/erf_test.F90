#include "test_macro.fi"

program erf_test
  use, intrinsic :: iso_fortran_env
  use fazang_test_mod
  use fazang_vari_mod, only : vari, adstack, vari_at
  use fazang_grad_mod
  use fazang_var_mod
  use fazang_erf_mod
  implicit none

  type(var) :: x, y1, y2, y3

  x = var(-0.3d0)
  y1 = erf(x)
  EXPECT_DBL_EQ(y1%val(), -0.3286267594591274d0)
  call y1%grad()
  EXPECT_DBL_EQ(x%adj(), (2/sqrt(pi) * exp(-0.09d0)))

  x = var(0.d0)  
  y2 = erf(x)
  EXPECT_DBL_EQ(y2%val(), 0.d0)
  x = -100.d0
  y3 = erf(x)
  EXPECT_DBL_EQ(y3%val(), -1.d0)

end program erf_test
