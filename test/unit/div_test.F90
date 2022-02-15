#include "test_macro.fi"

program div_test
  use, intrinsic :: iso_fortran_env
  use fazang_test_mod
  use fazang_vari_mod, only : vari, adstack, callstack
  use fazang_grad_mod
  use fazang_env_mod
  use fazang_var_mod
  use fazang_add_mod
  use fazang_mul_mod
  use fazang_div_mod
  implicit none

  type(var) :: x, y1, y2, y3, y4
  real(rk) :: z1 = 2.0d0, z2 = 4.5d0

  x = var(1.5d0)
  x = x / z1
  x = z2 / x
  EXPECT_EQ(callstack%head, 4)
  EXPECT_FLOAT_EQ(callstack%varis(1)%val(), 1.5d0)
  EXPECT_FLOAT_EQ(callstack%varis(2)%val(), 1.5d0 / z1)
  EXPECT_FLOAT_EQ(callstack%varis(3)%val(), z1 * z2 / 1.5d0)
  EXPECT_FLOAT_EQ(x%val(), z1 * z2 / 1.5d0)

  call x%grad()
  EXPECT_FLOAT_EQ(callstack%varis(1)%adj(), -z1 * z2 / (1.5d0 ** 2))
  EXPECT_FLOAT_EQ(callstack%varis(2)%adj(), -z2 * z1 * z1/(1.5d0 ** 2))
  EXPECT_FLOAT_EQ(callstack%varis(3)%adj(), 1.d0)

  y2 = var(2.5d0)
  y1 = x
  y3 = y1 * y2 / y1
  EXPECT_EQ(callstack%head, 7)
  call set_zero_all_adj()
  call y3%grad()
  EXPECT_FLOAT_EQ(y2%adj(), 1.d0)
  EXPECT_FLOAT_EQ(y1%adj(), 0.d0)
  
  y4 = y3 * z1 / (y2 / y1 / z2)
  call set_zero_all_adj()
  call y4%grad()
  EXPECT_FLOAT_EQ(y1%adj(), z1 * z2)
  EXPECT_FLOAT_EQ(y2%adj(), 0.0d0)
  EXPECT_FLOAT_EQ(y3%adj(), z1 * z2 * y1%val() / y2%val())

  y1 = x / x
  call set_zero_all_adj()
  call y1%grad()
  EXPECT_FLOAT_EQ(x%adj(), 0.d0)

end program div_test
