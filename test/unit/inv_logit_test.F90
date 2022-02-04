#include "test_macro.fi"

program inv_logit_test
  use, intrinsic :: iso_fortran_env
  use fazang_test_mod
  use fazang_grad_mod
  use fazang_env_mod
  use fazang_var_mod
  use fazang_add_mod
  use fazang_sub_mod
  use fazang_mul_mod
  use fazang_div_mod
  use fazang_inv_logit_mod
  use fazang_exp_mod
  implicit none

  type(var) :: x, y1, y2
  real(rk), parameter :: a1 = 0.6d0, a2 = -1.2d0

  x = var(a1)
  y1 = inv_logit(x)
  y2 = exp(x)/(1.d0 + exp(x))
  EXPECT_DBL_EQ(y1%val(), exp(a1)/(1.d0 + exp(a1)))
  EXPECT_DBL_EQ(y2%val(), exp(a1)/(1.d0 + exp(a1)))
  call y1%grad()
  EXPECT_DBL_EQ(x%adj(), exp(a1) / (1.d0 + exp(a1))**2)
  call set_zero_all_adj()
  call y2%grad()
  EXPECT_DBL_EQ(x%adj(), exp(a1) / (1.d0 + exp(a1))**2)
  
  x = var(a2)
  y1 = inv_logit(x)
  y2 = exp(x)/(1.d0 + exp(x))
  EXPECT_DBL_EQ(y1%val(), exp(a2)/(1.d0 + exp(a2)))
  EXPECT_DBL_EQ(y2%val(), exp(a2)/(1.d0 + exp(a2)))
  call y1%grad()
  EXPECT_DBL_EQ(x%adj(), exp(a2) / (1.d0 + exp(a2))**2)
  call set_zero_all_adj()
  call y2%grad()
  EXPECT_DBL_EQ(x%adj(), exp(a2) / (1.d0 + exp(a2))**2)


end program inv_logit_test
