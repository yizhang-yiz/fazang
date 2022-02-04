#include "test_macro.fi"

program logit_test
  use, intrinsic :: iso_fortran_env
  use fazang_test_mod
  use fazang_grad_mod
  use fazang_env_mod
  use fazang_var_mod
  use fazang_add_mod
  use fazang_sub_mod
  use fazang_mul_mod
  use fazang_div_mod
  use fazang_logit_mod
  use fazang_log_mod
  implicit none

  type(var) :: x, y1, y2

  x = var(0.6d0)
  y1 = logit(x)
  y2 = log(x/(1.d0 - x))
  EXPECT_DBL_EQ(y1%val(), log(0.6d0/0.4d0))
  EXPECT_DBL_EQ(y2%val(), log(0.6d0/0.4d0))
  call y1%grad()
  EXPECT_DBL_EQ(x%adj(), 1.d0/(0.6d0 - 0.36d0))
  call set_zero_all_adj()
  call y2%grad()
  EXPECT_DBL_EQ(x%adj(), 1.d0/(0.6d0 - 0.36d0))
  
end program logit_test
