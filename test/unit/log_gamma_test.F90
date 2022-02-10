#include "test_macro.fi"

program log_gamma_test
  use, intrinsic :: iso_fortran_env
  use fazang_test_mod
  use fazang_grad_mod
  use fazang_env_mod
  use fazang_var_mod
  use fazang_log_gamma_mod
  use fazang_digamma_mod
  implicit none

  type(var) :: x, y1

  x = var(0.6d0)
  y1 = log_gamma(x)
  EXPECT_DBL_EQ(y1%val(), log_gamma(0.6d0))
  call y1%grad()
  EXPECT_DBL_EQ(x%adj(), digamma(0.6d0))
  
end program log_gamma_test
