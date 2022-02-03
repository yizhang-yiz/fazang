#include "test_macro.fi"

program log_sum_exp_test
  use, intrinsic :: iso_fortran_env
  use fazang_test_mod
  use fazang_env_mod
  use fazang_grad_mod
  use fazang_var_mod
  use fazang_log_sum_exp_mod
  use fazang_log_mod
  use fazang_sum_mod
  use fazang_exp_mod
  use fazang_grad_mod
  implicit none

  type(var) :: x(4), s
  real(rk) :: z(4) = [1.d0, 7.d0, 3.d0, 5.d0], lse_z
  integer(ik) :: i

  lse_z = log(sum(exp(z)))
  x = var(z)
  s = log_sum_exp(x)
  EXPECT_DBL_EQ(s%val(), lse_z)
  call s%grad()
  do i = 1, size(z)
     EXPECT_DBL_EQ(x(i)%adj(), exp(z(i))/sum(exp(z)))
  end do
  s= log(sum(exp(x)))
  EXPECT_DBL_EQ(s%val(), lse_z)
  call set_zero_all_adj()
  call s%grad()
  do i = 1, size(z)
     EXPECT_DBL_EQ(x(i)%adj(), exp(z(i))/sum(exp(z)))
  end do

  z(3) = 1.e8
  lse_z = log(sum(exp(z)))
  x = var(z)
  s = log_sum_exp(x)
  EXPECT_DBL_EQ(s%val(), z(3))
  call set_zero_all_adj()
  call s%grad()
  do i = 1, size(z)
     EXPECT_DBL_EQ(x(i)%adj(), exp(z(i) - log_sum_exp(z)))
  end do
  
end program log_sum_exp_test
