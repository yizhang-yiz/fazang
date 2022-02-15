#include "test_macro.fi"

program sum_test
  use, intrinsic :: iso_fortran_env
  use fazang_test_mod
  use fazang_vari_mod, only : vari, adstack, callstack
  use fazang_env_mod
  use fazang_grad_mod
  use fazang_var_mod
  use fazang_sum_mod
  use fazang_add_mod
  use fazang_grad_mod
  implicit none

  type(var) :: x(4), s, a
  real(rk) :: z1(4) = [1.d0, 47.d0, 3.d0, 53.d0]
  integer(ik) :: i

  x = var(z1)
  s = sum(x)
  EXPECT_DBL_EQ(s%val(), sum(z1))
  call s%grad()
  EXPECT_DBL_EQ(s%adj(), 1.d0)
  do i = 1, 4
     EXPECT_DBL_EQ(x(i)%adj(), 1.d0)
  end do

  s = sum(var([val(x), 1.d0]))
  EXPECT_EQ(callstack%head, 12)
  call set_zero_all_adj()
  call s%grad()
  do i = 6, 11
     EXPECT_DBL_EQ(callstack%varis(i)%adj(), 1.d0)
  end do
  do i = 1, 5
     EXPECT_DBL_EQ(callstack%varis(i)%adj(), 0.d0)
  end do
  
  a = var(3.d0)
  x = [a, a, a, var(3.d0)]
  s = sum(x)
  call set_zero_all_adj()
  call s%grad()
  EXPECT_DBL_EQ(a%adj(), 3.d0)

end program sum_test
