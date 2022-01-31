#include "test_macro.fi"

program sum_test
  use, intrinsic :: iso_fortran_env
  use test_mod
  use vari_mod, only : vari, adstack, callstack
  use env_mod
  use grad_mod
  use fazang
  implicit none

  type(var) :: x(4), s
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
  
end program sum_test
