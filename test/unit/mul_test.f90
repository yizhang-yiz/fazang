#include "test_macro.fi"

program mul_test
  use, intrinsic :: iso_fortran_env
  use test_mod
  use vari_mod, only : vari, adstack, callstack, vari_at
  use grad_mod
  use env_mod
  use var_mod
  use mul_mod
  use add_mod
  implicit none

  type(var) :: x, y1, y2, y3, y4
  type(var) :: q1(2), q2(2), q(2)
  real(rk) :: z1 = 2.0d0, z2 = 3.9d0
  type(vari), pointer :: vp

  x = var(1.5d0)
  x = x * z1
  x = z2 * x
  EXPECT_EQ(callstack%head, 4)
  EXPECT_FLOAT_EQ(x%val(), 1.5d0 * z1 * z2)

  call x%grad()
  EXPECT_DBL_EQ(callstack%varis(3)%adj(), 1.d0)
  EXPECT_DBL_EQ(callstack%varis(2)%adj(), z2)
  EXPECT_DBL_EQ(callstack%varis(1)%adj(), z1 * z2)

  y2 = var(2.6d0)
  y1 = x
  y3 = y2 * y1
  EXPECT_EQ(callstack%head, 6)
  EXPECT_DBL_EQ(y3%val(), 2.6d0 * x%val())
  call set_zero_all_adj()
  vp => vari_at(y3%vi)
  call vp%set_adj(2.5d0)
  call vp%chain()
  EXPECT_DBL_EQ(y2%adj(), 2.5d0 * y1%val())
  EXPECT_DBL_EQ(y1%adj(), 2.5d0 * y2%val())
  EXPECT_DBL_EQ(x%adj(), 2.5d0 * y2%val())
  
  ! note that y3 = y2 * y1
  y4 = y3 * y2 + y1 * y2
  call set_zero_all_adj()
  call y4%grad()
  EXPECT_DBL_EQ(y3%adj(), y2%val())
  EXPECT_DBL_EQ(y2%adj(), 2.d0 * y2%val() * y1%val() + y1%val())
  EXPECT_DBL_EQ(y1%adj(), y2%val() + y2%val()**2)

  ! vectorized version
  q1 = var([1.2d0, 3.d0])
  q2 = var([2.2d0, 4.d0])
  q = q1 * q2
  call set_zero_all_adj()
  call q(1)%grad()
  EXPECT_DBL_EQ(q1%adj(), ([q2(1)%val(), 0.0d0]))
  EXPECT_DBL_EQ(q2%adj(), ([q1(1)%val(), 0.0d0]))
  call set_zero_all_adj()
  call q(2)%grad()
  EXPECT_DBL_EQ(q1%adj(), ([0.0d0, q2(2)%val()]))
  EXPECT_DBL_EQ(q2%adj(), ([0.0d0, q1(2)%val()]))

  q = 2.d0 * q1
  EXPECT_DBL_EQ(q%val(), ([2.d0 * 1.2d0, 2.d0 * 3.d0]))
  q = q1 * 3.4d0
  EXPECT_DBL_EQ(q%val(), ([3.4d0 * 1.2d0, 3.4d0 * 3.d0]))
  call set_zero_all_adj()
  vp => vari_at(q(2)%vi)
  call vp%init_dependent()
  call vp%chain()
  EXPECT_DBL_EQ(q1%adj(), ([0.0d0, 3.4d0]))
  EXPECT_DBL_EQ(q2%adj(), ([0.0d0, 0.0d0]))

end program mul_test
