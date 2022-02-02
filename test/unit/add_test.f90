#include "test_macro.fi"

program add_test
  use, intrinsic :: iso_fortran_env
  use test_mod
  use vari_mod, only : vari, adstack, callstack, vari_at
  use grad_mod
  use var_mod
  use cos_mod
  use sin_mod
  use add_mod
  implicit none

  type(var) :: x, y1, y2, y3, y4
  type(var) :: q1(2), q2(2), q(2)
  real(rk) :: z1 = 2.4d0, z2 = 3.9d0
  type(vari), pointer :: vp

  x = var(1.5d0)
  x = x + z1
  y1 = x
  EXPECT_EQ(callstack%head, 3)
  EXPECT_FLOAT_EQ(x%val(), 1.5d0 + z1)

  vp => vari_at(x%vi)
  call vp%set_adj(0.4d0)
  call vp%chain()
  EXPECT_FLOAT_EQ(callstack % stack % adj(1), x%adj())

  y2 = var(2.6d0)
  y3 = y2 + y1
  EXPECT_EQ(callstack%head, 5)
  EXPECT_FLOAT_EQ(y3%val(), y2%val() + y1%val())

  vp => vari_at(y3%vi)
  call vp%init_dependent()
  call vp%chain()
  EXPECT_FLOAT_EQ(y2%adj(), 1.d0)
  EXPECT_FLOAT_EQ(y1%adj(), 1.4d0)

  call callstack%set_zero_all_adj()
  EXPECT_FLOAT_EQ(y2%adj(), 0.d0)
  EXPECT_FLOAT_EQ(y1%adj(), 0.d0)
  y4 = y1 + z1 + y2 + z2
  call y4%grad()
  EXPECT_FLOAT_EQ(y2%adj(), 1.d0)
  EXPECT_FLOAT_EQ(y1%adj(), 1.d0)

  call set_zero_all_adj()
  y4 = sin(y1) + sin(z1) + cos(y2) + cos(z2)
  EXPECT_EQ(callstack%head, 13)
  call y4%grad()
  EXPECT_FLOAT_EQ(y4%val(), sin(y1%val()) + sin(z1) + cos(y2%val()) + cos(z2))
  EXPECT_FLOAT_EQ(y1%adj(), cos(y1%val()))
  EXPECT_FLOAT_EQ(y2%adj(), -sin(y2%val()))

  y4 = +y3
  call y4%grad()
  EXPECT_FLOAT_EQ(y3%adj(), 1.0d0)

  ! vectorized version
  q1 = var([1.2d0, 3.d0])
  q2 = var([2.2d0, 4.d0])
  q = q1 + q2
  call set_zero_all_adj()
  vp => vari_at(q(1)%vi)
  call vp%set_adj(1.3d0)
  call vp%chain()
  EXPECT_DBL_EQ(q1%adj(), ([1.3d0, 0.0d0]))
  EXPECT_DBL_EQ(q2%adj(), ([1.3d0, 0.0d0]))
  call set_zero_all_adj()
  vp => vari_at(q(2)%vi)
  call vp%set_adj(1.3d0)
  call vp%chain()
  EXPECT_DBL_EQ(q1%adj(), ([0.0d0, 1.3d0]))
  EXPECT_DBL_EQ(q1%adj(), ([0.0d0, 1.3d0]))

end program add_test
