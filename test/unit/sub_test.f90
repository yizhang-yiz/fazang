#include "test_macro.fi"

program sub_test
  use, intrinsic :: iso_fortran_env
  use test_mod
  use vari_mod
  use fazang, only : var, set_zero_all_adj, operator(-), operator(+), sin, cos
  implicit none

  type(var) :: x, y1, y2, y3, y4
  real(rk) :: z1 = 2.4d0, z2 = 3.9d0

  x = var(1.5d0)
  x = x - z1
  y1 = x
  EXPECT_EQ(callstack%head, 3)
  EXPECT_FLOAT_EQ(callstack%val(1), 1.5d0)
  EXPECT_FLOAT_EQ(callstack%val(2), 1.5d0 - z1)

  call y1%vi%set_adj(0.4d0)
  call y1%vi%chain()
  EXPECT_FLOAT_EQ(callstack%adj(1), x%adj())
  EXPECT_FLOAT_EQ(callstack%adj(1), y1%adj())
  EXPECT_FLOAT_EQ(callstack%adj(1), 0.4d0)

  y2 = var(2.6d0)
  y3 = y2 - y1
  EXPECT_EQ(callstack%head, 5)
  EXPECT_FLOAT_EQ(callstack%val(4), y2%val() - y1%val())

  call set_zero_all_adj()
  call y3%vi%init_dependent()
  call y3%vi%chain()
  EXPECT_FLOAT_EQ(y2%adj(), 1.d0)
  EXPECT_FLOAT_EQ(y1%adj(), -1.d0)

  y4 = y1 - z1 - y2 - z2
  EXPECT_EQ(callstack%head, 8)
  call set_zero_all_adj()
  call y4%grad()
  EXPECT_FLOAT_EQ(y1%adj(), 1.d0)
  EXPECT_FLOAT_EQ(y2%adj(), -1.d0)

  call callstack%set_zero_all_adj()
  y4 = sin(y1) - sin(z1) - cos(y2) + cos(z2)
  EXPECT_EQ(callstack%head, 13)
  call set_zero_all_adj()
  call y4%grad()
  EXPECT_FLOAT_EQ(y4%val(), sin(y1%val()) - sin(z1) - cos(y2%val()) + cos(z2))
  EXPECT_FLOAT_EQ(y1%adj(), cos(y1%val()))
  EXPECT_FLOAT_EQ(y2%adj(), sin(y2%val()))

  y4 = -y3
  call y4%grad()
  EXPECT_FLOAT_EQ(y3%adj(), -1.0d0)

end program sub_test
