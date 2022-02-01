#include "test_macro.fi"

program cos_test
  use, intrinsic :: iso_fortran_env
  use test_mod
  use vari_mod, only : vari, adstack, callstack
  use grad_mod
  use var_mod
  use cos_mod
  use acos_mod
  use sin_mod
  use asin_mod
  use tan_mod
  use atan_mod
  implicit none

  type(var) :: x, y1, y2, y3
  real(rk) :: z1, z2
  integer(ik) :: id(1)

  x = var(1.4d0)
  y1 = cos(x)
  call y1%vi%init_dependent()
  call y1%vi%chain()
  EXPECT_FLOAT_EQ(y1%val(), cos(1.4d0))
  EXPECT_FLOAT_EQ(y1%adj(), 1.0d0)
  EXPECT_FLOAT_EQ(x%adj(), -sin(1.4d0))
  EXPECT_EQ(callstack%head, 3)

  z1 = x%val()
  z2 = cos(z1)
  EXPECT_FLOAT_EQ(y1%val(), z2)

  y3 = var(1.5d0)
  y2 = cos(y1)
  call set_zero_all_adj()
  EXPECT_FLOAT_EQ(y1%adj(), 0.0d0)
  EXPECT_FLOAT_EQ(x%adj(), 0.0d0)
  EXPECT_FLOAT_EQ(y2%val(), cos(cos(1.4d0)))  
  id = y2%vi%operand_index()
  EXPECT_EQ(id(1), y1%vi%i)
  EXPECT_EQ(y1%vi%n_operand(), 1)
  EXPECT_EQ(y3%vi%n_operand(), 0)

  call set_zero_all_adj()
  call y2%vi%init_dependent()
  call y2%vi%chain()
  EXPECT_FLOAT_EQ(y3%adj(), 0.0d0)
  EXPECT_FLOAT_EQ(x%adj(), 0.0d0)
  EXPECT_FLOAT_EQ(y2%adj(), 1.0d0)
  EXPECT_FLOAT_EQ(y1%adj(), -sin(cos(1.4d0)))

end program cos_test
