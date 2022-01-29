#include "test_macro.fi"

program acos_test
  use, intrinsic :: iso_fortran_env
  use test_mod
  use vari_mod, only : vari, adstack, callstack
  use fazang
  implicit none

  type(var) :: x, y1, y2, y3
  real(rk) :: z1, z2
  integer(ik) :: id(1)

  x = var(0.7d0)
  y1 = acos(x)
  call y1%vi%init_dependent()
  call y1%vi%chain()
  EXPECT_FLOAT_EQ(y1%val(), acos(0.7d0))
  EXPECT_FLOAT_EQ(y1%adj(), 1.0d0)
  EXPECT_FLOAT_EQ(x%adj(), -1.0d0/sqrt(1.d0 - 0.7d0 * 0.7d0))
  EXPECT_EQ(callstack%head, 3)

  z1 = x%val()
  z2 = acos(z1)
  EXPECT_FLOAT_EQ(y1%val(), z2)

  y3 = var(1.5d0)
  y2 = sin(y1)
  call set_zero_all_adj()
  EXPECT_FLOAT_EQ(y1%adj(), 0.0d0)
  EXPECT_FLOAT_EQ(x%adj(), 0.0d0)
  EXPECT_FLOAT_EQ(y2%val(), sin(acos(0.7d0)))
  
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
  EXPECT_FLOAT_EQ(y1%adj(), cos(acos(0.7d0)))

end program acos_test
