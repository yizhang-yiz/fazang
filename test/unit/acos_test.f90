#include "test_macro.fi"

program acos_test
  use, intrinsic :: iso_fortran_env
  use env_mod
  use test_mod
  use var_mod, only: var
  use vari_mod
  use acos_mod
  use sin_mod
  implicit none

  type(var) :: x, y1, y2, y3
  real(rk) :: z1, z2

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
  call callstack%set_zero_all_adj()
  EXPECT_FLOAT_EQ(y1%adj(), 0.0d0)
  EXPECT_FLOAT_EQ(x%adj(), 0.0d0)
  EXPECT_FLOAT_EQ(y2%val(), sin(acos(0.7d0)))
  
  EXPECT_EQ(callstack%operand_index_begin(1), 0)
  EXPECT_EQ(callstack%operand_index_begin(2), 1)
  EXPECT_EQ(callstack%operand_index_begin(3), 0)
  EXPECT_EQ(callstack%operand_index_begin(4), 2)
  EXPECT_EQ(callstack%operand_index(1), 1)
  EXPECT_EQ(callstack%operand_index(2), 2)
  EXPECT_EQ(callstack%operand_index(3), 0)
  EXPECT_EQ(callstack%operand_index(4), 0)

  call y2%vi%init_dependent()
  call y2%vi%chain()
  EXPECT_FLOAT_EQ(y3%adj(), 0.0d0)
  EXPECT_FLOAT_EQ(x%adj(), 0.0d0)
  EXPECT_FLOAT_EQ(y2%adj(), 1.0d0)
  EXPECT_FLOAT_EQ(y1%adj(), cos(acos(0.7d0)))

end program acos_test
