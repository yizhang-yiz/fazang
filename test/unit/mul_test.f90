#include "test_macro.fi"

program mul_test
  use, intrinsic :: iso_fortran_env
  use test_mod
  use vari_mod
  use fazang
  implicit none

  type(var) :: x, y1, y2, y3, y4
  real(rk) :: z1 = 2.0d0, z2 = 3.9d0

  x = var(1.5d0)
  x = x * z1
  x = z2 * x
  EXPECT_EQ(callstack%head, 4)
  EXPECT_FLOAT_EQ(callstack%val(1), 1.5d0)
  EXPECT_FLOAT_EQ(callstack%val(2), 1.5d0 * z1)
  EXPECT_FLOAT_EQ(callstack%val(3), 1.5d0 * z1 * z2)
  EXPECT_FLOAT_EQ(x%val(), 1.5d0 * z1 * z2)

  call x%grad()
  EXPECT_FLOAT_EQ(callstack%adj(1), z1 * z2)
  EXPECT_FLOAT_EQ(callstack%adj(2), z2)
  EXPECT_FLOAT_EQ(callstack%adj(3), 1.d0)

  y2 = var(2.6d0)
  y1 = x
  y3 = y2 * y1
  EXPECT_EQ(callstack%head, 6)
  EXPECT_FLOAT_EQ(y3%val(), 2.6d0 * x%val())
  call set_zero_all_adj()
  call y3%vi%set_adj(2.5d0)
  call y3%vi%chain()
  EXPECT_FLOAT_EQ(y2%adj(), 2.5d0 * y1%val())
  EXPECT_FLOAT_EQ(y1%adj(), 2.5d0 * y2%val())
  EXPECT_FLOAT_EQ(x%adj(), 2.5d0 * y2%val())  
  
  ! note that y3 = y2 * y1
  y4 = y3 * y2 + y1 * y2
  call set_zero_all_adj()
  call y4%grad()
  EXPECT_FLOAT_EQ(y3%adj(), y2%val())
  EXPECT_FLOAT_EQ(y2%adj(), 2.d0 * y2%val() * y1%val() + y1%val())
  EXPECT_FLOAT_EQ(y1%adj(), y2%val() + y2%val()**2)

end program mul_test
