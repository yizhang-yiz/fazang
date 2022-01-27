#include "test_macro.fi"

program vari_test
  use, intrinsic :: iso_fortran_env
  use test_mod
  use vari_mod
  implicit none

  type(vari), pointer :: y1, y2, y3, y4

  y1 => vari()
  EXPECT_FLOAT_EQ(callstack%storage(1, 1), 0.0d0)
  EXPECT_EQ(callstack%head, 2)

  y2 => vari(2.5d0)
  EXPECT_FLOAT_EQ(callstack%storage(1, 1), 0.0d0)
  EXPECT_FLOAT_EQ(callstack%storage(1, 2), 2.5d0)
  EXPECT_EQ(callstack%head, 3)

  y3 => vari(5.5d0)
  EXPECT_FLOAT_EQ(callstack%storage(1, 1), 0.0d0)
  EXPECT_FLOAT_EQ(callstack%storage(1, 2), 2.5d0)
  EXPECT_FLOAT_EQ(callstack%storage(1, 3), 5.5d0)
  EXPECT_FLOAT_EQ(y1%val(), 0.0d0)
  EXPECT_FLOAT_EQ(y2%val(), 2.5d0)
  EXPECT_FLOAT_EQ(y3%val(), 5.5d0)
  EXPECT_EQ(callstack%head, 4)

  y3 = 9.4d0
  EXPECT_FLOAT_EQ(y1%val(), 0.0d0)
  EXPECT_FLOAT_EQ(y2%val(), 2.5d0)
  EXPECT_FLOAT_EQ(y3%val(), 9.4d0)
  EXPECT_EQ(y3%i, 3)
  EXPECT_EQ(callstack%head, 4)

  y2 = 9.2d0
  EXPECT_FLOAT_EQ(y1%val(), 0.0d0)
  EXPECT_FLOAT_EQ(y2%val(), 9.2d0)
  EXPECT_FLOAT_EQ(y3%val(), 9.4d0)
  EXPECT_EQ(y2%i, 2)
  EXPECT_EQ(y3%i, 3)
  EXPECT_EQ(callstack%head, 4)

  y2 => y3
  EXPECT_FLOAT_EQ(y2%val(), y3%val())
  y4 => y3
  EXPECT_FLOAT_EQ(y4%val(), y3%val())
  EXPECT_EQ(callstack%head, 4)

end program vari_test
