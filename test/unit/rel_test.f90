#include "test_macro.fi"

program rel_test
  use, intrinsic :: iso_fortran_env
  use test_mod
  use var_mod
  use fazang
  implicit none

  type(var) :: y1, y2, y3, y4

  y1 = var(3.0d0)
  y2 = y1
  y3 = var(4.0d0)
  y4 = var(4.1d0)

  EXPECT_TRUE(y1 == y2)
  EXPECT_TRUE(y3 > y2)
  EXPECT_TRUE(y3 >= y2)
  EXPECT_TRUE(y1 >= y2)
  EXPECT_TRUE(y1 <= y2)
  EXPECT_TRUE(y4 > y3)
  EXPECT_TRUE(y1 < y4)
  EXPECT_TRUE(.not.(y4 < y3))

end program rel_test
