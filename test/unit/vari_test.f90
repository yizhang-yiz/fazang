#include "test_macro.fi"

program vari_test
  use, intrinsic :: iso_fortran_env
  use env_mod
  use test_mod
  use vari_mod, only : vari, adstack, callstack
  implicit none

  type(vari), pointer :: y1, y2, y3, y4
  real(rk) :: x1(2)
  integer(ik) :: ops(3)

  y1 => vari()
  EXPECT_DBL_EQ(y1%val(), 0.0d0)
  y2 => vari(2.5d0)
  EXPECT_DBL_EQ(y2%val(), 2.5d0)
  y3 => vari(2.9d0, [y1%i, y2%i])
  x1 = y3%operand_val()
  EXPECT_DBL_EQ(x1(1), 0.0d0)
  EXPECT_DBL_EQ(x1(2), 2.5d0)
  y4 => vari(3.9d0, [y3%i, y2%i, y1%i], [0.3d0, 2.3d0])
  ops = y4%operand_index()
  EXPECT_EQ(ops(1), 13)
  EXPECT_EQ(ops(2), 7)
  EXPECT_EQ(ops(3), 1)
  x1 = y4%data_operand()
  EXPECT_DBL_EQ(x1(1), 0.3d0)
  EXPECT_DBL_EQ(x1(2), 2.3d0)

  EXPECT_EQ(callstack%head, 5)
  y4 = y3
  EXPECT_EQ(callstack%head, 5)
  EXPECT_DBL_EQ(y4%val(), 2.9d0)

  EXPECT_DBL_EQ(y3%adj(), 0.d0)
  call y4%init_dependent()
  EXPECT_DBL_EQ(y3%adj(), 1.d0)

end program vari_test
