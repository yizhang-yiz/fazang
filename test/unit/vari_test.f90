#include "test_macro.fi"

program vari_test
  use, intrinsic :: iso_fortran_env
  use test_mod
  use vari_mod
  implicit none

  type(vari) :: y1, y2, y3, y4
  type(vari) :: y5(3), y6(2, 3)
  real(rk) :: z(3) = (/1.2d0, 2.2d0, 5.2d0/)
  real(real64) :: a(2, 3) = reshape((/1.2d0, 2.2d0, 5.2d0, 1.3d0,&
       & 2.3d0, 5.3d0/), shape(y6))
  integer i, j

  y1 = vari()
  EXPECT_FLOAT_EQ(callstack%storage(1, 1), 0.0d0)
  EXPECT_EQ(callstack%head, 2)

  y2 = vari(2.5d0)
  EXPECT_FLOAT_EQ(callstack%storage(1, 1), 0.0d0)
  EXPECT_FLOAT_EQ(callstack%storage(1, 2), 2.5d0)
  EXPECT_EQ(callstack%head, 3)

  y3 = vari(5.5d0)
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

  y2 = y3
  EXPECT_FLOAT_EQ(y2%val(), y3%val())
  y4 = y3
  EXPECT_FLOAT_EQ(y4%val(), y3%val())
  EXPECT_EQ(callstack%head, 4)

  y5 = vari(z)
  do i = 1, 3
     EXPECT_FLOAT_EQ(y5(i)%val(), z(i))
     EXPECT_EQ(y5(i)%i, 4 + i - 1)
  end do
  EXPECT_EQ(callstack%head, 7)

  y6 = vari(a)
  do j = 1, 3
     do i = 1, 2
     EXPECT_FLOAT_EQ(y6(i, j)%val(), a(i, j))
     EXPECT_EQ(y6(i, j)%i, 7 + i + (j-1) * 2 - 1)
     end do
  end do
  EXPECT_EQ(callstack%head, 13)

end program vari_test
