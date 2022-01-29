#include "test_macro.fi"

program tape_test
  use, intrinsic :: iso_fortran_env
  use iso_c_binding
  use test_mod
  use env_mod
  use tape_mod

  type(tape), target :: t
  real(rk) :: op4(3), dop(2)
  real(rk) :: x(4), y(4), z(3)
  integer(ik) :: i

  i = t%push()
  EXPECT_EQ(t%head, min_rec_size + 1)
  i = t%push(1.2d0)
  EXPECT_EQ(t%head, 2 * min_rec_size + 1)
  i = t%push(4.2d0)
  EXPECT_DBL_EQ(t%val(1), 0.0d0)
  EXPECT_DBL_EQ(t%val(7), 1.2d0)
  EXPECT_DBL_EQ(t%val(13), 4.2d0)
  i = t%push(3.4d0, [1, 7, 13])
  EXPECT_EQ(t%head, 4 * min_rec_size + 3 + 1)
  op4 = t%operand_val(i)
  EXPECT_DBL_EQ(op4(1), 0.0d0)
  EXPECT_DBL_EQ(op4(2), 1.2d0)
  EXPECT_DBL_EQ(op4(3), 4.2d0)

  i = t%push(342.8d0)
  EXPECT_EQ(t%head, i + min_rec_size)
  call t%set_adj(13, -1.d0)
  EXPECT_DBL_EQ(t%adj(13), -1.d0)

  i = t%push(32.d0, [2.d0, 3.2d0])
  EXPECT_EQ(t%head, i + min_rec_size + 2 * 2) ! real64 to int32
  dop = t%data_operand(i)
  EXPECT_DBL_EQ(dop(1), 2.0d0)
  EXPECT_DBL_EQ(dop(2), 3.2d0)

  i = t%push(33.d0, [1, 7, 13, 19], [2.d0, 1.4d0, 3.2d0])
  call t%set_adj(13, 2.2d0)
  call t%set_adj(19, 2.4d0)
  x = t%operand_val(i)
  y = t%operand_adj(i)
  z = t%data_operand(i)
  EXPECT_DBL_EQ(x(1), 0.0d0)
  EXPECT_DBL_EQ(x(2), 1.2d0)
  EXPECT_DBL_EQ(x(3), 4.2d0)
  EXPECT_DBL_EQ(x(4), 3.4d0)
  EXPECT_DBL_EQ(y(1), 0.0d0)
  EXPECT_DBL_EQ(y(2), 0.0d0)
  EXPECT_DBL_EQ(y(3), 2.2d0)
  EXPECT_DBL_EQ(y(4), 2.4d0)
  EXPECT_DBL_EQ(z(1), 2.0d0)
  EXPECT_DBL_EQ(z(2), 1.4d0)
  EXPECT_DBL_EQ(z(3), 3.2d0)
  
end program tape_test
