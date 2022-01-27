#include "test_macro.fi"

program add_test
  use, intrinsic :: iso_fortran_env
  use env_mod
  use test_mod
  use vari_mod
  use add_mod
  implicit none

  type(vari) :: x, y1, y2, y3, y4
  real(rk) :: z1 = 2.4d0, z2 = 3.9d0

  x = vari(1.5d0)
  x = x + z1
  y1 = x
  EXPECT_EQ(callstack%head, 3)
  EXPECT_FLOAT_EQ(callstack%val(1), 1.5d0)
  EXPECT_FLOAT_EQ(callstack%val(2), 1.5d0 + z1)

  call x%set_adj(0.4d0)
  call x%chain()
  EXPECT_FLOAT_EQ(callstack%adj(1), x%adj())

  y2 = vari(2.6d0)
  y3 = y2 + y1
  EXPECT_EQ(callstack%head, 5)
  EXPECT_FLOAT_EQ(callstack%val(3), 2.6d0)
  EXPECT_FLOAT_EQ(callstack%val(4), y2%val() + y1%val())

  call y3%init_dependent()
  call y3%chain()
  EXPECT_FLOAT_EQ(y2%adj(), 1.d0)
  EXPECT_FLOAT_EQ(y1%adj(), 1.4d0)

  call callstack%set_zero_all_adj()
  EXPECT_FLOAT_EQ(y2%adj(), 0.d0)
  EXPECT_FLOAT_EQ(y1%adj(), 0.d0)
  y4 = y1 + z1 + y2 + z2
  EXPECT_EQ(callstack%head, 8)
  call y4%init_dependent()
  call y4%chain()
  ! EXPECT_FLOAT_EQ(y2%adj(), 1.d0)
  ! EXPECT_FLOAT_EQ(y1%adj(), 1.d0)


  ! EXPECT_FLOAT_EQ(y1%adj(), 1.0d0)
  ! EXPECT_FLOAT_EQ(x%adj(), 1.d0/1.5d0)
  ! call expect_eq(callstack%head, 3)

  ! z1 = x%val()
  ! z2 = log(z1)
  ! EXPECT_FLOAT_EQ(y1%val(), z2)

  ! y3 = vari(1.5d0)
  ! y2 = log(y1)
  ! call callstack%set_zero_all_adj()
  ! EXPECT_FLOAT_EQ(y1%adj(), 0.0d0)
  ! EXPECT_FLOAT_EQ(x%adj(), 0.0d0)
  ! EXPECT_FLOAT_EQ(y2%val(), log(log(1.5d0)))
  
  ! call expect_eq(callstack%operand_index_begin(1), 0)
  ! call expect_eq(callstack%operand_index_begin(2), 1)
  ! call expect_eq(callstack%operand_index_begin(3), 0)
  ! call expect_eq(callstack%operand_index_begin(4), 2)
  ! call expect_eq(callstack%operand_index(1), 1)
  ! call expect_eq(callstack%operand_index(2), 2)
  ! call expect_eq(callstack%operand_index(3), 0)
  ! call expect_eq(callstack%operand_index(4), 0)

  ! call y2%init_dependent()
  ! call y2%set_adj(2.5d0)
  ! call y2%chain()
  ! EXPECT_FLOAT_EQ(y3%adj(), 0.0d0)
  ! EXPECT_FLOAT_EQ(x%adj(), 0.0d0)
  ! EXPECT_FLOAT_EQ(y2%adj(), 2.5d0)
  ! EXPECT_FLOAT_EQ(y1%adj(), 2.5d0/log(1.5d0))

end program add_test
