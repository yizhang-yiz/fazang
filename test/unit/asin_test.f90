#include "test_macro.fi"

program asin_test
  use, intrinsic :: iso_fortran_env
  use test_mod
  use vari_mod, only : vari, adstack, callstack, vari_at
  use grad_mod
  use var_mod
  use asin_mod
  use sin_mod
  implicit none

  type(var) :: x, y1, y2, y3
  real(rk) :: z1, z2
  integer(ik) :: id(1)
  type(vari), pointer :: vp

  x = var(0.7d0)
  y1 = asin(x)
  vp => vari_at(y1%vi)  
  call vp%init_dependent()
  call vp%chain()
  EXPECT_FLOAT_EQ(y1%val(), asin(0.7d0))
  EXPECT_FLOAT_EQ(y1%adj(), 1.0d0)
  EXPECT_FLOAT_EQ(x%adj(), 1.0d0/sqrt(1.d0 - 0.7d0 * 0.7d0))
  EXPECT_EQ(callstack%head, 3)

  z1 = x%val()
  z2 = asin(z1)
  EXPECT_FLOAT_EQ(y1%val(), z2)

  y3 = var(1.5d0)
  y2 = asin(y1)
  call set_zero_all_adj()
  EXPECT_FLOAT_EQ(y1%adj(), 0.0d0)
  EXPECT_FLOAT_EQ(x%adj(), 0.0d0)
  EXPECT_FLOAT_EQ(y2%val(), asin(asin(0.7d0)))

  vp => vari_at(y2%vi)
  id = vp%operand_index()
  EXPECT_EQ(id(1), y1%vi_index())
  vp => vari_at(y1%vi)
  EXPECT_EQ(vp%n_operand(), 1)
  vp => vari_at(y3%vi)
  EXPECT_EQ(vp%n_operand(), 0)
  
  call set_zero_all_adj()
  vp => vari_at(y2%vi)  
  call vp%init_dependent()
  call vp%chain()
  EXPECT_FLOAT_EQ(y3%adj(), 0.0d0)
  EXPECT_FLOAT_EQ(x%adj(), 0.0d0)
  EXPECT_FLOAT_EQ(y2%adj(), 1.0d0)
  EXPECT_FLOAT_EQ(y1%adj(), 1.d0/sqrt(1-asin(0.7d0)*asin(0.7d0)))

end program asin_test
