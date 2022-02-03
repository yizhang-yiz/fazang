#include "test_macro.fi"

program sin_test
  use, intrinsic :: iso_fortran_env
  use fazang_test_mod
  use fazang_vari_mod, only : vari, adstack, callstack, vari_at, set_indexed_adj
  use fazang_env_mod
  use fazang_grad_mod
  use fazang_var_mod
  use fazang_sin_mod
  implicit none

  type(var) :: x, y1, y2, y3, z(2)
  real(rk) :: z1, z2
  integer(ik) :: id(1)
  type(vari), pointer :: vp

  x = var(1.4d0)
  y1 = sin(x)
  vp => vari_at(y1%vi)
  call vp%init_dependent()
  call vp%chain()
  EXPECT_FLOAT_EQ(y1%val(), sin(1.4d0))
  EXPECT_FLOAT_EQ(y1%adj(), 1.0d0)
  EXPECT_FLOAT_EQ(x%adj(), cos(1.4d0))
  EXPECT_EQ(callstack%head, 3)

  z1 = x%val()
  z2 = sin(z1)
  EXPECT_FLOAT_EQ(y1%val(), z2)

  y3 = var(1.5d0)
  y2 = sin(y1)
  call set_zero_all_adj()
  EXPECT_FLOAT_EQ(y1%adj(), 0.0d0)
  EXPECT_FLOAT_EQ(x%adj(), 0.0d0)
  EXPECT_FLOAT_EQ(y2%val(), sin(sin(1.4d0)))
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
  EXPECT_FLOAT_EQ(y1%adj(), cos(sin(1.4d0)))
  
  ! vectorized version
  z = sin([y1, y2])
  call set_zero_all_adj()
  call set_indexed_adj(z(1)%vi, 1.3d0)
  vp => vari_at(z(1)%vi)
  call vp%chain()
  EXPECT_DBL_EQ(y1%adj(), 1.3d0 * cos(y1%val()))
  EXPECT_DBL_EQ(y2%adj(), 0.d0)
  call set_zero_all_adj()
  EXPECT_DBL_EQ(y1%adj(), 0.d0)
  vp => vari_at(z(2)%vi)
  call set_indexed_adj(z(2)%vi, 1.3d0)
  call vp%chain()
  EXPECT_DBL_EQ(y1%adj(), 0.d0)
  EXPECT_DBL_EQ(y2%adj(), 1.3d0 * cos(y2%val()))

end program sin_test
