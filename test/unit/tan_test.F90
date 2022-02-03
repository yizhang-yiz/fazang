#include "test_macro.fi"

program tan_test
  use, intrinsic :: iso_fortran_env
  use fazang_test_mod
  use fazang_vari_mod, only : vari, adstack, callstack, vari_at
  use fazang_env_mod
  use fazang_tan_mod
  use fazang_var_mod
  use fazang_grad_mod
  implicit none

  type(var) :: x, y1, y2, y3
  real(rk) :: z1, z2
  integer(ik) :: id(1)
  type(vari), pointer :: vp

  x = var(1.4d0)
  y1 = tan(x)
  call y1%grad()
  EXPECT_FLOAT_EQ(y1%val(), tan(1.4d0))
  EXPECT_FLOAT_EQ(y1%adj(), 1.0d0)
  EXPECT_FLOAT_EQ(x%adj(), 1.0d0/cos(1.4d0)/cos(1.4d0))
  EXPECT_EQ(callstack%head, 3)

  z1 = x%val()
  z2 = tan(z1)
  EXPECT_FLOAT_EQ(y1%val(), z2)

  y3 = var(1.5d0)
  y2 = tan(y1)
  call set_zero_all_adj()
  EXPECT_FLOAT_EQ(y1%adj(), 0.0d0)
  EXPECT_FLOAT_EQ(x%adj(), 0.0d0)
  EXPECT_FLOAT_EQ(y2%val(), tan(tan(1.4d0)))
  
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
  EXPECT_FLOAT_EQ(y1%adj(), 1.d0/cos(tan(1.4d0))/cos(tan(1.4d0)))

end program tan_test
