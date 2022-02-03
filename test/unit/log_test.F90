#include "test_macro.fi"

program log_test
  use, intrinsic :: iso_fortran_env
  use fazang_test_mod
  use fazang_vari_mod, only : vari, adstack, callstack, vari_at
  use fazang_env_mod
  use fazang_grad_mod
  use fazang_var_mod
  use fazang_log_mod
  implicit none

  type(var) :: x, y1, y2, y3
  real(rk) :: z1, z2
  type(vari), pointer :: vp

  x = var(1.5d0)
  y1 = log(x)
  call y1%grad()
  EXPECT_FLOAT_EQ(y1%val(), log(1.5d0))
  EXPECT_FLOAT_EQ(y1%adj(), 1.0d0)
  EXPECT_FLOAT_EQ(x%adj(), 1.d0/1.5d0)
  EXPECT_EQ(callstack%head, 3)

  z1 = x%val()
  z2 = log(z1)
  EXPECT_FLOAT_EQ(y1%val(), z2)

  y3 = var(1.5d0)
  y2 = log(y1)
  call set_zero_all_adj()
  EXPECT_FLOAT_EQ(y1%adj(), 0.0d0)
  EXPECT_FLOAT_EQ(x%adj(), 0.0d0)
  EXPECT_FLOAT_EQ(y2%val(), log(log(1.5d0)))
  
  vp => vari_at(y2%vi)
  call vp%set_adj(2.5d0)
  call vp%chain()
  EXPECT_FLOAT_EQ(y3%adj(), 0.0d0)
  EXPECT_FLOAT_EQ(x%adj(), 0.0d0)
  EXPECT_FLOAT_EQ(y2%adj(), 2.5d0)
  EXPECT_FLOAT_EQ(y1%adj(), 2.5d0/log(1.5d0))

end program log_test
