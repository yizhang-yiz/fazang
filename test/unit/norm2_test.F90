#include "test_macro.fi"

program norm2_test
  use, intrinsic :: iso_fortran_env
  use fazang_test_mod
  use fazang_env_mod
  use fazang_grad_mod
  use fazang_var_mod
  use fazang_norm2_mod
  use fazang_add_mod
  use fazang_grad_mod
  implicit none

  type(var) :: x(4), s
  real(rk) :: z1(4) = [1.d0, 47.d0, 3.d0, 53.d0]
  integer(ik) :: i

  x = var(z1)
  s = norm2(x)
  EXPECT_DBL_EQ(s%val(), norm2(z1))
  call s%grad()
  EXPECT_DBL_EQ(s%adj(), 1.d0)
  EXPECT_DBL_EQ(x%adj(), z1/norm2(z1))

end program norm2_test
