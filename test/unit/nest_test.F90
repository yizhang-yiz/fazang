#include "test_macro.fi"

module rhs
  use iso_c_binding
  use fazang_var_mod
  use fazang_mul_mod
  use fazang_nested_tape_mod
  implicit none

contains
  subroutine func(t, y, fy)
    implicit none
    real(c_double), intent(in) :: t
    type(var), intent(in) :: y(:)
    type(var), intent(inout) :: fy(size(y))
    fy(1) = y(1) * y(2)
    fy(2) = y(2) * y(2) * y(1)
  end subroutine func
end module rhs

program nest_test
  use, intrinsic :: iso_fortran_env
  use iso_c_binding
  use fazang_test_mod
  use fazang_env_mod
  use fazang_var_mod
  use fazang_grad_mod
  use fazang_vari_mod, only : adstack, callstack, vari
  use fazang_tape_mod
  use fazang_mul_mod
  use fazang_add_mod
  use fazang_square_mod
  use fazang_nested_tape_mod

  use rhs, only : func

  implicit none

  type(var) :: v(3), u(4), a, b
  type(var) :: y(2), f(2)

  call begin_nested()
  y = var([2.d0, 3.d0])
  f = var([0.d0, 0.d0])
  call func(0.d0, y, f)
  call set_zero_nested_adj()
  call f(1)%grad()
  EXPECT_DBL_EQ(y%adj(), ([3.d0, 2.d0]))
  call set_zero_nested_adj()
  call f(2)%grad()
  EXPECT_DBL_EQ(y%adj(), ([9.d0, 12.d0]))
  call end_nested()

  EXPECT_EQ(callstack % stack % head, 1)
  v = var([1.d0, 2.d0, 3.d0])
  EXPECT_EQ(callstack % stack % head, 19)
  call begin_nested()
  EXPECT_EQ(callstack % stack % head, 19)
  u = var([1.d0, 2.d0, 3.d0, 4.d0])
  EXPECT_EQ(callstack % stack % head, 43)
  call begin_nested()
  EXPECT_EQ(callstack % stack % head, 43)
  b = var(443.d0)
  a = var(232.d0)
  EXPECT_EQ(callstack % stack % head, 55)
  call end_nested()
  EXPECT_EQ(callstack % stack % head, 43)
  EXPECT_DBL_EQ(a%val(), 0d0)
  EXPECT_DBL_EQ(b%val(), 0d0)
  call end_nested()  
  EXPECT_EQ(callstack % stack % head, 19)
  EXPECT_DBL_EQ(u%val(), ([0d0,0d0,0d0,0d0]))
  EXPECT_DBL_EQ(v%val(), ([1.d0, 2.d0, 3.d0]))
  
  ! should be a dummy call
  call end_nested()
  EXPECT_EQ(callstack % stack % head, 19)
  EXPECT_DBL_EQ(v%val(), ([1.d0, 2.d0, 3.d0]))

  ! call begin_nested()
  ! y = var([2.d0, 3.d0])
  ! f = var([0.d0, 0.d0])
  ! call func(0.d0, y, f)
  ! call set_zero_nested_adj()
  ! call f(1)%grad()
  ! EXPECT_DBL_EQ(y%adj(), ([3.d0, 2.d0]))
  ! call set_zero_nested_adj()
  ! call f(2)%grad()
  ! EXPECT_DBL_EQ(y%adj(), ([9.d0, 12.d0]))
  ! call end_nested()

end program nest_test
