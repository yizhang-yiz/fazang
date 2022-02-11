#include "test_macro.fi"

module eq_mod
  use fazang_env_mod
  use fazang_var_mod
  use, intrinsic :: iso_c_binding
  implicit none

  type :: rhs
     procedure(func), nopass, pointer :: fp
  end type rhs

  abstract interface
     subroutine func(a, b, c)
       import
       ! integer, intent(in) :: n
       real(real64), intent(in) :: a(:), b(:)
       real(real64), intent(out) :: c(size(a))
     end subroutine func
  end interface

contains
  subroutine apply_func(user_data)
    use, intrinsic :: iso_c_binding
    implicit none
    type(c_ptr), value :: user_data ! user-defined data
    type(rhs), pointer :: rhs_p
    real(real64) :: c(2)
    call c_f_pointer(user_data, rhs_p)
    
    call rhs_p%fp([1.0d0, 2.d0], [3.d0, 4.d0], c)
  end subroutine apply_func

  subroutine func_impl(a, b, c)
    ! integer, intent(in) :: n
    real(real64), intent(in) :: a(:), b(:)
    real(real64), intent(out) :: c(size(a))
    c(1) = a(1) * 2.0
    c(2) = b(2) * 3.0
  end subroutine func_impl

end module eq_mod

program debug_test
  use eq_mod
  use fazang_env_mod
  implicit none

  type(rhs), target :: rhs1
  type(c_ptr) :: cfp

  rhs1%fp => func_impl
  cfp = c_loc(rhs1)
  call apply_func(cfp)

end program debug_test
