#include "test_macro.fi"

! the following module fails to compile on gfortran 11.2.0:
! "internal compiler error: in gfc_conv_expr_descriptor, at fortran/trans-array.c:7307"
! it would seem C-F procedure pointer interop is buggy

! module eq_mod
!   implicit none
!   abstract interface
!      function func(a) result(s)
!        real, intent(in) :: a(2)
!        real :: s(2)
!      end function func
!   end interface

! contains
!   subroutine apply_func(user_data)
!     use, intrinsic :: iso_c_binding
!     implicit none
!     type(c_funptr), value :: user_data ! user-defined data
!     procedure(func), pointer :: fp
!     call c_f_procpointer(user_data, fp)
!   end subroutine apply_func

! end module eq_mod

module task_mod
  use fazang_env_mod
  use fazang_var_mod
  use, intrinsic :: iso_c_binding
  implicit none

  type(var) :: params(2)

contains
  subroutine eval_rhs(t, y, fy)
    real(c_double), intent(in) :: t, y(:)
    real(c_double), intent(out) :: fy(size(y))
    fy(1) = y(2)
    fy(2) = t * y(1) * sum(params%val())
  end subroutine eval_rhs

end module task_mod

program cvodes_rhs_callback_test
  use, intrinsic :: iso_c_binding
  use task_mod
  use fazang_test_mod
  use fazang_env_mod
  use fazang_cvodes_model_mod

  use fsundials_nvector_mod
  use fsundials_context_mod         ! Fortran interface to SUNContext
  use fnvector_serial_mod           ! Fortran interface to serial N_Vector
  implicit none

  type(cvs_rhs), target :: ode
  real(c_double) :: yvec(2), fvec(2)
  type(N_Vector), pointer :: sunvec_f ! sundials vector
  type(N_Vector), pointer :: sunvec_y ! sundials vector
  type(c_ptr) :: ctx         ! SUNDIALS simulation context
  type(c_ptr) :: user_data ! user-defined data
  integer :: ierr
  
  ierr = FSUNContext_Create(c_null_ptr, ctx)

  yvec = [2.d0, 3.d0]
  sunvec_y => FN_VMake_Serial(2_c_long, yvec, ctx)
  sunvec_f => FN_VMake_Serial(2_c_long, fvec, ctx)
  ode%f => eval_rhs

  ode = cvs_rhs(eval_rhs)

  params = var([12.d0, 89.d0])
  ! call eval_rhs(2.0d0, yvec, fvec)
  ! call ode%f(2.0d0, yvec, fvec)
  user_data = c_loc(ode)

  ierr = ode % RhsFn(2.d0, sunvec_y, sunvec_f, user_data)
  EXPECT_DBL_EQ(fvec(1), 3.d0)
  EXPECT_DBL_EQ(fvec(2), 404.d0)

end program cvodes_rhs_callback_test
