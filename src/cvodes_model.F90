module fazang_cvodes_model_mod
  use, intrinsic :: iso_c_binding
  use fazang_env_mod
  use fazang_var_mod
  implicit none

  type :: cvs_rhs
     procedure(cvs_rhs_func), nopass, pointer :: f
   contains
     procedure, nopass :: RhsFn
  end type cvs_rhs

  abstract interface
     subroutine cvs_rhs_func(t, y, fy)
       import c_double
       real(c_double), intent(in) :: t, y(:)
       real(c_double), intent(out) :: fy(size(y))
     end subroutine cvs_rhs_func
  end interface

  interface cvs_rhs
     module procedure :: new_cvs_rhs
  end interface cvs_rhs
contains

  function new_cvs_rhs(func) result(rhs)
    procedure(cvs_rhs_func) :: func
    type(cvs_rhs), target :: rhs
    rhs % f => func
  end function new_cvs_rhs

  ! ----------------------------------------------------------------
  ! RhsFn provides the right hand side function for the
  ! ODE: dy/dt = f(t,y)
  !
  ! Return values:
  !    0 = success,
  !    1 = recoverable error,
  !   -1 = non-recoverable error
  ! ----------------------------------------------------------------
  integer(c_int) function RhsFn(tn, sunvec_y, sunvec_f, user_data) &
       result(ierr) bind(C,name='RhsFn')
    use, intrinsic :: iso_c_binding
    use fsundials_nvector_mod
    implicit none

    real(c_double), value :: tn        ! current time
    type(N_Vector)        :: sunvec_y  ! solution N_Vector
    type(N_Vector)        :: sunvec_f  ! rhs N_Vector
    type(c_ptr),    value :: user_data ! user-defined data

    ! pointers to data in SUNDIALS vectors
    real(c_double), pointer :: yvec(:)
    real(c_double), pointer :: fvec(:)

    type(cvs_rhs), pointer :: rhs_p

    call c_f_pointer(user_data, rhs_p)

    ! get data arrays from SUNDIALS vectors
    yvec => FN_VGetArrayPointer(sunvec_y)
    fvec => FN_VGetArrayPointer(sunvec_f)

    ! fill RHS vector
    call rhs_p % f(tn, yvec, fvec)

    ! return success
    ierr = 0
  end function RhsFn

end module fazang_cvodes_model_mod
