module fazang_cvodes_model_mod
  use, intrinsic :: iso_c_binding
  use fazang_env_mod
  use fazang_var_mod
  use fsundials_nvector_mod
  use fsundials_matrix_mod
  implicit none

  type :: cvs_rhs
     procedure(cvs_rhs_func), nopass, pointer :: f => null()
     procedure(cvs_rhs_func_yvar), nopass, pointer :: f_yvar => null()
     procedure(cvs_rhs_func_pvar), nopass, pointer :: f_pvar => null()
     procedure(RhsFn), nopass, pointer :: cvs_f => null()
     procedure(CVSensRhsFn), nopass, pointer :: cvs_sens => null()
     logical :: ys = .false.
     integer :: ns = 0
     real(rk), pointer :: pval(:) => null()
   contains
     procedure, nopass :: RhsFn
     procedure, nopass :: CVSensRhsFn
  end type cvs_rhs

  abstract interface
     subroutine cvs_rhs_func(t, y, fy)
       import c_double
       real(c_double), intent(in) :: t, y(:)
       real(c_double), intent(inout) :: fy(size(y))
     end subroutine cvs_rhs_func

     subroutine cvs_rhs_func_yvar(t, y, f)
       import c_double, var
       real(c_double), intent(in) :: t
       type(var), intent(in) :: y(:)
       type(var), intent(inout) :: f(size(y))
     end subroutine cvs_rhs_func_yvar

     subroutine cvs_rhs_func_pvar(t, y, f, p)
       import c_double, var
       real(c_double), intent(in) :: t
       type(var), intent(in) :: y(:), p(:)
       type(var), intent(inout) :: f(size(y))
     end subroutine cvs_rhs_func_pvar
  end interface

  interface cvs_rhs
     module procedure :: new_cvs_rhs
     module procedure :: new_cvs_rhs_yvar
     module procedure :: new_cvs_rhs_pvar
  end interface cvs_rhs
contains

  function new_cvs_rhs(func) result(rhs)
    procedure(cvs_rhs_func) :: func
    type(cvs_rhs), target :: rhs
    rhs % f => func
    rhs % cvs_f => RhsFn
  end function new_cvs_rhs

  function new_cvs_rhs_yvar(ys, ns, func1, func2) result(rhs)
    logical, intent(in) :: ys
    integer(ik), intent(in) :: ns
    procedure(cvs_rhs_func) :: func1
    procedure(cvs_rhs_func_yvar) :: func2
    type(cvs_rhs), target :: rhs
    rhs % ys = ys
    rhs % ns = ns
    rhs % f => func1
    rhs % cvs_f => RhsFn
    rhs % f_yvar => func2
    rhs % cvs_sens => CVSensRhsFn
  end function new_cvs_rhs_yvar

  function new_cvs_rhs_pvar(ys, ns, p, func1, func2) result(rhs)
    logical, intent(in) :: ys
    integer, intent(in) :: ns
    real(rk), target, intent(in) :: p(:)
    procedure(cvs_rhs_func) :: func1
    procedure(cvs_rhs_func_pvar) :: func2
    type(cvs_rhs), target :: rhs
    rhs % ys = ys
    rhs % ns = ns
    rhs % f => func1
    rhs % cvs_f => RhsFn
    rhs % f_pvar => func2
    rhs % cvs_sens => CVSensRhsFn
    rhs % pval => p
  end function new_cvs_rhs_pvar

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

    type(cvs_rhs), pointer :: ode

    ierr = -1
    call c_f_pointer(user_data, ode)

    ! get data arrays from SUNDIALS vectors
    yvec => FN_VGetArrayPointer(sunvec_y)
    fvec => FN_VGetArrayPointer(sunvec_f)

    ! fill RHS vector
    call ode % f(tn, yvec, fvec)

    ! return success
    ierr = 0
  end function RhsFn

  subroutine calc_jac_yvar(n, t, y, f, jac, ode)
    use fazang_nested_tape_mod
    implicit none

    integer(c_long), intent(in) :: n
    real(c_double), intent(in) :: t
    real(c_double), intent(in) :: y(n)
    real(c_double), intent(in) :: f(n)
    real(c_double), intent(out) :: jac(n * n)
    type(cvs_rhs), intent(in) :: ode

    type(var) :: yvar(n)
    type(var) :: fvar(n)
    integer(c_long) :: i, j

    call begin_nested()
    yvar = var(y)
    fvar = var([(0.d0, i = 1, size(y))])
    call ode % f_yvar(t, yvar, fvar)
    do i = 1, n
       call set_zero_nested_adj ()
       call fvar(i)%grad_nested()
       do j = 1, n
          jac(i + (j - 1) * n) = yvar(j)%adj()
       end do
    end do
    call end_nested()
  end subroutine calc_jac_yvar

  subroutine set_sens_rhs(n, t, y, f, yS, ySdot, ode)
    use fazang
    use fazang_nested_tape_mod
    implicit none

    integer(ik), intent(in) :: n
    real(c_double), intent(in) :: t
    real(c_double), intent(in) :: y(n)
    real(c_double), intent(in) :: f(n)
    type(c_ptr), value :: yS
    type(c_ptr), value :: ySdot
    type(cvs_rhs), intent(in) :: ode

    type(var) :: yvar(n)
    type(var) :: fvar(n)
    integer :: i, j
    type(N_Vector), pointer :: yiS
    real(c_double), pointer :: yiSvec(:)
    type(N_Vector), pointer :: yiSdot
    real(c_double), pointer :: yiSdotvec(:)
    logical :: par_is_var
    type(var) :: pvar(3)
    integer m               ! # of var params

    m = merge(ode % ns - n, ode % ns, ode % ys)

    par_is_var = .not.(ode % ys .and. ode % ns == size(y))

    call begin_nested()
    yvar = var(y)
    fvar = var([(0.d0, i = 1, size(y))])
    if ( par_is_var ) then
       pvar = var(ode % pval)
       call ode % f_pvar(t, yvar, fvar, pvar)
    else
       call ode % f_yvar(t, yvar, fvar)
    endif

    do i = 1, n
       call fvar(i)%grad_nested()
       do j = 1, ode % ns
          yiS => FN_VGetVecAtIndexVectorArray(yS, j - 1)
          yiSdot => FN_VGetVecAtIndexVectorArray(ySdot, j - 1)
          yiSvec => FN_VGetArrayPointer(yiS)
          yiSdotvec => FN_VGetArrayPointer(yiSdot)
          yiSdotvec(i) = dot_product(yvar%adj(), yiSvec)
       end do
       
       if ( par_is_var ) then
          do j = 1, m
             yiSdot => FN_VGetVecAtIndexVectorArray(ySdot, ode % ns - m + j - 1)
             yiSdotvec => FN_VGetArrayPointer(yiSdot)
             yiSdotvec(i) = yiSdotvec(i) + pvar(j) % adj();
          enddo
       end if
       call set_zero_nested_adj ()
    end do
    call end_nested()
  end subroutine set_sens_rhs

  integer(c_int) function CVSensRhsFn(Ns, t, y, f, yS, ySdot, user_data, tmp1, tmp2)&
       result(ierr) bind(C, name='CVSensRhsFn')       
    use, intrinsic :: iso_c_binding
    use fsundials_nvector_mod
    use fsundials_matrix_mod
    use fsunmatrix_dense_mod

    use fazang_nested_tape_mod
    implicit none

    integer(c_long), value :: Ns
    real(c_double), value :: t  ! current time
    type(N_Vector)        :: y  ! solution N_Vector
    type(N_Vector)        :: f  ! rhs N_Vector
    type(c_ptr), value :: yS
    type(c_ptr), value :: ySdot
    type(c_ptr),    value :: user_data ! user-defined data
    type(N_Vector)        :: tmp1, tmp2
    integer(int32) :: n

    ! pointers to data in SUNDIALS vectors
    real(c_double), pointer :: yvec(:)
    real(c_double), pointer :: fvec(:)

    type(cvs_rhs), pointer :: ode

    call c_f_pointer(user_data, ode)

    ! get data arrays from SUNDIALS vectors
    yvec => FN_VGetArrayPointer(y)
    fvec => FN_VGetArrayPointer(f)
    n = int(FN_VGetLength(y), kind=int32)
    call set_sens_rhs(n, t, yvec, fvec, yS, ySdot, ode)
    ierr = 0
  end function CVSensRhsFn

end module fazang_cvodes_model_mod
