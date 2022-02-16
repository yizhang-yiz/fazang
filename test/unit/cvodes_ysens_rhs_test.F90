#include "test_macro.fi"

module task_mod
  use fazang
  use, intrinsic :: iso_c_binding
  implicit none

  real(rk), parameter :: omega = 0.3d0

contains
  subroutine eval_rhs(t, y, fy)
    implicit none
    real(c_double), intent(in) :: t, y(:)
    real(c_double), intent(inout) :: fy(size(y))
    fy(1) = y(2)
    fy(2) = sin(omega * t)
  end subroutine eval_rhs

  subroutine eval_rhs_yvar(t, y, fy)
    implicit none
    real(c_double), intent(in) :: t
    type(var), intent(in) :: y(:)
    type(var), intent(inout) :: fy(size(y))
    fy(1) = y(1) * y(2)
    fy(2) = y(2) * y(2) * y(1)
  end subroutine eval_rhs_yvar
end module task_mod

program cvodes_rhs_callback_test
  use, intrinsic :: iso_c_binding
  use task_mod
  use fazang_test_mod
  use fazang_env_mod
  use fazang_cvodes_model_mod
  use fazang

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
  integer :: ierr, is

  type(c_ptr) :: yS, ySdot
  type(N_Vector), pointer :: yiS, yiSdot
  real(c_double), pointer :: yiSvec(:), yiSdotvec(:)

  ierr = FSUNContext_Create(c_null_ptr, ctx)

  yvec = [2.d0, 3.d0]
  fvec = 0.d0
  sunvec_y => FN_VMake_Serial(2_c_long, yvec, ctx)
  sunvec_f => FN_VMake_Serial(2_c_long, fvec, ctx)
  ode = cvs_rhs(.true., 2, eval_rhs, eval_rhs_yvar)

  user_data = c_loc(ode)

  EXPECT_EQ(ode % ns, 2)

  yS = FN_VCloneVectorArray(ode % ns, sunvec_y) 
  ySdot = FN_VCloneVectorArray(ode % ns, sunvec_y)
  do is = 0, ode % ns - 1
     yiS => FN_VGetVecAtIndexVectorArray(yS, is)
     call FN_VConst(0.d0, yiS)
     yiSvec => FN_VGetArrayPointer(yiS)
     yiSvec(is + 1) = 1.0;
     yiSdot => FN_VGetVecAtIndexVectorArray(ySdot, is)
     call FN_VConst(0.d0, yiSdot)
  end do

  ierr = ode % cvs_sens(2_8, 2.d0, sunvec_y, sunvec_f, yS, ySdot, user_data, sunvec_f, sunvec_f)

  yiSdot => FN_VGetVecAtIndexVectorArray(ySdot, 0)
  yiSvec => FN_VGetArrayPointer(yiSdot)
  EXPECT_DBL_EQ(yiSvec(1), 3.d0)
  EXPECT_DBL_EQ(yiSvec(2), 9.d0)
  yiSdot => FN_VGetVecAtIndexVectorArray(ySdot, 1)
  yiSvec => FN_VGetArrayPointer(yiSdot)
  EXPECT_DBL_EQ(yiSvec(1), 2.d0)
  EXPECT_DBL_EQ(yiSvec(2), 12.d0)

  call FN_VDestroy(sunvec_y)
  call FN_VDestroy(sunvec_f)
  call FN_VDestroyVectorArray(yS, 2)
  call FN_VDestroyVectorArray(ySdot, 2)
  ierr = FSUNContext_Free(ctx)

end program cvodes_rhs_callback_test
