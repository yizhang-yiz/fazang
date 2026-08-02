#include "vari_op_inc.f90"

module fz_fvari_op
  use, intrinsic :: iso_fortran_env
  use, intrinsic :: iso_c_binding
  use fz_env
  use fz_fvari

contains

  DEF_OP1(type(fvari), exp, exp(vi%val_), (this%val_))

  DEF_OP1(type(fvari), sin, sin(vi%val_), (cos(a%val_)))

  DEF_OP1(type(fvari), cos, cos(vi%val_), (-sin(a%val_)))

  DEF_OP1(type(fvari), tan, tan(vi%val_), (1.d0/(cos(a%val_)*cos(a%val_))))

  DEF_OP1(type(fvari), asin, asin(vi%val_), (1.d0/sqrt(1.d0-a%val_*a%val_)))

  DEF_OP1(type(fvari), acos, acos(vi%val_), (-1.d0/sqrt(1.d0-a%val_*a%val_)))

  DEF_OP1(type(fvari), atan, atan(vi%val_), (1.d0/(1.d0+a%val_*a%val_)))

  DEF_OP1(type(fvari), log, log(vi%val_), (1.d0/a%val_))

  DEF_OP1(type(fvari), log10, log10(vi%val_), (1.d0/(a%val_*dlog(10.d0))))

  DEF_OP1(type(fvari), sqrt, sqrt(vi%val_), (0.5d0/sqrt(a%val_)))

  DEF_OP1(type(fvari), neg, (-vi%val_), (-1.d0))

  DEF_OP1(type(fvari), pos, (vi%val_), (1.d0))

  ! DEF_OP1(type(fvari), sinh, dsinh(vi%val_), (dcosh(a%val_)))

  ! DEF_OP1(type(fvari), cosh, dcosh(vi%val_), (dsinh(a%val_)))

  ! DEF_OP1(type(fvari), tanh, dtanh(vi%val_), (1.d0/(dcosh(a%val_)*dcosh(a%val_))) )

  ! elemental real(rk) function logit_d(d)
  !   implicit none
  !   real(rk), intent(in) :: d
  !   logit_d = log(d / (1.d0 - d))
  ! end function logit_d
  ! DEF_OP1(type(fvari), logit, logit_d(vi%val_), (1.d0 / (a%val_ - a%val_ * a%val_)) )

  ! elemental function inv_logit_d(d) result(s)
  !   use fz_env, only : log_eps
  !   implicit none
  !   real(rk), intent(in) :: d
  !   real(rk) :: s, exp_d
  !   if ( d < 0.d0 ) then
  !      exp_d = exp(d)
  !      if (d < log_eps) then
  !         s = exp_d
  !      else
  !         s = exp_d / (1.d0 + exp_d);
  !      endif
  !   else
  !      s = 1.d0/(1.d0 + exp(-d))
  !   endif
  ! end function inv_logit_d
  ! DEF_OP1(type(fvari), inv_logit, inv_logit_d(vi%val_), (this%val_ * (1.d0 - this%val_)) )

  DEF_OP2_VD(type(fvari), add, (va%val_ + b), (1.d0))
  DEF_OP2_DV(type(fvari), add, (vb%val_ + a), (1.d0))
  DEF_OP2_VV(type(fvari), add, (vb%val_ + va%val_), (1.d0), (1.d0))

  DEF_OP2_VD(type(fvari), sub, (va%val_ - b), (1.d0))
  DEF_OP2_DV(type(fvari), sub, (a - vb%val_), (-1.d0))
  DEF_OP2_VV(type(fvari), sub, (va%val_ - vb%val_), (1.d0), (-1.d0))

  DEF_OP2_VD(type(fvari), mul, (va%val_ * b), (b))
  DEF_OP2_DV(type(fvari), mul, (a * vb%val_), (a))
  DEF_OP2_VV(type(fvari), mul, (va%val_ * vb%val_), (vb%val_), (va%val_))

  DEF_OP2_VD(type(fvari), div, (va%val_/b), (1.d0/b))
  DEF_OP2_DV(type(fvari), div, (a/vb%val_), (-this%val_/vb%val_))
  DEF_OP2_VV(type(fvari), div, (va%val_/vb%val_), (1.d0/vb%val_), (-this%val_/vb%val_))

  subroutine chain_sum (this)
    implicit none
    type(fvari), pointer, intent(in) :: this
    type(fvari), pointer :: va
    integer(ik) :: i, j, n
    n = core_adstack%pop_int(this%i + visize)
    do i = 1, n
       call recover(va, core_adstack%pop_int(this%i + visize + i*iksize))
       va%adj_ = va%adj_ + this%adj_
    enddo
  end subroutine chain_sum

end module fz_fvari_op
