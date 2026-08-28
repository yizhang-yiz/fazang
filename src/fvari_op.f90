#include "vari_op_inc.f90"

DEF_VARI1_MOD(exp_fvari_mod, fz_fvari, exp(vi%val_), (this%val_))
DEF_VARI1_MOD(sin_fvari_mod, fz_fvari, sin(vi%val_), (cos(a%val_)))
DEF_VARI1_MOD(cos_fvari_mod, fz_fvari, cos(vi%val_), (-sin(a%val_)))
DEF_VARI1_MOD(tan_fvari_mod, fz_fvari, tan(vi%val_), (1.d0/(cos(a%val_)*cos(a%val_))))
DEF_VARI1_MOD(asin_fvari_mod, fz_fvari, asin(vi%val_), (1.d0/sqrt(1.d0-a%val_*a%val_)))
DEF_VARI1_MOD(acos_fvari_mod, fz_fvari, acos(vi%val_), (-1.d0/sqrt(1.d0-a%val_*a%val_)))
DEF_VARI1_MOD(atan_fvari_mod, fz_fvari, atan(vi%val_), (1.d0/(1.d0+a%val_*a%val_)))
DEF_VARI1_MOD(log_fvari_mod, fz_fvari, log(vi%val_), (1.d0/a%val_))
DEF_VARI1_MOD(log10_fvari_mod, fz_fvari, log10(vi%val_), (1.d0/(a%val_*dlog(10.d0))))
DEF_VARI1_MOD(sqrt_fvari_mod, fz_fvari, sqrt(vi%val_), (0.5d0/sqrt(a%val_)))
DEF_VARI1_MOD(neg_fvari_mod, fz_fvari, (-vi%val_), (-1.d0))
DEF_VARI1_MOD(pos_fvari_mod, fz_fvari, (vi%val_), (1.d0))
! DEF_VARI1_MOD(sinh_fvari_mod, fz_fvari, dsinh(vi%val_), (dcosh(a%val_)))
! DEF_VARI1_MOD(cosh_fvari_mod, fz_fvari, dcosh(vi%val_), (dsinh(a%val_)))
! DEF_VARI1_MOD(tanh_fvari_mod, fz_fvari, dtanh(vi%val_), (1.d0/(dcosh(a%val_)*dcosh(a%val_))) )
! DEF_VARI1_MOD(square_fvari_mod, fz_fvari, (vi%val_)**2, (2.0d0*a%val_))
! DEF_VARI1_MOD(logit_fvari_mod, fz_fvari, logit_d(vi%val_), (1.d0 / (a%val_ - a%val_ * a%val_)) )
! DEF_VARI1_MOD(inv_logit_fvari_mod, fz_fvari, inv_logit_d(vi%val_), (this%val_ * (1.d0 - this%val_)) )

DEF_VARI2_MOD(add_fvari_mod, fz_fvari, (vi_val(a) + vi_val(b)), (1.d0), (1.d0))
DEF_VARI2_MOD(sub_fvari_mod, fz_fvari, (vi_val(a) - vi_val(b)), (1.d0), (-1.d0))
DEF_VARI2_MOD(mul_fvari_mod, fz_fvari, (vi_val(a) * vi_val(b)), (vi_val(b)), (vi_val(a)))
DEF_VARI2_MOD(div_fvari_mod, fz_fvari, (vi_val(a)/vi_val(b)), (1.d0/vi_val(b)), (-this%val_/vi_val(b)))

module sum_fvari_mod
  use, intrinsic :: iso_fortran_env
  use, intrinsic :: iso_c_binding
  use fz_env
  use fz_vari

  implicit none

  type, extends(chain_base) :: vi_chain
   contains
     procedure, nopass :: chain => chain_sum
  end type vi_chain
  type( vi_chain ), target :: vi_chain_instance

contains

  subroutine chain_sum (ip)
    implicit none
    integer(ik), intent(in) :: ip
    type(vari), pointer :: va, this
    ! type(fvari), pointer :: va
    ! integer(ik) :: i, j, k, n
    ! k = this%i + visize
    ! call core_adstack%pop(j, n)
    ! do i = 1, n
    !    k = this%i + visize + i*iksize
    !    call core_adstack%pop(k, j)
    !    call recover(va, j)
    !    va%adj_ = va%adj_ + this%adj_
    ! enddo
  end subroutine chain_sum

end module sum_fvari_mod

! module fz_fvari_op
!   use fz_env
!   use fz_fvari

! contains

!   ! DEF_OP1(fvari, sinh, dsinh(vi%val_), (dcosh(a%val_)))

!   ! DEF_OP1(fvari, cosh, dcosh(vi%val_), (dsinh(a%val_)))

!   ! DEF_OP1(fvari, tanh, dtanh(vi%val_), (1.d0/(dcosh(a%val_)*dcosh(a%val_))) )

!   ! elemental real(rk) function logit_d(d)
!   !   implicit none
!   !   real(rk), intent(in) :: d
!   !   logit_d = log(d / (1.d0 - d))
!   ! end function logit_d
!   ! DEF_OP1(fvari, logit, logit_d(vi%val_), (1.d0 / (a%val_ - a%val_ * a%val_)) )

!   ! elemental function inv_logit_d(d) result(s)
!   !   use fz_env, only : log_eps
!   !   implicit none
!   !   real(rk), intent(in) :: d
!   !   real(rk) :: s, exp_d
!   !   if ( d < 0.d0 ) then
!   !      exp_d = exp(d)
!   !      if (d < log_eps) then
!   !         s = exp_d
!   !      else
!   !         s = exp_d / (1.d0 + exp_d);
!   !      endif
!   !   else
!   !      s = 1.d0/(1.d0 + exp(-d))
!   !   endif
!   ! end function inv_logit_d
!   ! DEF_OP1(fvari, inv_logit, inv_logit_d(vi%val_), (this%val_ * (1.d0 - this%val_)) )

!   DEF_OP2_VD(fvari, add, (va%val_ + b), (1.d0))
!   DEF_OP2_DV(fvari, add, (vb%val_ + a), (1.d0))
!   DEF_OP2_VV(fvari, add, (vb%val_ + va%val_), (1.d0), (1.d0))

!   DEF_OP2_VD(fvari, sub, (va%val_ - b), (1.d0))
!   DEF_OP2_DV(fvari, sub, (a - vb%val_), (-1.d0))
!   DEF_OP2_VV(fvari, sub, (va%val_ - vb%val_), (1.d0), (-1.d0))

!   DEF_OP2_VD(fvari, mul, (va%val_ * b), (b))
!   DEF_OP2_DV(fvari, mul, (a * vb%val_), (a))
!   DEF_OP2_VV(fvari, mul, (va%val_ * vb%val_), (vb%val_), (va%val_))

!   DEF_OP2_VD(fvari, div, (va%val_/b), (1.d0/b))
!   DEF_OP2_DV(fvari, div, (a/vb%val_), (-this%val_/vb%val_))
!   DEF_OP2_VV(fvari, div, (va%val_/vb%val_), (1.d0/vb%val_), (-this%val_/vb%val_))


! end module fz_fvari_op
