#include "vari_op_inc.f90"

module fz_vari_op
  use, intrinsic :: iso_fortran_env
  use, intrinsic :: iso_c_binding
  use fz_env
  use fz_vari

  implicit none

contains

  DEF_OP1(vari, exp, dexp(vi%val_), (this%val_))

  DEF_OP1(vari, sin, dsin(vi%val_), (cos(a%val_)))

  DEF_OP1(vari, cos, dcos(vi%val_), (-sin(a%val_)))

  DEF_OP1(vari, tan, dtan(vi%val_), (1.d0/(cos(a%val_)*cos(a%val_))))

  DEF_OP1(vari, asin, dasin(vi%val_), (1.d0/sqrt(1.d0-a%val_*a%val_)))

  DEF_OP1(vari, acos, dacos(vi%val_), (-1.d0/sqrt(1.d0-a%val_*a%val_)))

  DEF_OP1(vari, atan, datan(vi%val_), (1.d0/(1.d0+a%val_*a%val_)))

  DEF_OP1(vari, log, dlog(vi%val_), (1.d0/a%val_))

  DEF_OP1(vari, log10, dlog10(vi%val_), (1.d0/(a%val_*dlog(10.d0))))

  DEF_OP1(vari, sqrt, dsqrt(vi%val_), (0.5d0/dsqrt(a%val_)))

  DEF_OP1(vari, neg, (-vi%val_), (-1.d0))

  DEF_OP1(vari, pos, (vi%val_), (1.d0))

  DEF_OP1(vari, sinh, dsinh(vi%val_), (dcosh(a%val_)))

  DEF_OP1(vari, cosh, dcosh(vi%val_), (dsinh(a%val_)))

  DEF_OP1(vari, tanh, dtanh(vi%val_), (1.d0/(dcosh(a%val_)*dcosh(a%val_))) )

  elemental real(rk) function logit_d(d)
    implicit none
    real(rk), intent(in) :: d
    logit_d = log(d / (1.d0 - d))
  end function logit_d
  DEF_OP1(vari, logit, logit_d(vi%val_), (1.d0 / (a%val_ - a%val_ * a%val_)) )

  elemental function inv_logit_d(d) result(s)
    use fz_env, only : log_eps
    implicit none
    real(rk), intent(in) :: d
    real(rk) :: s, exp_d
    if ( d < 0.d0 ) then
       exp_d = exp(d)
       if (d < log_eps) then
          s = exp_d
       else
          s = exp_d / (1.d0 + exp_d);
       endif
    else
       s = 1.d0/(1.d0 + exp(-d))
    endif
  end function inv_logit_d
  DEF_OP1(vari, inv_logit, inv_logit_d(vi%val_), (this%val_ * (1.d0 - this%val_)) )

  DEF_OP2_VD(vari, add, (va%val_ + b), (1.d0))
  DEF_OP2_DV(vari, add, (vb%val_ + a), (1.d0))
  DEF_OP2_VV(vari, add, (vb%val_ + va%val_), (1.d0), (1.d0))

  DEF_OP2_VD(vari, sub, (va%val_ - b), (1.d0))
  DEF_OP2_DV(vari, sub, (a - vb%val_), (-1.d0))
  DEF_OP2_VV(vari, sub, (va%val_ - vb%val_), (1.d0), (-1.d0))

  DEF_OP2_VD(vari, mul, (va%val_ * b), (b))
  DEF_OP2_DV(vari, mul, (a * vb%val_), (a))
  DEF_OP2_VV(vari, mul, (va%val_ * vb%val_), (vb%val_), (va%val_))

  DEF_OP2_VD(vari, div, (va%val_/b), (1.d0/b))
  DEF_OP2_DV(vari, div, (a/vb%val_), (-this%val_/vb%val_))
  DEF_OP2_VV(vari, div, (va%val_/vb%val_), (1.d0/vb%val_), (-this%val_/vb%val_))

  DEF_OP2_VD(vari, pow, ((va%val_) ** (b)), (b*(va%val_)**(b-1)))
  DEF_OP2_DV(vari, pow, ((a) ** (vb%val_)), (a**(vb%val_)*log(a)) )
  DEF_OP2_VV(vari, pow, ((va%val_) ** (vb%val_)), ((vb%val_)*(va%val_)**(vb%val_-1)), ((va%val_)**(vb%val_)*log(va%val_)) )

  subroutine chain_sum (this)
    implicit none
    type(vari), pointer, intent(in) :: this
    type(vari), pointer :: va
    integer(ik) :: i, j, n
    n = core_adstack%pop_int(this%i + visize)
    do i = 1, n
       call recover(va, core_adstack%pop_int(this%i + visize + i*iksize))
       va%adj_ = va%adj_ + this%adj_
    enddo
  end subroutine chain_sum

end module fz_vari_op
