#include "vari_op_inc.f90"

module fz_real_op
  use fz_env
  use fz_vari
contains

  elemental real(rk) function logit_d(d)
    implicit none
    real(rk), intent(in) :: d
    logit_d = log(d / (1.d0 - d))
  end function logit_d

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

  elemental function normal_lpdf_d_d_d(mu, sigma, y) result(loglik)
    implicit none
    real(rk), intent(in) :: mu, sigma, y
    real(rk) :: z, loglik
    z = (y - mu) / sigma
    loglik = -0.5d0 * log(2.0d0 * pi) - log(sigma) - 0.5d0 * z * z
  end function normal_lpdf_d_d_d
  elemental function normal_dsigma(mu, sigma, y) result(d)
    implicit none
    real(rk), intent(in) :: mu, sigma, y
    real(rk) :: z, d
    z = (y - mu) / sigma
    d = -1.0d0 / sigma + z * z / sigma
  end function normal_dsigma

  elemental function lognormal_lpdf_d_d_d(mu, sigma, y) result(loglik)
    implicit none
    real(rk), intent(in) :: mu, sigma, y
    real(rk) :: z, ly, loglik
    ly = log(y); z = (ly - mu) / sigma
    loglik = -ly - log(sigma) - 0.5d0 * log(2.0d0 * pi) - 0.5d0 * z * z
  end function lognormal_lpdf_d_d_d
  elemental function lognormal_dsigma(mu, sigma, y) result(d)
    implicit none
    real(rk), intent(in) :: mu, sigma, y
    real(rk) :: z, ly, d
    ly = log(y); z = (ly - mu) / sigma
    d = -1.0d0 / sigma + z * z / sigma
  end function lognormal_dsigma

elemental function weibull_lpdf_d_d_d(shape, scale, y) result(loglik)
  implicit none
  real(rk), intent(in) :: shape, scale, y
  real(rk) :: loglik, z
  z = y / scale
  loglik = log(shape) - shape * log(scale) + (shape - 1.0d0) * log(y) - z**shape
end function weibull_lpdf_d_d_d
elemental function weibull_dshape(shape, scale, y) result(d)
  implicit none
  real(rk), intent(in) :: shape, scale, y
  real(rk) :: d, z, lz
  z = y / scale
  lz = log(z)
  d = 1.0d0 / shape - log(scale) + log(y) - z**shape * lz
end function weibull_dshape
elemental function weibull_dscale(shape, scale, y) result(d)
  implicit none
  real(rk), intent(in) :: shape, scale, y
  real(rk) :: d, z
  z = y / scale
  d = (shape / scale) * (z**shape - 1.0d0)
end function weibull_dscale

elemental function cauchy_lpdf_d_d_d(loc, scale, y) result(loglik)
  implicit none
  real(rk), intent(in) :: loc, scale, y
  real(rk) :: loglik, r, rs
  r = y - loc; rs = r / scale
  loglik = -log(pi) - log(scale) - log(1.0d0 + rs * rs)
end function cauchy_lpdf_d_d_d
elemental function cauchy_dloc(loc, scale, y) result(d)
  implicit none
  real(rk), intent(in) :: loc, scale, y
  real(rk) :: d, r, rs
  r = y - loc
  d = 2.0d0 * r / (scale * scale + r * r)
end function cauchy_dloc
elemental function cauchy_dscale(loc, scale, y) result(d)
  implicit none
  real(rk), intent(in) :: loc, scale, y
  real(rk) :: d, r, rs
  r = y - loc
  d = -1.0d0 / scale + 2.0d0 * r * r / (scale * (scale * scale + r * r))
end function cauchy_dscale
end module fz_real_op

module sum_vi_mod
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
    integer(ik) :: i
    integer(ik), pointer :: n
    integer(ik), pointer :: p(:)
    call recover(ip, this)
    call c_f_pointer(c_loc(core_adstack%s_(ip+visize)), n)
    call c_f_pointer(c_loc(core_adstack%s_(ip+v_visize)), p, [n])
    do i = 1, n
       call recover(p(i), va)
       va%adj_ = va%adj_ + this%adj_
    enddo
  end subroutine chain_sum

end module sum_vi_mod

DEF_VARI1_MOD(vari, exp, dexp(vi%val_), (this%val_))
DEF_VARI1_MOD(vari, sin, dsin(vi%val_), (cos(a%val_)))
DEF_VARI1_MOD(vari, cos, dcos(vi%val_), (-sin(a%val_)))
DEF_VARI1_MOD(vari, tan, dtan(vi%val_), (1.d0/(cos(a%val_)*cos(a%val_))))
DEF_VARI1_MOD(vari, asin, dasin(vi%val_), (1.d0/sqrt(1.d0-a%val_*a%val_)))
DEF_VARI1_MOD(vari, acos, dacos(vi%val_), (-1.d0/sqrt(1.d0-a%val_*a%val_)))
DEF_VARI1_MOD(vari, atan, datan(vi%val_), (1.d0/(1.d0+a%val_*a%val_)))
DEF_VARI1_MOD(vari, log, dlog(vi%val_), (1.d0/a%val_))
DEF_VARI1_MOD(vari, log10, dlog10(vi%val_), (1.d0/(a%val_*dlog(10.d0))))
DEF_VARI1_MOD(vari, sqrt, dsqrt(vi%val_), (0.5d0/dsqrt(a%val_)))
DEF_VARI1_MOD(vari, neg, (-vi%val_), (-1.d0))
DEF_VARI1_MOD(vari, pos, (vi%val_), (1.d0))
DEF_VARI1_MOD(vari, sinh, dsinh(vi%val_), (dcosh(a%val_)))
DEF_VARI1_MOD(vari, cosh, dcosh(vi%val_), (dsinh(a%val_)))
DEF_VARI1_MOD(vari, tanh, dtanh(vi%val_), (1.d0/(dcosh(a%val_)*dcosh(a%val_))) )
DEF_VARI1_MOD(vari, square, (vi%val_)**2, (2.0d0*a%val_))
DEF_VARI1_MOD(vari, logit, logit_d(vi%val_), (1.d0 / (a%val_ - a%val_ * a%val_)) )
DEF_VARI1_MOD(vari, inv_logit, inv_logit_d(vi%val_), (this%val_ * (1.d0 - this%val_)) )

DEF_VARI2_MOD(vari, add, (vi_val(a) + vi_val(b)), (1.d0), (1.d0))
DEF_VARI2_MOD(vari, sub, (vi_val(a) - vi_val(b)), (1.d0), (-1.d0))
DEF_VARI2_MOD(vari, mul, (vi_val(a) * vi_val(b)), (vi_val(b)), (vi_val(a)))
DEF_VARI2_MOD(vari, div, (vi_val(a)/vi_val(b)), (1.d0/vi_val(b)), (-this%val_/vi_val(b)))
DEF_VARI2_MOD(vari, pow, ((vi_val(a)) ** (vi_val(b))), ((vi_val(b))*(vi_val(a))**(vi_val(b)-1)), ((vi_val(a))**(vi_val(b))*log(vi_val(a))) )

! prob
DEF_VARI1_INT_MOD(vari, bernoulli_lpmf, n*log(vi%val_) + (1_ik-n)*log(1.0d0-vi%val_), (this%val_ * (1.d0 - this%val_)) )

DEF_VARI2_REAL_MOD(vari, normal_lpdf, normal_lpdf_d_d_d(vi_val(a), vi_val(b), c), ((c - vi_val(a)) / (vi_val(b) * vi_val(b))), normal_dsigma(vi_val(a), vi_val(b), c))

DEF_VARI2_REAL_MOD(vari, lognormal_lpdf, lognormal_lpdf_d_d_d(vi_val(a), vi_val(b), c), (log(c) - vi_val(a)) / (vi_val(b) * vi_val(b)), lognormal_dsigma(vi_val(a), vi_val(b), c))

DEF_VARI2_REAL_MOD(vari, weibull_lpdf, weibull_lpdf_d_d_d(vi_val(a), vi_val(b), c), weibull_dshape(vi_val(a), vi_val(b), c), weibull_dscale(vi_val(a), vi_val(b), c))

DEF_VARI2_REAL_MOD(vari, cauchy_lpdf, cauchy_lpdf_d_d_d(vi_val(a), vi_val(b), c), cauchy_dloc(vi_val(a), vi_val(b), c), cauchy_dscale(vi_val(a), vi_val(b), c))
