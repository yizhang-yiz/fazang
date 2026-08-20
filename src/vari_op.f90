#include "vari_op_inc.f90"

module fz_prim_op
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

elemental function gumbel_lpdf_d_d_d(loc, scale, y) result(loglik)
  implicit none
  real(rk), intent(in) :: loc, scale, y
  real(rk) :: z, ez, loglik
  z = (y - loc) / scale
  ez = exp(-z)
  loglik = -log(scale) - z - ez
end function gumbel_lpdf_d_d_d
elemental function gumbel_dloc(loc, scale, y) result(d)
  implicit none
  real(rk), intent(in) :: loc, scale, y
  real(rk) :: z, ez, d
  z = (y - loc) / scale
  ez = exp(-z)
  d = (1.0d0 - ez) / scale
end function gumbel_dloc
elemental function gumbel_dscale(loc, scale, y) result(d)
  implicit none
  real(rk), intent(in) :: loc, scale, y
  real(rk) :: z, ez, d
  z = (y - loc) / scale
  ez = exp(-z)
  d = (-1.0d0 + z - z * ez) / scale
end function gumbel_dscale

elemental function logistic_lpdf_d_d_d(loc, scale, y) result(loglik)
  implicit none
  real(rk), intent(in) :: loc, scale, y
  real(rk) :: z, ez, t, loglik
  z = (y - loc) / scale
  ez = exp(-z)
  t = tanh(0.5d0 * z)
  loglik = -log(scale) - z - 2.0d0 * log(1.0d0 + ez)
end function logistic_lpdf_d_d_d
elemental function logistic_dloc(loc, scale, y) result(d)
  implicit none
  real(rk), intent(in) :: loc, scale, y
  real(rk) :: z, ez, t, d
  z = (y - loc) / scale
  ez = exp(-z)
  t = tanh(0.5d0 * z)
  d = t/scale
end function
elemental function logistic_dscale(loc, scale, y) result(d)
  implicit none
  real(rk), intent(in) :: loc, scale, y
  real(8) :: z, ez, t, d
  z = (y - loc) / scale
  ez = exp(-z)
  t = tanh(0.5d0 * z)
  d = (-1.0d0 + z * t) / scale
end function logistic_dscale

elemental function laplace_lpdf_d_d_d(loc, scale, y) result(loglik)
  implicit none
  real(rk), intent(in) :: loc, scale, y
  real(rk) :: r, ar, loglik
  r = y - loc; ar = abs(r)
  loglik = -log(2.0d0 * scale) - ar / scale
end function laplace_lpdf_d_d_d
elemental function laplace_dloc(loc, scale, y) result(d)
  implicit none
  real(rk), intent(in) :: loc, scale, y
  real(rk) :: r, ar, d
  r = y - loc
  ar = abs(r)
  d = sign(1.0d0, r) / scale
end function laplace_dloc
elemental function laplace_dscale(loc, scale, y) result(d)
  implicit none
  real(rk), intent(in) :: loc, scale, y
  real(rk) :: r, ar, d
  r = y - loc
  ar = abs(r)
  d = -1.0d0 / scale + ar / (scale * scale)
end function laplace_dscale

pure real(rk) function digamma ( x )

!*****************************************************************************80
!
!! DIGAMMA calculates DIGAMMA ( X ) = d ( LOG ( GAMMA ( X ) ) ) / dX
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2016
!
!  Author:
!
!    Original FORTRAN77 version by Jose Bernardo.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Jose Bernardo,
!    Algorithm AS 103:
!    Psi ( Digamma ) Function,
!    Applied Statistics,
!    Volume 25, Number 3, 1976, pages 315-317.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the digamma function.
!    0 < X.
!
!    Output, integer IFAULT, error flag.
!    0, no error.
!    1, X <= 0.
!
!    Output, real ( kind = 8 ) DIGAMMA, the value of the digamma function at X.
!
  implicit none

  real(rk), parameter :: c = 8.5D+00
  real(rk), parameter :: euler_mascheroni = 0.57721566490153286060D+00
  integer(ik) :: ifault
  real(rk), intent(in) :: x
  real(rk) :: r, x2
!
!  Check the input.
!
  if ( x <= 0.0D+00 ) then
    digamma = 0.0D+00
    ifault = 1
    return
  end if
!
!  Initialize.
!
  ifault = 0
!
!  Approximation for small argument.
!
  if ( x <= 0.000001D+00 ) then
    digamma = - euler_mascheroni - 1.0D+00 / x + 1.6449340668482264365D+00 * x
    return
  end if
!
!  Reduce to DIGAMA(X + N).
!
  digamma = 0.0D+00
  x2 = x

  do while ( x2 < c )
    digamma = digamma - 1.0D+00 / x2
    x2 = x2 + 1.0D+00
  end do
!
!  Use Stirling's (actually de Moivre's) expansion.
!
  r = 1.0D+00 / x2

  digamma = digamma + log ( x2 ) - 0.5D+00 * r

  r = r * r

  digamma = digamma &
    - r * ( 1.0D+00 / 12.0D+00 &
    - r * ( 1.0D+00 / 120.0D+00 &
    - r * ( 1.0D+00 / 252.0D+00 &
    - r * ( 1.0D+00 / 240.0D+00 &
    - r * ( 1.0D+00 / 132.0D+00 ) ) ) ) )
end function digamma

elemental function chi_square_lpdf_d_d(nu, y) result(loglik)
  real(rk), intent(in) :: nu, y
  real(rk) :: loglik
  loglik = (0.5d0 * nu - 1.0d0) * log(y) - 0.5d0 * y - 0.5d0 * nu * log(2.0d0) - log(gamma(0.5d0 * nu))
end function chi_square_lpdf_d_d
elemental function chi_square_dnu(nu, y) result(d)
  real(rk), intent(in) :: nu, y
  real(rk) :: d
  d = 0.5d0 * log(y) - 0.5d0 * log(2.0d0) - 0.5d0 * digamma(0.5d0 * nu)
end function chi_square_dnu

elemental function inv_chi_square_lpdf_d_d(nu, y) result(loglik)
  real(rk), intent(in) :: nu, y
  real(rk) :: loglik
  real(rk) :: half_nu
  half_nu = 0.5d0*nu
  loglik = -half_nu*log2-log(gamma(half_nu))-(half_nu+1.d0)*log(y)-0.5d0/y
end function
elemental function inv_chi_square_dnu(nu, y) result(d)
  real(rk), intent(in) :: nu, y
  real(rk) :: d
  real(rk) :: half_nu
  half_nu = 0.5d0*nu
  d = -0.5d0*(log2 + digamma(half_nu) + log(y))
end function
end module fz_prim_op

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
