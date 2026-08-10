#include "vari_op_inc.f90"

module fz_vari_prob
  use, intrinsic :: iso_fortran_env
  use, intrinsic :: iso_c_binding
  use fz_env
  use fz_vari

  implicit none

contains

DEF_OP2_VD(vari, exponential_lpdf, log(va%val_) - va%val_*b, (1.0d0/va%val_ - b))
DEF_OP2_VI(vari, bernoulli_lpmf, b*log(va%val_) + (1_ik-b)*log(1.0d0-va%val_), b/va%val_ - (1_ik-b)/(1.0d0-va%val_))

! normal
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
DEF_OP3_VDD(vari, normal_lpdf, normal_lpdf_d_d_d(va%val_, b, c), ((c - va%val_) / (b * b)))
DEF_OP3_DVD(vari, normal_lpdf, normal_lpdf_d_d_d(a, vb%val_, c), normal_dsigma(a, vb%val_, c))
DEF_OP3_VVD(vari, normal_lpdf, normal_lpdf_d_d_d(va%val_, vb%val_, c), ((c - va%val_) / (vb%val_ * vb%val_)), normal_dsigma(va%val_, vb%val_, c))

! lognormal
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
DEF_OP3_VDD(vari, lognormal_lpdf, lognormal_lpdf_d_d_d(va%val_, b, c), (log(c) - va%val_) / (b * b))
DEF_OP3_DVD(vari, lognormal_lpdf, lognormal_lpdf_d_d_d(a, vb%val_, c), lognormal_dsigma(a, vb%val_, c))
DEF_OP3_VVD(vari, lognormal_lpdf, lognormal_lpdf_d_d_d(va%val_, vb%val_, c), (log(c) - va%val_) / (vb%val_ * vb%val_), lognormal_dsigma(va%val_, vb%val_, c))

! weibull
! elemental function weibull_loglik(shape, scale, y, loglik, dshape, dscale)
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
DEF_OP3_VDD(vari, weibull_lpdf, weibull_lpdf_d_d_d(va%val_, b, c), weibull_dshape(va%val_, b, c))
DEF_OP3_DVD(vari, weibull_lpdf, weibull_lpdf_d_d_d(a, vb%val_, c), weibull_dscale(a, vb%val_, c))
DEF_OP3_VVD(vari, weibull_lpdf, weibull_lpdf_d_d_d(va%val_, vb%val_, c), weibull_dshape(va%val_, vb%val_, c), weibull_dscale(va%val_, vb%val_, c))

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
DEF_OP3_VDD(vari, cauchy_lpdf, cauchy_lpdf_d_d_d(va%val_, b, c), cauchy_dloc(va%val_, b, c))
DEF_OP3_DVD(vari, cauchy_lpdf, cauchy_lpdf_d_d_d(a, vb%val_, c), cauchy_dscale(a, vb%val_, c))
DEF_OP3_VVD(vari, cauchy_lpdf, cauchy_lpdf_d_d_d(va%val_, vb%val_, c), cauchy_dloc(va%val_, vb%val_, c), cauchy_dscale(va%val_, vb%val_, c))

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
DEF_OP3_VDD(vari, gumbel_lpdf, gumbel_lpdf_d_d_d(va%val_, b, c), gumbel_dloc(va%val_, b, c))
DEF_OP3_DVD(vari, gumbel_lpdf, gumbel_lpdf_d_d_d(a, vb%val_, c), gumbel_dscale(a, vb%val_, c))
DEF_OP3_VVD(vari, gumbel_lpdf, gumbel_lpdf_d_d_d(va%val_, vb%val_, c), gumbel_dloc(va%val_, vb%val_, c), gumbel_dscale(va%val_, vb%val_, c))

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
DEF_OP3_VDD(vari, logistic_lpdf, logistic_lpdf_d_d_d(va%val_, b, c), logistic_dloc(va%val_, b, c))
DEF_OP3_DVD(vari, logistic_lpdf, logistic_lpdf_d_d_d(a, vb%val_, c), logistic_dscale(a, vb%val_, c))
DEF_OP3_VVD(vari, logistic_lpdf, logistic_lpdf_d_d_d(va%val_, vb%val_, c), logistic_dloc(va%val_, vb%val_, c), logistic_dscale(va%val_, vb%val_, c))


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
DEF_OP3_VDD(vari, laplace_lpdf, laplace_lpdf_d_d_d(va%val_, b, c), laplace_dloc(va%val_, b, c))
DEF_OP3_DVD(vari, laplace_lpdf, laplace_lpdf_d_d_d(a, vb%val_, c), laplace_dscale(a, vb%val_, c))
DEF_OP3_VVD(vari, laplace_lpdf, laplace_lpdf_d_d_d(va%val_, vb%val_, c), laplace_dloc(va%val_, vb%val_, c), laplace_dscale(va%val_, vb%val_, c))

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
DEF_OP2_VD(vari, chi_square_lpdf, chi_square_lpdf_d_d(va%val_, b), chi_square_dnu(va%val_, b))

end module fz_vari_prob
