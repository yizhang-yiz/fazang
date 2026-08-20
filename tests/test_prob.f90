#include "assert_inc.f90"

module test_lpdf
  use fz_env, only : ik, rk
  use fz_var
  implicit none

contains

  subroutine test_normal_lpdf()
    implicit none
    real(rk), parameter :: x     = 2.3_rk
    real(rk), parameter :: mean0 = 1.1_rk
    real(rk), parameter :: sd0   = 0.7_rk
    real(rk), parameter :: tol   = 1.0e-15_rk

    real(rk), parameter :: expected_dmean = (x - mean0) / sd0**2
    real(rk), parameter :: expected_dsd   = -1.0_rk / sd0 + (x - mean0)**2 / sd0**3
    real(rk), parameter :: expected_lp = -log(sd0) - 0.5_rk * log(2.0_rk * acos(-1.0_rk)) &
                  - 0.5_rk * ((x - mean0) / sd0)**2

    type(var) :: mean
    type(var) :: sd
    type(var) :: lp

    ! Check the all-real overload and the primal value.
    ASSERT_TOL(normal_lpdf(mean0, sd0, x), expected_lp, TOL)

    mean = mean0; lp = normal_lpdf(mean, sd0, x); call grad(lp)
    ASSERT_TOL(val(lp), expected_lp, TOL)
    ASSERT_TOL(adj(mean), expected_dmean, TOL)
    call reset_adj()

    sd = sd0; lp = normal_lpdf(mean0, sd, x); call grad(lp)
    ASSERT_TOL(val(lp), expected_lp, TOL)
    ASSERT_TOL(adj(sd), expected_dsd, TOL)
    call reset_adj()

    mean = mean0; sd = sd0; lp = normal_lpdf(mean, sd, x); call grad(lp)
    ASSERT_TOL(val(lp), expected_lp, TOL)
    ASSERT_TOL(adj(sd), expected_dsd, TOL)
    ASSERT_TOL(adj(mean), expected_dmean, TOL)
    call reset_adj()
  end subroutine test_normal_lpdf

  subroutine test_lognormal_lpdf()
    implicit none

    real(rk), parameter :: x     = 2.3_rk
    real(rk), parameter :: mean0 = 0.3_rk
    real(rk), parameter :: sd0   = 0.7_rk
    real(rk), parameter :: tol   = 1.0e-12_rk

    real(rk), parameter :: log_x = log(x)
    real(rk), parameter :: expected_dmean = (log_x - mean0) / sd0**2
    real(rk), parameter :: expected_dsd   = -1.0_rk / sd0 + (log_x - mean0)**2 / sd0**3
    real(rk), parameter :: expected_lp = -log_x - log(sd0) &
      - 0.5_rk * log(2.0_rk * acos(-1.0_rk)) &
      - 0.5_rk * ((log_x - mean0) / sd0)**2

    type(var) :: mean, sd, lp

    ASSERT_TOL(lognormal_lpdf(mean0, sd0, x), expected_lp, tol)

    mean = mean0
    lp = lognormal_lpdf(mean, sd0, x)
    call grad(lp)
    ASSERT_TOL(val(lp), expected_lp, tol)
    ASSERT_TOL(adj(mean), expected_dmean, tol)
    call reset_adj()

    sd = sd0
    lp = lognormal_lpdf(mean0, sd, x)
    call grad(lp)
    ASSERT_TOL(val(lp), expected_lp, tol)
    ASSERT_TOL(adj(sd), expected_dsd, tol)
    call reset_adj()

    mean = mean0
    sd = sd0
    lp = lognormal_lpdf(mean, sd, x)
    call grad(lp)
    ASSERT_TOL(val(lp), expected_lp, tol)
    ASSERT_TOL(adj(mean), expected_dmean, tol)
    ASSERT_TOL(adj(sd), expected_dsd, tol)
    call reset_adj()
  end subroutine test_lognormal_lpdf

  subroutine test_weibull_lpdf()
    implicit none

    real(rk), parameter :: x      = 2.3_rk
    real(rk), parameter :: shape0 = 1.7_rk
    real(rk), parameter :: scale0 = 0.9_rk
    real(rk), parameter :: tol    = 1.0e-12_rk

    real(rk), parameter :: z = x / scale0
    real(rk), parameter :: log_z = log(z)
    real(rk), parameter :: z_shape = z**shape0

    real(rk), parameter :: expected_dshape = 1.0_rk / shape0 + log_z - z_shape * log_z
    real(rk), parameter :: expected_dscale = shape0 * (z_shape - 1.0_rk) / scale0
    real(rk), parameter :: expected_lp = log(shape0) - log(scale0) &
         + (shape0 - 1.0_rk) * log_z - z_shape

    type(var) :: shape, scale, lp

    ASSERT_TOL(weibull_lpdf(shape0, scale0, x), expected_lp, tol)

    shape = shape0
    lp = weibull_lpdf(shape, scale0, x)
    call grad(lp)
    ASSERT_TOL(val(lp), expected_lp, tol)
    ASSERT_TOL(adj(shape), expected_dshape, tol)
    call reset_adj()

    scale = scale0
    lp = weibull_lpdf(shape0, scale, x)
    call grad(lp)
    ASSERT_TOL(val(lp), expected_lp, tol)
    ASSERT_TOL(adj(scale), expected_dscale, tol)
    call reset_adj()

    shape = shape0
    scale = scale0
    lp = weibull_lpdf(shape, scale, x)
    call grad(lp)
    ASSERT_TOL(val(lp), expected_lp, tol)
    ASSERT_TOL(adj(shape), expected_dshape, tol)
    ASSERT_TOL(adj(scale), expected_dscale, tol)
    call reset_adj()
  end subroutine test_weibull_lpdf

  subroutine test_cauchy_lpdf()
    implicit none

    real(rk), parameter :: x        = 2.3_rk
    real(rk), parameter :: location = 1.1_rk
    real(rk), parameter :: scale0   = 0.7_rk
    real(rk), parameter :: tol      = 1.0e-12_rk

    real(rk), parameter :: z = (x - location) / scale0
    real(rk), parameter :: expected_dlocation = 2.0_rk * z / (scale0 * (1.0_rk + z**2))
    real(rk), parameter :: expected_dscale = (z**2 - 1.0_rk) / (scale0 * (1.0_rk + z**2))
    real(rk), parameter :: expected_lp = -log(acos(-1.0_rk)) - log(scale0) - log(1.0_rk + z**2)

    type(var) :: loc, scale, lp

    ASSERT_TOL(cauchy_lpdf(location, scale0, x), expected_lp, tol)

    loc = location
    lp = cauchy_lpdf(loc, scale0, x)
    call grad(lp)
    ASSERT_TOL(val(lp), expected_lp, tol)
    ASSERT_TOL(adj(loc), expected_dlocation, tol)
    call reset_adj()

    scale = scale0
    lp = cauchy_lpdf(location, scale, x)
    call grad(lp)
    ASSERT_TOL(val(lp), expected_lp, tol)
    ASSERT_TOL(adj(scale), expected_dscale, tol)
    call reset_adj()

    loc = location
    scale = scale0
    lp = cauchy_lpdf(loc, scale, x)
    call grad(lp)
    ASSERT_TOL(val(lp), expected_lp, tol)
    ASSERT_TOL(adj(loc), expected_dlocation, tol)
    ASSERT_TOL(adj(scale), expected_dscale, tol)
    call reset_adj()
  end subroutine test_cauchy_lpdf


  subroutine test_gumbel_lpdf()
    implicit none

    real(rk), parameter :: x        = 2.3_rk
    real(rk), parameter :: location = 1.1_rk
    real(rk), parameter :: scale0   = 0.7_rk
    real(rk), parameter :: tol      = 1.0e-12_rk

    real(rk), parameter :: z = (x - location) / scale0
    real(rk), parameter :: exp_neg_z = exp(-z)
    real(rk), parameter :: expected_dlocation = (1.0_rk - exp_neg_z) / scale0
    real(rk), parameter :: expected_dscale = (-1.0_rk + z * (1.0_rk - exp_neg_z)) / scale0
    real(rk), parameter :: expected_lp = -log(scale0) - z - exp_neg_z

    type(var) :: loc, scale, lp

    ASSERT_TOL(gumbel_lpdf(location, scale0, x), expected_lp, tol)

    loc = location
    lp = gumbel_lpdf(loc, scale0, x)
    call grad(lp)
    ASSERT_TOL(val(lp), expected_lp, tol)
    ASSERT_TOL(adj(loc), expected_dlocation, tol)
    call reset_adj()

    scale = scale0
    lp = gumbel_lpdf(location, scale, x)
    call grad(lp)
    ASSERT_TOL(val(lp), expected_lp, tol)
    ASSERT_TOL(adj(scale), expected_dscale, tol)
    call reset_adj()

    loc = location
    scale = scale0
    lp = gumbel_lpdf(loc, scale, x)
    call grad(lp)
    ASSERT_TOL(val(lp), expected_lp, tol)
    ASSERT_TOL(adj(loc), expected_dlocation, tol)
    ASSERT_TOL(adj(scale), expected_dscale, tol)
    call reset_adj()
  end subroutine test_gumbel_lpdf


  subroutine test_logistic_lpdf()
    implicit none

    real(rk), parameter :: x        = 2.3_rk
    real(rk), parameter :: location = 1.1_rk
    real(rk), parameter :: scale0   = 0.7_rk
    real(rk), parameter :: tol      = 1.0e-12_rk

    real(rk), parameter :: z = (x - location) / scale0
    real(rk), parameter :: p = 1.0_rk / (1.0_rk + exp(-z))
    real(rk), parameter :: expected_dlocation = (2.0_rk * p - 1.0_rk) / scale0
    real(rk), parameter :: expected_dscale = (-1.0_rk - z * (1.0_rk - 2.0_rk * p)) / scale0
    real(rk), parameter :: expected_lp = -log(scale0) - z - 2.0_rk * log(1.0_rk + exp(-z))

    type(var) :: loc, scale, lp

    ASSERT_TOL(logistic_lpdf(location, scale0, x), expected_lp, tol)

    loc = location
    lp = logistic_lpdf(loc, scale0, x)
    call grad(lp)
    ASSERT_TOL(val(lp), expected_lp, tol)
    ASSERT_TOL(adj(loc), expected_dlocation, tol)
    call reset_adj()

    scale = scale0
    lp = logistic_lpdf(location, scale, x)
    call grad(lp)
    ASSERT_TOL(val(lp), expected_lp, tol)
    ASSERT_TOL(adj(scale), expected_dscale, tol)
    call reset_adj()

    loc = location
    scale = scale0
    lp = logistic_lpdf(loc, scale, x)
    call grad(lp)
    ASSERT_TOL(val(lp), expected_lp, tol)
    ASSERT_TOL(adj(loc), expected_dlocation, tol)
    ASSERT_TOL(adj(scale), expected_dscale, tol)
    call reset_adj()
  end subroutine test_logistic_lpdf


  subroutine test_laplace_lpdf()
    implicit none

    real(rk), parameter :: x        = 2.3_rk
    real(rk), parameter :: location = 1.1_rk
    real(rk), parameter :: scale0   = 0.7_rk
    real(rk), parameter :: tol      = 1.0e-12_rk

    real(rk), parameter :: distance = abs(x - location)
    real(rk), parameter :: expected_dlocation = 1.0_rk / scale0
    real(rk), parameter :: expected_dscale = -1.0_rk / scale0 + distance / scale0**2
    real(rk), parameter :: expected_lp = -log(2.0_rk * scale0) - distance / scale0

    type(var) :: loc, scale, lp

    ASSERT_TOL(laplace_lpdf(location, scale0, x), expected_lp, tol)

    loc = location
    lp = laplace_lpdf(loc, scale0, x)
    call grad(lp)
    ASSERT_TOL(val(lp), expected_lp, tol)
    ASSERT_TOL(adj(loc), expected_dlocation, tol)
    call reset_adj()

    scale = scale0
    lp = laplace_lpdf(location, scale, x)
    call grad(lp)
    ASSERT_TOL(val(lp), expected_lp, tol)
    ASSERT_TOL(adj(scale), expected_dscale, tol)
    call reset_adj()

    loc = location
    scale = scale0
    lp = laplace_lpdf(loc, scale, x)
    call grad(lp)
    ASSERT_TOL(val(lp), expected_lp, tol)
    ASSERT_TOL(adj(loc), expected_dlocation, tol)
    ASSERT_TOL(adj(scale), expected_dscale, tol)
    call reset_adj()
  end subroutine test_laplace_lpdf


  subroutine test_chi_square_lpdf()
    implicit none

    real(rk), parameter :: x   = 2.3_rk
    real(rk), parameter :: df0 = 4.2_rk
    real(rk), parameter :: tol = 1.0e-12_rk

    ! d/d(df) log chi_square(x | df):
    ! 0.5 * [log(x) - log(2) - digamma(df / 2)]
    real(rk), parameter :: expected_ddf = -0.172787013152337_rk
    real(rk), parameter :: expected_lp = (0.5_rk * df0 - 1.0_rk) * log(x) - 0.5_rk * x &
      - 0.5_rk * df0 * log(2.0_rk) - log_gamma(0.5_rk * df0)

    type(var) :: df, lp

    ASSERT_TOL(chi_square_lpdf(df0, x), expected_lp, tol)

    df = df0
    lp = chi_square_lpdf(df, x)
    call grad(lp)
    ASSERT_TOL(val(lp), expected_lp, tol)
    ASSERT_TOL(adj(df), expected_ddf, tol)
    call reset_adj()
  end subroutine test_chi_square_lpdf


  subroutine test_inv_chi_square_lpdf()
    implicit none

    real(rk), parameter :: x   = 2.3_rk
    real(rk), parameter :: df0 = 4.2_rk
    real(rk), parameter :: tol = 1.0e-12_rk

    ! Standard inverse-chi-square:
    !
    ! p(x | df) = 2**(-df/2) / gamma(df/2)
    !           * x**(-df/2 - 1) * exp(-1 / (2*x))
    !
    ! d/d(df) log p(x | df)
    !   = -0.5 * [log(2) + digamma(df/2) + log(x)]
  real(rk), parameter :: expected_lp = &
    -0.5_rk * df0 * log(2.0_rk) &
    - log_gamma(0.5_rk * df0) &
    - (0.5_rk * df0 + 1.0_rk) * log(x) &
    - 1.0_rk / (2.0_rk * x)

  ! Precomputed because digamma is not an intrinsic Fortran function.
  real(rk), parameter :: expected_ddf = -1.0056961360874408_rk

    type(var) :: df, lp

    ASSERT_TOL(inv_chi_square_lpdf(df0, x), expected_lp, tol)

    df = df0
    lp = inv_chi_square_lpdf(df, x)
    call grad(lp)
    ASSERT_TOL(val(lp), expected_lp, tol)
    ASSERT_TOL(adj(df), expected_ddf, tol)
    call reset_adj()
  end subroutine test_inv_chi_square_lpdf

end module test_lpdf

program test
  use fz_env
  use fz_var
  use test_lpdf
  implicit none

  call test_normal_lpdf()
  call test_lognormal_lpdf()
  call test_weibull_lpdf()
  call test_cauchy_lpdf()
  call test_gumbel_lpdf()
  call test_laplace_lpdf()
  call test_logistic_lpdf()
  call test_chi_square_lpdf()
  call test_inv_chi_square_lpdf()

end program test
