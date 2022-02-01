#include "test_macro.fi"

program var_grad_test
  use iso_c_binding
  use test_mod
  use fazang

  implicit none
  
  real(rk) :: y, fx_d, x(2), d_mu, d_sigma
  type(var) :: f, sigma, mu

  ! data
  y = 1.3d0

  ! independent variables
  mu = var(0.5d0)
  sigma = var(1.2d0)
  x = val([mu, sigma])

  ! dependent
  f = var(-0.5d0 * log(2 * pi))
  f = f - log(sigma)
  f = f - 0.5d0 * ((y - mu) / sigma) ** 2.d0;

  ! use grad() to calculate df/d(mu) and df/d(sigma). Each var's
  ! derivative (also called adjoint) can be access through var%adj().
  call f%grad()
  fx_d = -0.5d0 * log(2 * pi) - log(1.2d0) - 0.5d0 * ((y - 0.5d0) / 1.2d0) ** 2.d0
  EXPECT_DBL_EQ(f%val(), fx_d)
  d_mu = (y - x(1)) / (x(2) * x(2))
  d_sigma = - 1.d0/x(2) + (y - x(1)) ** 2 / (x(2) ** 3)
  EXPECT_DBL_EQ(mu%adj(), d_mu)
  EXPECT_DBL_EQ(sigma%adj(), d_sigma)

end program var_grad_test
