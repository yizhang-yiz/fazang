#include "test_macro.fi"

module func
  use env_mod
  use var_mod
  use pow_mod
  use add_mod
  use sub_mod
  use log_mod
  use mul_mod
  use div_mod
  implicit none

  real(rk), parameter :: y = 1.3d0

contains
  type(var) function f(x)
    type(var), intent(in) :: x(:)
    type(var) :: mu, sigma
    mu = x(1)
    sigma = x(2)
    f = -0.5d0 * log(2 * pi) - log(sigma) - 0.5d0 * ((y - mu) / sigma) ** 2.d0;
  end function f

end module func

program grad_test
  use iso_c_binding
  use test_mod
  use env_mod
  use var_mod
  use grad_mod
  use func

  implicit none
  
  real(rk) :: fx(3), x(2), fx_d, d_sigma, d_mu
  x = [0.5d0, 1.2d0]

  fx = gradient(f, x)
  fx_d = -0.5d0 * log(2 * pi) - log(1.2d0) - 0.5d0 * ((y - 0.5d0) / 1.2d0) ** 2.d0
  EXPECT_DBL_EQ(fx(1), fx_d)
  d_mu = (y - x(1)) / (x(2) * x(2))
  d_sigma = - 1.d0/x(2) + (y - x(1)) ** 2 / (x(2) ** 3)
  EXPECT_DBL_EQ(fx(2), d_mu)
  EXPECT_DBL_EQ(fx(3), d_sigma)

end program grad_test
