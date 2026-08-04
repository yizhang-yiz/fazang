program var_grad_example

  use fazang ! load Fazang library

  implicit none

  real(rk) :: y, fx_d
  type(var) :: f, sigma, mu

  ! data
  y = 1.3d0

  ! independent variables
  mu = 0.5d0
  sigma = 1.2d0

  ! dependent
  f = -0.5d0 * log(2 * pi)
  f = f - log(sigma)
  f = f - 0.5d0 * ((y - mu) / sigma) ** 2.d0;

  ! use grad() to calculate df/d(mu) and df/d(sigma). Each var's
  ! derivative (also called adjoint) can be access through var%adj().

  call grad(f)
  write(*, *) "df/d(mu): ", adj(mu)
  write(*, *) "df/d(sigma): ", adj(sigma)

end program var_grad_example
