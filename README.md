- [Fazang](#orgbff6fc3)
  - [Quick start](#org147a984)
  - [Use `Fazang`](#org1e03de7)
  - [Name](#org5b8df80)


<a id="orgbff6fc3"></a>

# Fazang

`Fazang` is a Fortran library for reverse-mode automatic differentiation, inspired by [Stan/Math library](https://mc-stan.org/users/interfaces/math).


<a id="org147a984"></a>

## Quick start

`Fazang` provides `var` type as dependent and independent variables whose derivatives will be calculated.

```fortran
program var_grad_test
  use iso_c_binding
  use fazang ! load Fazang library

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
  write(*, *) "df/d(mu): ", mu%adj()
  write(*, *) "df/d(sigma): ", sigma%adj()
end program var_grad_test
#+end_src fortran

An alternative to the same problem above is to write a function then
use it as a procedure argument to =Fazang= 's =gradient= funcion.
#+begin_src fortran
module func
  use iso_c_binding
  use fazang ! load Fazang library
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
  use fazang
  use func

  implicit none

  real(rk) :: fx(3), x(2)
  x = [0.5d0, 1.2d0]

  fx = gradient(f, x)
  write(*, *) "f(x): ", fx(1)
  write(*, *) "df/d(x(1)): ", fx(2)
  write(*, *) "df/d(x(2)): ", fx(3)
end program grad_test
```


<a id="org1e03de7"></a>

## Use `Fazang`


<a id="org5b8df80"></a>

## Name

The library is named after ancient Chinese philosopher Fazang (法藏), who follows the view of cosmos "as an infinite number of interdependent and interpenetrating parts" (一法为因，万法为果；万法为因，一法为果).
