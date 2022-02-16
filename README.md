![build](https://github.com/yizhang-yiz/fazang/actions/workflows/build.yml/badge.svg)
# Fazang

`Fazang` is a Fortran library for reverse-mode automatic differentiation, inspired by [Stan/Math library](https://mc-stan.org/users/interfaces/math).


## Quick start

`Fazang` provides user-facing variable `var` type. It is the type for dependent and independent variables of which derivatives will be calculated.

```fortran
program var_grad_test
  use fazang ! load Fazang library

  implicit none

  real(rk) :: y, fx_d
  type(var) :: f, sigma, mu

  ! data
  y = 1.3d0

  ! independent variables
  mu = var(0.5d0)
  sigma = var(1.2d0)

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
```

An alternative to the same problem above is to write a function then use it as a procedure argument to `Fazang` 's `gradient` funcion.

```fortran
module func
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


## Use `Fazang`

User guide can be found [here](https://github.com/yizhang-yiz/fazang/blob/main/doc/fazang_user_guide.pdf).


### Build

`Fazang` uses `meson` to build.

```bash
cd /path/to/fazang
mkdir build
cd build
meson compile
```

Afterwards one can run the unit tests

```bash
meson test
```


### Use the library

`Fazang` can be accessed by `use fazang` module.

A variable declared `var`

```fortran
type(var) x
```

can be defined as

```fortran
x = var()           ! value of x is 0.d0
x = var(1.5d0)      ! value of x is 1.5d0
```

`Fazang` overloads instrinc arithmatic unary and binary functions. A list of supported functions can be found in [the user guide](https://github.com/yizhang-yiz/fazang/blob/main/doc/fazang_user_guide.pdf).

All the downstream variables that depend on a `var` should also be `var`

```fortran
type(var) x, y
x = var(1.d0)
y = sin(x)
```

The value and the adjoint (derivative) of a `var` can be accessed using `var%val()` and `var%adj()` functions, respectively.

```fortran
write(*, *) y%val()   ! equals to sin(x%val())
write(*, *) y%adj()   ! equals to 0.d0 before any gradient operations
```

`Fazang`'s unary and binary functions are `elemental`, so they can be extended to arrays.

```fortran
type(var) a(3), b, c(3), d
a = var([1.d0, 2.d0, 3.d0])
b = var(0.5d0)
c = 2.d0 * a
d = log(b * a * exp(c))
```

To calculate a dependent variable's derivatives, call `var%grad()` function

```fortran
call d(2)%grad()
```

and access each upstream variable's derivative through `var%adj()` afterwards.

```fortran
write(*, *) c%adj()    ! should be [0.0, 1.0, 0.0]
```

Though `Fazang` uses special storge pattern for array and matrix operations for efficiency purpose, the storage mechanism is transparent to the user.

```fortran
type(var) :: x(4, 2), y(2, 5), z(4, 5)
real(rk) :: a(4, 2) = reshape([1.d0, 47.d0, 3.d0, 53.d0, 21.d0,&
& 7.d0, 3.d0, 3.d0], [4, 2])
real(rk) :: b(2, 5) = reshape([1.d0, 47.d0, 3.d0, 53.d0, 21.d0,&
& 7.d0, 3.d0, 3.d0, 3.2d0, 8.d0], [2, 5])

x = var(a)
y = var(b)
z = matmul(x, y)
do j = 1, 5
   do i = 1, 4
      call z(i, j)%grad()
      ! ...
      call set_zero_all_adj()  ! reset all adjionts to zero
   end do
end do
```


### ODE sensitivity

`Fazang` also supports ordinary differential equation sensitivity without explicitly asking for Jacobian. For that the user-defined ODE must include two RHS definitions: one with `var` parameters, and one `real` parameters.

```fortran
module ode_mod
  use fazang
  use, intrinsic :: iso_c_binding
  implicit none

  real(rk), parameter :: omega = 0.5d0
  real(rk), parameter :: d1 = 1.0d0
  real(rk), parameter :: d2 = 1.0d0

contains
  ! right-hand-side for data input
  subroutine eval_rhs(t, y, fy)
    implicit none
    real(c_double), intent(in) :: t, y(:)
    real(c_double), intent(inout) :: fy(size(y))
    fy(1) = y(2)
    fy(2) = sin(omega * d1 * d2 * t)
  end subroutine eval_rhs

  ! right-hand-side for var input with parameters
  ! y, p, and output fy must all be of var type
  subroutine eval_rhs_pvar(t, y, fy, p)
    implicit none
    real(c_double), intent(in) :: t
    type(var), intent(in) :: y(:), p(:)
    type(var), intent(inout) :: fy(size(y))
    fy(1) = y(2)
    fy(2) = sin(p(1) * p(2) * p(3) * t)
  end subroutine eval_rhs_pvar
end module ode_mod
```

Now we can solve the defined ODE.

```fortran
program cvodes_demo
  use ode_mod
  use fazang
  implicit none

  type(var) :: yt(2, 3)
  type(cvodes_tol) :: tol
  real(rk), parameter :: ts(3) = [1.2d0, 2.4d0, 4.8d0]
  real(rk), parameter :: y00(2) = [0.2d0, 0.8d0]
  type(var) :: param(3)
  real(rk) :: y0(2), ga(2)
  integer :: i, j

  y0 = y00                      ! init condition
  param = var([omega, d1, d2])  ! parameters
  tol = cvodes_tol(CV_BDF, 1.d-10, 1.d-10, 1000_8)

  yt = cvodes_sol(0.d0, y0, ts, param, eval_rhs,&
       & eval_rhs_pvar, tol)
! ...
end program cvodes_demo
```

Note that now the call to the solver function `cvodes_sol` includes argument `param` as the sensitivity parameters, as well as *two* RHS functions. After solution the sensitivities are obtained the same way by calling `grad` and `adj` functions.

```fortran
call yt(1, 1) % grad()
write(*, *) "dy_1/ d_omega at time ts(1):", param(1)%adj()
```


## Planned

-   More function and matrices operations
-   DAE solver support
-   Contiguous memory model for large arrays


## Name

The library is named after ancient Chinese philosopher [Fazang](https://en.wikipedia.org/wiki/Fazang) (法藏), who views the cosmos "as an infinite number of interdependent and interpenetrating parts" (一法为因，万法为果；万法为因，一法为果).
