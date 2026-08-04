# Fazang

`Fazang` is a Fortran library for reverse-mode automatic differentiation, inspired by [Stan/Math library](https://mc-stan.org/users/interfaces/math).


## Quick start

`Fazang` provides user-facing variable `var` type. It is the type for dependent and independent variables of which derivatives will be calculated.

```f90
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
```


## Use `Fazang`


### Build

`Fazang` uses `cmake` to build. The `CMakeLists.txt` file has a target `ftest` that compiles the above example. User may replicate the steps to build new applications.

```bash
mkdir build
cd build
cmake /path/to/fazang -B. -GNinja
cmake --build .
```

The library also contains option `test` so that adding `-Dtest=ON` to the above configuration

```bash
cmake /path/to/fazang -B. -GNinja -Dtest=ON
```

enables building tests. One can then run the unit tests with

```bash
ctest
```


### Use the library

`Fazang` can be accessed by `use fazang` module.

A variable declared `var`

```fortran
type(var) x
```

can be defined as

```fortran
x = d           ! assume "d" is a real64 variable
x = 3.d0        ! using a constant
```

`Fazang` overloads instrinc arithmatic unary and binary functions.

All the downstream variables that depend on a `var` should also be `var`

```fortran
type(var) x, y
x = 1.d0
y = sin(x)
```

The value and the adjoint (derivative) of a `var` can be accessed using `val()` and `adj()` functions, respectively.

```fortran
write(*, *) val(y)   ! equals to sin(val(x))
write(*, *) adj(y)   ! equals to 0.d0 before any gradient operations
```

`Fazang`'s unary and binary functions are `elemental`, so they can be extended to arrays.

```fortran
type(var) a(3), b, c(3), d
a = ([1.d0, 2.d0, 3.d0])
b = 0.5d0
c = 2.d0 * a
d = log(b * a * exp(c))
```

To calculate a dependent variable's derivatives, call `grad()` function

```fortran
call grad(d(2))
```

and access each upstream variable's derivative through `adj()` afterwards.

```fortran
write(*, *) adj(c)    ! should be [0.0, 1.0, 0.0]
```


### User defined operators

`Fazang` provides a macro `NEW_OP` so that user can define a new `var` operator. The syntax is

```f90
NEW_OP(operation_name, value_func, jacobian_func)
```

Here `operation_name` is the name of the function to be added; `value_func` is the function that takes an assumed-shape `real(real64)` array argument. It returns a `real64` that is the value of the new function. `jacobian_func` should accept the same argument of `value_func` and returns an real array that is the gradient of the new function. See `tests/test_new_op.f90` as an example.


### Hessian

`Fazang` provides a "front-over-reverse" approach to calculate Hessians, through another derived type `fvar`. The work flow is similar to `var`, but one must first use `init_deriv` to initialize the variable with respect to which that the 2nd-order derivate will be calculated. The corresponding function to retrieve hessian is `adj_dv`. Thus

```f90
call init_deriv(b)
! ...
call grad(f)
write(*,*) adj_dv(a)
```

calculates `d(df/da)/db`. See the following example (`htest.f90`) for details:

```f90
module hessian_func_example
  use fazang
  implicit none
contains
  type(fvar) function f1(x, y)
    implicit none
    type(fvar), intent(in) :: x, y
    f1 = 2.d0*x*x + 3.d0*x*y + 5.d0 *y*y
  end function f1

end module hessian_func_example

program hessian_example
  use fazang
  use hessian_func_example
  implicit none

  real(rk), parameter :: tol = 1.d-15
  type(fvar) :: a, b, c, d, p, q
  real(rk), parameter :: a0=0.6d0, b0=4.38d0
  real(rk) :: v0(2), v(2), s(2)

  a=a0; b=b0
  call init_deriv(a) ! fvar with respect to which the hessian will be taken
  c = f1(a, b)
  call grad(c)       ! calc both adjoint (gradients) and the hessian
  write(*, *) "d^2c/(dada) ", adj_dv(a)
  write(*, *) "d^2c/(dbda) ", adj_dv(b)

  call reset_from(c) ! reset chain
  call init_deriv(b)
  c = f1(a, b)
  call grad(c)
  write(*, *) "d^2c/(dadb) ", adj_dv(a)
  write(*, *) "d^2c/(dbdb) ", adj_dv(b)

end program hessian_example
```


## Name

The library is named after ancient Chinese philosopher [Fazang](https://en.wikipedia.org/wiki/Fazang) (法藏), who views the cosmos "as an infinite number of interdependent and interpenetrating parts" (一法为因，万法为果；万法为因，一法为果).
