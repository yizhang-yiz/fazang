# Fazang

`Fazang` is a Fortran library for reverse-mode automatic differentiation, inspired by [Stan/Math library](https://mc-stan.org/users/interfaces/math).


## Quick start

`Fazang` provides user-facing variable `var` type. It is the type for dependent and independent variables of which derivatives will be calculated.

```f90
module benchmark_rosenbrock2
  use fazang
  implicit none

contains

    ! N-dimensional Rosenbrock function
    function rosenbrock(x) result(f)
      implicit none
        type(var), intent(in) :: x(:)
        type(var) :: f
        integer :: i, n
        n  = size(x)

        f = 0.d0
        do i = 1, n-1
           f = f + 100.d0*(x(i+1)-x(i)**2.d0)**2.d0 + (1.d0-x(i))**2.d0
        enddo

    end function rosenbrock

end module benchmark_rosenbrock2

program autodiff_benchmark
  use fazang
  use benchmark_rosenbrock2
  implicit none

  integer, parameter:: n_dims=1000, iterations=1000
  integer :: i, iter

  ! Timing variables
  integer(int64) :: count_start, count_end, count_rate
  real(rk) :: elapsed, total_time

  type(var) :: x(n_dims), f

  total_time = 0.0_rk

  ! Get the clock resolution for high-precision timing
  call system_clock(count_rate=count_rate)

  do iter = 1, iterations
     ! 1. Initialize independent variables
     do i = 1, n_dims
        x(i) = 1.0_rk + 0.1_rk * real(i - 1, rk)
     end do

     call system_clock(count=count_start)

     ! 2. Forward pass: Build the expression graph
     f = rosenbrock(x)

     ! 3. Reverse pass: Propagate the adjoints (chain rule)
     call grad(f)

     call system_clock(count=count_end)

     elapsed = real(count_end - count_start, rk) / real(count_rate, rk)
     total_time = total_time + elapsed

     ! 4. (Optional) Extract gradients
     if (.false.) write(*, *) "d(f)/dx(1): ", adj(x(1))

     ! 5. clean the AD tape
     call reboot_chain()
  end do

  print "(A, F10.6, A)", "Total Time: ", total_time, " s"
  print "(A, F10.6, A)", "Average Time per Iteration: ", (total_time / real(iterations, rk)) * 1000.0_rk, " ms"

end program autodiff_benchmark
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


### Hessian

`Fazang` provides a "front-over-reverse" approach to calculate Hessians, through another derived type `fvar`. The work flow is similar to `var`, but one must load module `fz_fvar` instead of `fazang`. First use `init_deriv` to initialize the variable with respect to which that the 2nd-order derivate will be calculated, then use `deriv` (counterpart of `grad` ) to calculate 1st- and 2nd-order derivatives. The corresponding function to retrieve hessian is `adj_dv`.

```f90
use fz_fvar
! ...
call init_deriv(b)
! ...
call deriv(f)
write(*,*) adj_dv(a)
```

calculates `d(df/da)/db`. See the following example (`htest.f90`) for details:

```f90
module hessian_func_example
  use fz_fvar
  implicit none
contains
  type(fvar) function f1(x, y)
    implicit none
    type(fvar), intent(in) :: x, y
    f1 = 2.d0*x*x + 3.d0*x*y + 5.d0 *y*y
  end function f1

end module hessian_func_example

program hessian_example
  use fz_fvar
  use hessian_func_example
  implicit none

  real(rk), parameter :: tol = 1.d-15
  type(fvar) :: a, b, c, d, p, q
  real(rk), parameter :: a0=0.6d0, b0=4.38d0
  real(rk) :: v0(2), v(2), s(2)

  a=a0; b=b0
  call init_deriv(a) ! fvar with respect to which the hessian will be taken
  c = f1(a, b)
  call deriv(c)       ! calc both adjoint (gradients) and the hessian
  write(*, *) "d^2c/(dada) ", adj_dv(a)
  write(*, *) "d^2c/(dbda) ", adj_dv(b)

  call reset_from(c) ! reset chain
  call init_deriv(b)
  c = f1(a, b)
  call deriv(c)
  write(*, *) "d^2c/(dadb) ", adj_dv(a)
  write(*, *) "d^2c/(dbdb) ", adj_dv(b)

end program hessian_example
```


## Nested AD

Sometimes we want to evaluate AD for only a few variables but not others. `Fazang` provde

```f90
call begin_nest()
! ...
call end_nest()
```

pair so that AD evaluations within do not affect variables outside. This is best used with Fortran's `block` construct:

```f90
type(var) :: a, b, c
a = 3.d0
b = 5.d0
c = b/a

block
  type(var) :: a, b, c
  call begin_nest()
  a = 5.d0
  b = 27.d0
  c= a*b
  ! inner a, b, c derivatives
  call grad(c)
  ! ...
  call end_nest()
end block

! outer a, b, c derivaties
call grad(c)
! ...
```

Just like `block`, `begin_nest()` / `end_nest()` pair can be nested.


## Name

The library is named after ancient Chinese philosopher [Fazang](https://en.wikipedia.org/wiki/Fazang) (法藏), who views the cosmos "as an infinite number of interdependent and interpenetrating parts" (一法为因，万法为果；万法为因，一法为果).
