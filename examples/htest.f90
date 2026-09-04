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
