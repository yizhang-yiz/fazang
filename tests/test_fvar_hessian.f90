#include "assert_inc.f90"

module hessian_test_func
  use fz_env
  use fz_fvar
  implicit none
contains
  type(fvar) function f1(x, y)
    implicit none
    type(fvar), intent(in) :: x, y
    f1 = 2.d0*x*x + 3.d0*x*y + 5.d0 *y*y
  end function f1

  type(fvar) function f1v(x)
    implicit none
    type(fvar), intent(in) :: x(:)
    f1v = f1(x(1), x(2))
  end function f1v

  type(fvar) function f2(x, y)
    implicit none
    type(fvar), intent(in) :: x, y
    f2 = x*x*x + x*y*y + 2.d0*x*x - y
  end function f2

  type(fvar) function f2v(x)
    implicit none
    type(fvar), intent(in) :: x(:)
    f2v = f2(x(1), x(2))
  end function f2v

end module hessian_test_func

program fz_fvar_hessian
  use fz_env
  use fz_fvar
  use hessian_test_func
  implicit none

  real(rk), parameter :: tol = 1.d-15
  type(fvar) :: a, b, c, d, p(2), q
  real(rk), parameter :: a0=0.6d0, b0=4.38d0
  real(rk) :: v0(2), v(2), s(2)

  a=a0; b=b0
  call init_deriv(a)
  c = f1(a, b)
  call deriv(c)
  ASSERT_TOL( adj_dv(a), 4.d0, tol) ! d(df1/da)/da
  ASSERT_TOL( adj_dv(b), 3.d0, tol) ! d(df1/db)/da
  call reset_deriv(c)
  call init_deriv(b)
  c = f1(a, b)
  call deriv(c)
  ASSERT_TOL( adj_dv(a), 3.d0, tol) ! d(df1/da)/db
  ASSERT_TOL( adj_dv(b), 10.d0, tol) ! d(df1/db)/db

  call reset_deriv()
  call init_deriv(a)
  c = f2(a, b)
  call deriv(c)
  ASSERT_TOL( adj_dv(a), 6*a0+4, tol)
  ASSERT_TOL( adj_dv(b), 2*b0, tol)
  call reset_deriv(c)
  call init_deriv(b)
  c = f2(a, b)
  call deriv(c)
  ASSERT_TOL( adj_dv(a), 2*b0, tol)
  ASSERT_TOL( adj_dv(b), 2*a0, tol)

  call reboot_chain()
  a=a0; b=b0
  v0=(/a0, b0/)
  v=(/1.2d0, 2.2d0/)
  s = hvp(f1v, v0, v)
  ASSERT_TOL( s(1), 4*v(1)+3*v(2), tol)
  ASSERT_TOL( s(2), 3*v(1)+10.0*v(2), tol)

  call reboot_chain()
  a=a0; b=b0
  v0=(/a0, b0/)
  v=(/1.2d0, 2.2d0/)
  s = hvp(f2v, v0, v)
  ASSERT_TOL( s(1), (6*a0+4)*v(1)+2*b0*v(2), 1.e-14)
  ASSERT_TOL( s(2), 2*b0*v(1)+2*a0*v(2), tol)

end program fz_fvar_hessian
