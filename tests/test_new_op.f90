#include "assert_inc.f90"
#include "new_op_inc.f90"

module new_op_mod
  use fz_env
  implicit none

contains
  real(rk) function div_op(x)
    implicit none
    real(rk), intent(in) :: x(:)
    div_op = x(1)/x(2)
  end function div_op

  function div_jac_op(x) result(res)
    implicit none
    real(rk), intent(in) :: x(:)
    real(rk) :: res(size(x))
    res = (/1.d0/x(2), -x(1)/(x(2)*x(2)) /)
  end function div_jac_op

  real(rk) function hill_eq(x)
    real(rk), intent(in) :: x(:)
    hill_eq = x(1)**x(3)/(x(2)**x(3)+x(1)**x(3))
  end function hill_eq

  function hill_eq_jac(x) result(res)
    real(rk), intent(in) :: x(:)
    real(rk) :: res(size(x))
    res = (/ &
    x(3)*x(2)**x(3)*x(1)**(x(3)-1)/(x(1)**x(3)+x(2)**x(3))**2, &
    -x(3)*x(2)**(x(3)-1)*x(1)**x(3)/(x(1)**x(3)+x(2)**x(3))**2, &
    x(2)**x(3)*x(1)**x(3)*log(x(1)/x(2))/(x(1)**x(3)+x(2)**x(3))**2 /)
  end function hill_eq_jac

  NEW_OP(new_div, div_op, div_jac_op)

  NEW_OP(hill, hill_eq, hill_eq_jac)

end module new_op_mod

program test
  use fazang
  use new_op_mod
  implicit none
  type(var) :: a(2) ,c, d(3)
  type(var) :: av(2) ,cv, dv(3)
  real(rk) :: adj_a(2), adj_d(3)
  real(rk), parameter :: tol = 1.d-15

  a(1) = 3.d0
  a(2) = 2.d0
  d(2) = 0.7d0
  d(3) = 0.9d0
  d(1) = new_div(a)
  c = hill(d)
  call grad(c)
  adj_a = adj(a)
  adj_d = adj(d)

  call reset_adj()
  av(1) = 3.d0
  av(2) = 2.d0
  dv(2) = 0.7d0
  dv(3) = 0.9d0
  dv(1) = av(1)/av(2)

  cv = dv(1)**dv(3)/(dv(2)**dv(3)+dv(1)**dv(3))
  call grad(cv)

  ASSERT_TOL(adj_a(1), adj(av(1)), tol)
  ASSERT_TOL(adj_a(2), adj(av(2)), tol)
  ASSERT_TOL(adj_d(1), adj(dv(1)), tol)
  ASSERT_TOL(adj_d(2), adj(dv(2)), tol)
  ASSERT_TOL(adj_d(3), adj(dv(3)), tol)

end program test
