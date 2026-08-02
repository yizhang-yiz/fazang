#include "assert_inc.f90"

program fz_dual_test
  use fz_env
  use fz_fvari

  implicit none
  real(rk), parameter :: tol = 1.d-15
  real(rk) :: v

  type(dual) :: a, b, c, d

  a = 0.5d0
  b = exp(a)
  d = 0.5d0
  c = sin(b)
  ASSERT_TOL( c%v, dsin(dexp(a%v)), tol )
  ASSERT_TOL( c%dv, -0.1283465274185981d0, tol )

  d = log(cos(c))
  ASSERT_TOL( d%v, dlog(dcos(c%v)), tol )
  ASSERT_TOL( d%dv, 0.198559967222446865d0, tol )

  a = 0.8d0
  b = -a
  ASSERT_TOL( b%v, -8.d-1, tol )
  ASSERT_TOL( b%dv, -1.d0, tol )
  ASSERT_TOL( a%dv,  1.d0, tol )

  v = 0.8d0
  a = v
  b = sin(a); ASSERT_TOL( b%dv, cos(v), tol )
  b = cos(a); ASSERT_TOL( b%dv, -sin(v), tol )
  b = tan(a); ASSERT_TOL( b%dv, 1.d0/(cos(v)*cos(v)), tol )
  b = sqrt(a); ASSERT_TOL( b%dv, 0.5d0/sqrt(v), tol )
  b = atan(a); ASSERT_TOL( b%dv, 1.d0/(1.d0+v*v), tol )
  b = asin(a); ASSERT_TOL( b%dv, 1.d0/sqrt(1.d0-v*v), tol )
  b = acos(a); ASSERT_TOL( b%dv, -1.d0/sqrt(1.d0-v*v), tol )

end program fz_dual_test
