#include "assert_inc.f90"

program fz_op1_test
  use fz_env
  use fz_var
  use fz_vari, only: visize, iksize, rksize

  implicit none
  real(rk), parameter :: tol = 1.d-12

  type(var) :: a, b, c, d
  integer(ik) :: i, j
  integer(ik) :: k

  a = 0.5d0
  b = exp(a)
  d = 0.5d0
  c = sin(b)
  call grad(c)
  ASSERT_TOL( val(c), dsin(dexp(val(a))), tol )
  ASSERT_TOL( adj(a), -0.1283465274185981d0, tol )
  ASSERT(a%i == 1)
  ASSERT( (b%i - 1) == visize )
  ASSERT( (d%i - 1) == visize + visize + iksize )
  ASSERT( (c%i - 1) == visize + visize + iksize + visize )
  ASSERT( core_adstack%j_ == c%i )

  d = log(cos(c))
  ASSERT( d%i == c%i + visize + iksize + visize + iksize )
  ASSERT( core_adstack%i_ == d%i + visize + iksize )
  call reset()
  ASSERT_TOL( adj(d), 0.d0, tol )
  ASSERT_TOL( adj(c), 0.d0, tol )
  ASSERT_TOL( adj(a), 0.d0, tol )

  call grad(d)
  ASSERT_TOL( val(d), dlog(dcos(val(c))), tol )
  ASSERT_TOL( adj(a), 0.198559967222446865d0, tol )

  a = 0.8d0
  b = -a
  call grad(b)
  ASSERT_TOL( val(b), -8.d-1, tol )
  ASSERT_TOL( adj(b), 1.d0, tol )
  ASSERT_TOL( adj(a), -1.d0, tol )

  a = 0.8d0
  b = +a
  call grad(b)
  ASSERT_TOL( adj(a), 1.d0, tol )
end program fz_op1_test
