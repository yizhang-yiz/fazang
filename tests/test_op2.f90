#include "assert_inc.f90"

program fz_op2_test
  use fz_env
  use fz_var
  use fz_vari, only: visize, iksize, rksize

  implicit none

  real(rk), parameter :: tol = 1.d-15
  type(var) :: a, b, c, d, p, q
  integer(ik) :: i, j
  integer(ik) :: k

  a = 0.5d0
  b = exp(a)
  d = 0.5d0
  c = b / a
  call grad(c)
  ASSERT_TOL( val(c), dexp(val(a))/val(a), tol )
  ASSERT_TOL( val(c), val(b)/val(a), tol )
  ASSERT_TOL( adj(a), -3.297442541400256293697d0, tol )
  ASSERT_TOL( adj(b), 2.d0, tol )
  ASSERT_TOL( adj(d), 0.d0, tol )
  ASSERT( c%p%i + visize + iksize + iksize == core_adstack%i_ )
  ASSERT( c%p%i  == core_adstack%j_ )

  call reset()
  d = a
  ASSERT( a%p%i  == 1 )
  ASSERT( d%p%i  == 1 )
  i = core_adstack%i_
  a = b/2.d0 + a
  ASSERT( a%p%i  == i + visize + iksize + rksize )
  ASSERT( a%p%i + visize + 2*iksize  == core_adstack%i_ )
  call grad(a)
  ASSERT_TOL( adj(b), 0.5d0, tol )
  ASSERT_TOL( adj(c), 0.0d0, tol )
  ASSERT_TOL( adj(d), 1.824360635350064073d0, tol)

  call reset()
  a = 0.5d0
  p = a
  b = exp(a)
  d = 2.5d0
  c = b / a
  a = b/2.d0 + atan(d) - c * a
  ASSERT_TOL( VAL(a), exp(0.5d0)/2.d0 + atan(2.5d0) - exp(0.5d0), tol)
  call grad(a)
  ASSERT_TOL( adj(p), -0.5d0*exp(0.5d0), tol )
  ASSERT_TOL( adj(b), -0.5d0, tol )
  ASSERT_TOL( adj(d), 0.137931034482758620d0, tol )
  ASSERT_TOL( adj(a), 1.d0, tol )
  ASSERT_TOL( adj(c), -0.5d0, tol )

end program fz_op2_test
