#include "assert_inc.f90"

program fz_op1_test
  use fz_env
  use fz_var
  use fz_vari, only: visize, iksize, rksize

  implicit none

  type(var) :: a, b, c, d
  integer(ik) :: i, j
  integer(ik) :: k

  a = 0.5d0
  b = exp(a)
  d = 0.5d0
  c = sin(b)
  call grad(c)
  ASSERT_TOL( val(c), dsin(dexp(val(a))), 1.d-12 )
  ASSERT_TOL( adj(a), -0.1283465274185981d0, 1.d-12 )
  ASSERT(a%i == 1)
  ASSERT( (b%i - 1) == visize )
  ASSERT( (d%i - 1) == visize + visize + iksize )
  ASSERT( (c%i - 1) == visize + visize + iksize + visize )
  ASSERT( core_adstack%j_ == c%i )

  d = log(cos(c))
  ASSERT( d%i == c%i + visize + iksize + visize + iksize )
  ASSERT( core_adstack%i_ == d%i + visize + iksize )
  call reset()
  ASSERT_TOL( adj(d), 0.d0, 1.d-12 )
  ASSERT_TOL( adj(c), 0.d0, 1.d-12 )
  ASSERT_TOL( adj(a), 0.d0, 1.d-12 )

  call grad(d)
  ASSERT_TOL( val(d), dlog(dcos(val(c))), 1.d-12 )
  ASSERT_TOL( adj(a), 0.198559967222446865d0, 1.d-12 )

end program fz_op1_test
