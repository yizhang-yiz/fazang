#include "assert_inc.f90"

program fz_fvar_op1_test
  use fz_env
  use fz_fvar
  use fz_fvari, only: visize, v_visize

  implicit none
  real(rk), parameter :: tol = 1.d-15

  type(fvar) :: a, b, c, d
  integer(ik) :: i, j
  integer(ik) :: k

  a = 0.5d0
  b = exp(a)
  d = 0.5d0
  c = sin(b)
  call deriv(c)
  ASSERT_TOL( val(c), dsin(dexp(val(a))), tol )
  ASSERT_TOL( adj(a), -0.1283465274185981d0, tol )
  ASSERT(a%i == 1)
  ASSERT( (chains(b%i)%i - 1) == visize )
  ASSERT( (chains(d%i)%i - 1) == visize + v_visize )
  ASSERT( (chains(c%i)%i - 1) == visize + visize + v_visize )

  d = log(cos(c))
  ASSERT( chains(d%i)%i == chains(c%i)%i + v_visize + v_visize )
  call reset_deriv()
  ASSERT_TOL( adj(d), 0.d0, tol )
  ASSERT_TOL( adj(c), 0.d0, tol )
  ASSERT_TOL( adj(a), 0.d0, tol )

  call deriv(d)
  ASSERT_TOL( val(d), dlog(dcos(val(c))), tol )
  ASSERT_TOL( adj(a), 0.198559967222446865d0, tol )

  a = 0.8d0
  b = -a
  call deriv(b)
  ASSERT_TOL( val(b), -8.d-1, tol )
  ASSERT_TOL( adj(b), 1.d0, tol )
  ASSERT_TOL( adj(a), -1.d0, tol )

  a = 0.8d0
  b = +a
  call deriv(b)
  ASSERT_TOL( adj(a), 1.d0, tol )

  ! a = 0.8d0
  ! c = logit(a)
  ! call reset_adj()
  ! call grad(c)
  ! ASSERT_TOL( val(c), log(0.8d0/0.2d0), tol )
  ! ASSERT_TOL( adj(a), 1.d0/(0.8d0 - 0.64d0), 1.d-14 )

  ! a = 0.8d0
  ! c = inv_logit(a)
  ! call reset_adj()
  ! call grad(c)
  ! ASSERT_TOL( val(c), 1.d0/(1.d0 + exp(-0.8d0)), tol )
  ! ASSERT_TOL( adj(a), val(c)*(1.d0 - val(c)), tol )

end program fz_fvar_op1_test
