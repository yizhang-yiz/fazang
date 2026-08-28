#include "assert_inc.f90"

program fz_fvar_op2_test
  use fz_env
  use fz_fvar
  use fz_fvari, only: visize, v_visize, vv_visize, vd_visize

  implicit none

  real(rk), parameter :: tol = 1.d-15
  type(fvar) :: a, b, c, d, p, q
  integer(ik) :: i, j
  integer(ik) :: k

  a = 0.5d0
  b = exp(a)
  d = 0.5d0
  c = b / a
  call deriv(c)
  ASSERT_TOL( val(c), dexp(val(a))/val(a), tol )
  ASSERT_TOL( val(c), val(b)/val(a), tol )
  ASSERT_TOL( adj(a), -3.297442541400256293697d0, tol )
  ASSERT_TOL( adj(b), 2.d0, tol )
  ASSERT_TOL( adj(d), 0.d0, tol )
  ASSERT( chains(c%i)%i + vv_visize == core_adstack%i_ )

  call reset_all_deriv()
  d = a
  ASSERT( chains(a%i)%i  == 1 )
  ASSERT( chains(d%i)%i  == 1 )
  i = core_adstack%i_
  a = b/2.d0 + a
  ASSERT( chains(a%i)%i  == i + vd_visize )
  ASSERT( chains(a%i)%i + vv_visize  == core_adstack%i_ )
  call deriv(a)
  ASSERT_TOL( adj(b), 0.5d0, tol )
  ASSERT_TOL( adj(c), 0.0d0, tol )
  ASSERT_TOL( adj(d), 1.824360635350064073d0, tol)

  call reset_all_deriv()
  a = 0.5d0
  p = a
  b = exp(a)
  d = 2.5d0
  c = b / a
  a = b/2.d0 + atan(d) - c * a
  ASSERT_TOL( VAL(a), exp(0.5d0)/2.d0 + atan(2.5d0) - exp(0.5d0), tol)
  call deriv(a)
  ASSERT_TOL( adj(p), -0.5d0*exp(0.5d0), tol )
  ASSERT_TOL( adj(b), -0.5d0, tol )
  ASSERT_TOL( adj(d), 0.137931034482758620d0, tol )
  ASSERT_TOL( adj(a), 1.d0, tol )
  ASSERT_TOL( adj(c), -0.5d0, tol )

end program fz_fvar_op2_test
