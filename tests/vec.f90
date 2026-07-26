#include "assert_inc.f90"

program fz_vec_test
  use fz_env
  use fz_var
  use fz_vari, only: visize, iksize, rksize

  implicit none
  real(rk), parameter :: tol = 1.d-12

  type(var) :: a, b, c
  type(var) :: d(3)
  real(rk) :: d_adj(3)

  ASSERT(all(val(d) == 0.d0))

  a = 0.5d0
  b = 0.6d0
  d(1) = a
  d(2) = b
  d(3) = a * b
  c = d(3)
  ASSERT_TOL(val(d(3)), 0.3d0, tol)

  call grad(c)
  d_adj = adj(d)
  ASSERT_TOL(d_adj(1), 0.6d0, tol)
  ASSERT_TOL(d_adj(2), 0.5d0, tol)
  ASSERT_TOL(d_adj(3), 1.0d0, tol)

end program fz_vec_test
