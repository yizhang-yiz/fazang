#include "assert_inc.f90"

program fz_vec_test
  use fz_env
  use fz_var
  use fz_vari, only: visize, iksize, rksize

  implicit none
  real(rk), parameter :: tol = 1.d-15

  type(var) :: a, b, c
  real(rk) :: dmat(3, 2)=reshape([3.d0, 5.d0, 7.d0, 2.d0, 4.d0, 6.d0], [3, 2])
  type(var) :: d(3), vmat(3, 2)
  real(rk) :: d_adj(3)

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

  call reset_adj()
  d(1) = 0.5d0
  d(2) = 0.6d0
  d(3) = 0.7d0
  a = sum(d)*2.d0
  ASSERT_TOL(val(a), 3.6d0, tol)
  call grad(a)
  d_adj = adj(d)
  ASSERT_TOL(d_adj(1), 2.0d0, tol)
  ASSERT_TOL(d_adj(2), 2.0d0, tol)
  ASSERT_TOL(d_adj(3), 2.0d0, tol)

  vmat = dmat
  ASSERT_TOL(val(vmat(1, 1)), dmat(1, 1), tol)
  ASSERT_TOL(val(vmat(2, 1)), dmat(2, 1), tol)
  ASSERT_TOL(val(vmat(3, 2)), dmat(3, 2), tol)
  vmat(1, 1) = vmat(2, 2) * vmat(3, 2)
  ASSERT_TOL(val(vmat(1, 1)), dmat(2, 2)*dmat(3, 2), tol)
  call reset_adj()
  call grad(vmat(1, 1))
  ASSERT_TOL(adj(vmat(1, 2)), 0.d0, tol)
  ASSERT_TOL(adj(vmat(2, 1)), 0.d0, tol)
  ASSERT_TOL(adj(vmat(3, 1)), 0.d0, tol)
  ASSERT_TOL(adj(vmat(2, 2)), dmat(3, 2), tol)
  ASSERT_TOL(adj(vmat(3, 2)), dmat(2, 2), tol)
  ASSERT_TOL(adj(vmat(1, 1)), 1.d0, tol)

end program fz_vec_test
