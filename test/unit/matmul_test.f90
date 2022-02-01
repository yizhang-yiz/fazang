#include "test_macro.fi"

program matmul_test
  use, intrinsic :: iso_fortran_env
  use test_mod
  use vari_mod, only : vari, adstack, callstack
  use grad_mod
  use env_mod
  use var_mod
  use matmul_mod
  use add_mod
  implicit none

  type(var) :: x(4, 2), y(2, 5), z(4, 5), q(3, 2)
  real(rk) :: a(4, 2) = reshape([1.d0, 47.d0, 3.d0, 53.d0, 21.d0,&
       & 7.d0, 3.d0, 3.d0], [4, 2])
  real(rk) :: b(2, 5) = reshape([1.d0, 47.d0, 3.d0, 53.d0, 21.d0,&
       & 7.d0, 3.d0, 3.d0, 3.2d0, 8.d0], [2, 5])
  real(rk) :: c(3, 4) = reshape([1.d0, 47.d0, 3.d0, 53.d0, 21.d0,&
       & 7.d0, 3.d0, 3.d0, 3.2d0, 8.d0, 2.d0, 42.d0], [3, 4])
  real(rk) :: d(3, 2)
  integer(ik) :: i, j

  x = var(a)
  y = var(b)
  z = matmul(x, y)
  EXPECT_EQ(callstack%head, 39)
  EXPECT_DBL_EQ(val(z), matmul(a, b))
  
  do j = 1, 4
     do i = 1, 4
        call set_zero_all_adj()
        call z(i, j)%vi%init_dependent()
        call z(i, j)%vi%chain()
        EXPECT_DBL_EQ(adj(x(i, :)), b(:, j))
        EXPECT_DBL_EQ(adj(y(:, j)), a(i, :))
     end do
  end do

  do j = 1, 5
     do i = 1, 4
        call set_zero_all_adj()
        call z(i, j)%vi%set_adj(2.5d0)
        call z(i, j)%vi%chain()
        EXPECT_DBL_EQ(adj(x(i, :)), 2.5d0 * b(:, j))
        EXPECT_DBL_EQ(adj(y(:, j)), 2.5d0 * a(i, :))
     end do
  end do

  z = matmul(x, b)
  EXPECT_DBL_EQ(val(z), matmul(a, b))
  do j = 1, 5
     do i = 1, 4
        call set_zero_all_adj()
        call z(i, j)%vi%set_adj(2.5d0)
        call z(i, j)%vi%chain()
        EXPECT_DBL_EQ(adj(x(i, :)), 2.5d0 * b(:, j))
     end do
  end do

  q = matmul(c, x)
  EXPECT_DBL_EQ(val(q), matmul(c, a))
  do j = 1, 2
     do i = 1, 3
        call set_zero_all_adj()
        call q(i, j)%grad()
        EXPECT_DBL_EQ(adj(x(:, j)), c(i, :))
     end do
  end do

  ! double version should also work
  d = matmul(c, a)

end program matmul_test
