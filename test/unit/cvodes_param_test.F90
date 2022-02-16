#include "test_macro.fi"

module task_mod
  use fazang
  use, intrinsic :: iso_c_binding
  implicit none

  real(rk), parameter :: omega = 0.5d0
  real(rk), parameter :: d1 = 1.0d0
  real(rk), parameter :: d2 = 1.0d0
  real(rk), parameter :: a = omega * d1 * d2

contains
  subroutine eval_rhs(t, y, fy)
    implicit none
    real(c_double), intent(in) :: t, y(:)
    real(c_double), intent(inout) :: fy(size(y))
    fy(1) = y(2)
    fy(2) = sin(omega * d1 * d2 * t)
  end subroutine eval_rhs

  subroutine eval_rhs_pvar(t, y, fy, p)
    implicit none
    real(c_double), intent(in) :: t
    type(var), intent(in) :: y(:), p(:)
    type(var), intent(inout) :: fy(size(y))
    fy(1) = y(2)
    fy(2) = sin(p(1) * p(2) * p(3) * t)
  end subroutine eval_rhs_pvar

  function sol(y0, t) result(s)
    implicit none
    real(rk), intent(in) :: t, y0(2)
    real(rk) :: s(2)
    s(1) = -sin(a * t)/a/a + (y0(2) + 1.d0/a)*t + y0(1)
    s(2) = -cos(a * t)/a + (y0(2) + 1.d0/a)
  end function sol

  function grad_a(t) result(s)
    implicit none
    real(rk), intent(in) :: t
    real(rk) :: s(2)
    s(1) = (2.d0 * a**(-3) * sin(a * t) - t * cos(a * t)/a**2 -  t/ a**2)
    s(2) = cos(a * t)/a**2 + sin(a * t) * t/a - 1.d0/a**2
  end function grad_a

end module task_mod

program cvodes_test
  use, intrinsic :: iso_c_binding
  use task_mod
  use fazang_test_mod
  use fazang_env_mod
  use fazang_cvodes_mod
  use fazang_grad_mod
  use fazang_cvodes_options_mod

  use fsundials_nvector_mod
  use fsundials_context_mod         ! Fortran interface to SUNContext
  use fnvector_serial_mod           ! Fortran interface to serial N_Vector
  implicit none

  type(var) :: yt(2, 3)
  type(cvodes_tol) :: tol
  real(rk), parameter :: ts(3) = [1.2d0, 2.4d0, 4.8d0], y00(2) = [0.2d0, 0.8d0]
  type(var) :: param(3)
  real(rk) :: y0(2), ga(2)
  integer :: i, j

  y0 = y00
  param = var([omega, d1, d2])
  tol = cvodes_tol(CV_BDF, 1.d-10, 1.d-10, 1000_8)

  yt = cvodes_sol(0.d0, y0, ts, param, eval_rhs,&
       & eval_rhs_pvar, tol)

  EXPECT_FLOAT_EQ(yt(:, 1)%val(), (sol(y00, ts(1))))
  EXPECT_FLOAT_EQ(yt(:, 2)%val(), (sol(y00, ts(2))))
  EXPECT_FLOAT_EQ(yt(:, 3)%val(), (sol(y00, ts(3))))

  do i = 1, 3
     ga = grad_a(ts(i))
     do j = 1, 2
        call set_zero_all_adj()
        call yt(j, i)%grad()
        EXPECT_FLOAT_EQ(param(1)%adj(), (d1 * d2 * ga(j)))
        EXPECT_FLOAT_EQ(param(2)%adj(), (d1 * omega * ga(j)))
        EXPECT_FLOAT_EQ(param(3)%adj(), (d2 * omega * ga(j)))
     end do
  enddo

end program cvodes_test
