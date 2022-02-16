module ode_mod
  use fazang
  use, intrinsic :: iso_c_binding
  implicit none

  real(rk), parameter :: omega = 0.5d0
  real(rk), parameter :: d1 = 1.0d0
  real(rk), parameter :: d2 = 1.0d0

contains
  ! right-hand-side for data input
  subroutine eval_rhs(t, y, fy)
    implicit none
    real(c_double), intent(in) :: t, y(:)
    real(c_double), intent(inout) :: fy(size(y))
    fy(1) = y(2)
    fy(2) = sin(omega * d1 * d2 * t)
  end subroutine eval_rhs

  ! right-hand-side for var input with parameters
  ! y, p, and output fy must all be of var type
  subroutine eval_rhs_pvar(t, y, fy, p)
    implicit none
    real(c_double), intent(in) :: t
    type(var), intent(in) :: y(:), p(:)
    type(var), intent(inout) :: fy(size(y))
    fy(1) = y(2)
    fy(2) = sin(p(1) * p(2) * p(3) * t)
  end subroutine eval_rhs_pvar
end module ode_mod

program cvodes_demo
  use ode_mod
  use fazang
  implicit none

  type(var) :: yt(2, 3)
  type(cvodes_tol) :: tol
  real(rk), parameter :: ts(3) = [1.2d0, 2.4d0, 4.8d0]
  real(rk), parameter :: y00(2) = [0.2d0, 0.8d0]
  type(var) :: param(3)
  real(rk) :: y0(2), ga(2)
  integer :: i, j

  y0 = y00                      ! init condition
  param = var([omega, d1, d2])  ! parameters
  tol = cvodes_tol(CV_BDF, 1.d-10, 1.d-10, 1000_8)

  yt = cvodes_sol(0.d0, y0, ts, param, eval_rhs,&
       & eval_rhs_pvar, tol)

  ! call yt(1, 1) % grad()
  ! write(*, *) "dy_1/ d_omega at time ts(1):", param(1)%adj()
end program cvodes_demo
