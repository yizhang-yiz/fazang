! user defined ODE
module ode_mod
  use fazang
  use, intrinsic :: iso_c_binding
  implicit none

  real(rk), parameter :: params(2) = [0.2d0, 0.1d0]

contains
  ! user defined right-hand-side
  subroutine eval_rhs(t, y, fy)
    real(c_double), intent(in) :: t, y(:)
    real(c_double), intent(inout) :: fy(size(y))
    fy(1) = y(2)
    fy(2) = t * y(1) * sum(params)
  end subroutine eval_rhs
end module ode_mod

program cvodes_solve_data
  use ode_mod                   ! import ODE
  use fazang                    ! import Fazang

  implicit none

  real(rk) :: yt(2, 3)          ! output array
  real(rk) :: y0(2)             ! initial condition
  type(cvodes_tol) :: tol       ! basic solver control
  
  ! use BDF method with given relative tolerance, absolute tolerance,
  ! and max number of steps between outputs
  tol = cvodes_tol(CV_BDF, 1.d-10, 1.d-10, 1000_8)

  ! initial condition
  y0 = [1.2d0, 1.8d0]

  ! solve the ODE with initial time 0.d0 and
  ! output time 1.d0, 2.d0, 3.d0
  yt = cvodes_sol(0.d0, y0, [1.d0, 2.d0, 3.d0], eval_rhs, tol)

end program cvodes_solve_data
