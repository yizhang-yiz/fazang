#include "test_macro.fi"

module ode_mod
  use fazang_env_mod
  use fazang_var_mod
  use, intrinsic :: iso_c_binding
  implicit none

  type(var) :: params(2)

contains
  subroutine eval_rhs(t, y, fy)
    real(c_double), intent(in) :: t, y(:)
    real(c_double), intent(inout) :: fy(size(y))
    fy(1) = y(2)
    fy(2) = t * y(1) * sum(params%val())
  end subroutine eval_rhs

end module ode_mod

program cvodes_solve_data_test
  use, intrinsic :: iso_c_binding
  use ode_mod
  use fazang_test_mod
  use fazang_env_mod
  use fazang_cvodes_mod

  use fsundials_nvector_mod
  use fsundials_context_mod         ! Fortran interface to SUNContext
  use fnvector_serial_mod           ! Fortran interface to serial N_Vector
  implicit none

  real(c_double) :: yt(2, 3), y0(2)
  type(cvodes_tol) :: tol
  
  tol = cvodes_tol(CV_BDF, 1.d-10, 1.d-10, 1000_8)

  y0 = [1.2d0, 1.8d0]
  params = var([0.2d0, 0.1d0])
  yt = cvodes_sol(0.d0, y0, [1.d0, 2.d0, 3.d0], eval_rhs, tol)

  EXPECT_EQ(size(yt), 6)

  ! compare with lsode solution with 3e5 steps in [0,3]
  EXPECT_NEAR(yt(1, 1), 3.105910d0, 2.d-5)
  EXPECT_NEAR(yt(2, 1), 2.165877d0, 1.d-5)
  EXPECT_NEAR(yt(1, 2), 6.081948d0, 2.d-5)
  EXPECT_NEAR(yt(2, 2), 4.230688d0, 2.d-5)
  EXPECT_NEAR(yt(1, 3), 13.124382d0, 1.d-5)
  EXPECT_NEAR(yt(2, 3), 11.181090d0, 1.d-5)

end program cvodes_solve_data_test
