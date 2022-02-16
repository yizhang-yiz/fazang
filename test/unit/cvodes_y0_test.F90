#include "test_macro.fi"

module osci_ode_mod
  use fazang_env_mod
  use fazang_var_mod
  use, intrinsic :: iso_c_binding
  implicit none

  ! type(var) :: params(2)
  real(rk), parameter :: omega = 0.5d0

contains
  subroutine eval_rhs(t, y, fy)
    real(c_double), intent(in) :: t, y(:)
    real(c_double), intent(inout) :: fy(size(y))
    fy(1) = y(2)
    fy(2) = sin(omega * t)
  end subroutine eval_rhs

  subroutine eval_rhs_yvar(t, y, fy)
    real(c_double), intent(in) :: t
    type(var), intent(in) :: y(:)
    type(var), intent(inout) :: fy(size(y))
    fy(1) = y(2)
    fy(2) = sin(omega * t)
  end subroutine eval_rhs_yvar

  function sol(y0, t) result(s)
    real(rk), intent(in) :: y0(2), t
    real(rk) :: s(2)    
    s(1) = -sin(omega * t)/omega/omega + (y0(2) + 1.d0/omega)*t + y0(1)
    s(2) = -cos(omega * t)/omega + (y0(2) + 1.d0/omega)
  end function sol

end module osci_ode_mod

program cvodes_solve_data_test
  use, intrinsic :: iso_c_binding
  use osci_ode_mod
  use fazang_test_mod
  use fazang_env_mod
  use fazang_cvodes_mod
  use fazang_grad_mod
  use fazang_vari_mod

  use fsundials_nvector_mod
  use fsundials_context_mod         ! Fortran interface to SUNContext
  use fnvector_serial_mod           ! Fortran interface to serial N_Vector
  implicit none

  type(var) :: yt(2, 3), y0(2)
  type(cvodes_tol) :: tol
  real(rk), parameter :: y0val(2) = [0.2d0, 0.8d0]
  real(rk), parameter :: ts(3) = [1.2d0, 2.4d0, 4.8d0]
  
  tol = cvodes_tol(1.d-10, 1.d-10, 1000_8)

  y0 = var(y0val)
  yt = cvodes_bdf_y0_sens(0.d0, y0, ts, eval_rhs,&
       & eval_rhs_yvar, tol)

  EXPECT_EQ(size(yt), 6)

  EXPECT_FLOAT_EQ(yt(:, 1)%val(), (sol(y0val, ts(1))))
  EXPECT_FLOAT_EQ(yt(:, 2)%val(), (sol(y0val, ts(2))))
  EXPECT_FLOAT_EQ(yt(:, 3)%val(), (sol(y0val, ts(3))))

  call set_zero_all_adj()
  call yt(1, 1)%grad()
  EXPECT_DBL_EQ(y0%adj(), ([1.d0, ts(1)]))
  call set_zero_all_adj()
  call yt(2, 1)%grad()
  EXPECT_DBL_EQ(y0%adj(), ([0.d0, 1.d0]))
  call set_zero_all_adj()
  call yt(1, 2)%grad()
  EXPECT_DBL_EQ(y0%adj(), ([1.d0, ts(2)]))
  call set_zero_all_adj()
  call yt(2, 2)%grad()
  EXPECT_DBL_EQ(y0%adj(), ([0.d0, 1.d0]))
  call set_zero_all_adj()
  call yt(1, 3)%grad()
  EXPECT_DBL_EQ(y0%adj(), ([1.d0, ts(3)]))
  call set_zero_all_adj()
  call yt(2, 3)%grad()
  EXPECT_DBL_EQ(y0%adj(), ([0.d0, 1.d0]))

end program cvodes_solve_data_test
