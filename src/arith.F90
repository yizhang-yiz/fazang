module fazang_arith_mod
  use, intrinsic :: iso_fortran_env
  use fazang_env_mod
  use fazang_var_mod, only : var
  
  implicit none

  ! PI
  real(rk), parameter :: pi = 4.d0*atan(1.d0)

  ! 2PI
  real(rk), parameter :: two_pi = 2.d0 * pi

  ! E
  real(rk), parameter :: euler_e = exp(1.0d0)

  ! smallest
  real(rk), parameter :: epsilon = tiny(0.0d0)

  ! log(smallest)
  real(rk), parameter :: log_epsilon = log(epsilon)

  ! log(2)
  real(rk), parameter :: log_pi = log(pi)

  ! log(2)
  real(rk), parameter :: log_two = log(2.d0)

  ! log(0.5)
  real(rk), parameter :: log_half = -log(2.d0)

  ! log(2pi)
  real(rk), parameter :: log_two_pi = log_two + log_pi

  ! log(10)
  real(rk), parameter :: log_ten = log(10.d0)

  ! sqrt(2)
  real(rk), parameter :: sqrt_two = sqrt(2.d0)

contains
  elemental logical function is_inf(d)
    real(rk), intent(in) :: d
    is_inf = d > huge(d)
  end function is_inf

  elemental logical function is_neg_inf(d)
    real(rk), intent(in) :: d
    is_neg_inf = d < -huge(d)
  end function is_neg_inf

  elemental logical function is_nan(d)
    real(rk), intent(in) :: d
    is_nan = d /= d
  end function is_nan


end module fazang_arith_mod
