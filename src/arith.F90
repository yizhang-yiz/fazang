module fazang_arith_mod
  use, intrinsic :: iso_fortran_env
  use fazang_env_mod
  use fazang_var_mod, only : var
  
  implicit none

  ! NaN.
  ! see https://stackoverflow.com/questions/31971836/having-parameter-constant-variable-with-nan-value-in-fortran
  real(real64), parameter :: nan64 =  transfer(-2251799813685248_int64, 1._real64)

  ! PI
  real(rk), parameter :: pi = 4.d0*atan(1.d0)

  ! 2PI
  real(rk), parameter :: two_pi = 2.d0 * pi

  ! sqrt(PI)
  real(rk), parameter :: sqrt_pi = sqrt(pi)

  ! 0.5 sqrt(PI)
  real(rk), parameter :: half_sqrt_pi = 0.5d0 * sqrt(pi)

  ! 2/sqrt(PI)
  real(rk), parameter :: two_over_sqrt_pi = 2.d0 / sqrt(pi)

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

  ! log(sqrt(2pi))
  real(rk), parameter :: log_sqrt_two_pi = 0.5d0 * log_two_pi

  ! log(1/sqrt(2pi))
  real(rk), parameter :: neg_log_sqrt_two_pi = -log_sqrt_two_pi

  ! log(10)
  real(rk), parameter :: log_ten = log(10.d0)

  ! sqrt(2)
  real(rk), parameter :: sqrt_two = sqrt(2.d0)

  interface is_nan
     module procedure :: is_nan_d
     module procedure :: is_nan_v
  end interface is_nan

contains
  elemental logical function is_inf(d)
    real(rk), intent(in) :: d
    is_inf = d > huge(d)
  end function is_inf

  elemental logical function is_neg_inf(d)
    real(rk), intent(in) :: d
    is_neg_inf = d < -huge(d)
  end function is_neg_inf

  elemental logical function is_nan_d(d)
    real(rk), intent(in) :: d
    is_nan_d = d /= d
  end function is_nan_d

  elemental logical function is_nan_v(v)
    type(var), intent(in) :: v
    is_nan_v = is_nan_d(v%val())
  end function is_nan_v

end module fazang_arith_mod
