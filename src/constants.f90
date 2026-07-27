module fz_constants
  use, intrinsic :: iso_fortran_env
  use fz_env

  implicit none

  ! smallest
  real(rk), parameter :: eps = tiny(0.0d0)

  ! log(smallest)
  real(rk), parameter :: log_eps = log(eps)
end module fz_constants
