module env_mod
  use, intrinsic :: iso_fortran_env

  implicit none

  integer, parameter :: adstack_len = 1024

  ! default real KIND
  integer, parameter :: rk = real64

  ! default int KIND
  integer, parameter :: ik = int32

end module env_mod
