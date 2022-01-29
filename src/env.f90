module env_mod
  use, intrinsic :: iso_fortran_env

  implicit none

  integer, parameter :: adstack_len = 1024

  ! default real KIND
  integer, parameter :: rk = real64

  ! default int KIND
  integer, parameter :: ik = int32

contains

  elemental subroutine incr(var, inc)
    integer,intent(inout) :: var
    integer,intent(in)    :: inc
    var = var + inc
  end subroutine incr

  elemental subroutine incr1(var)
    integer,intent(inout) :: var
    var = var + 1
  end subroutine incr1

  elemental subroutine incr2(var)
    integer,intent(inout) :: var
    var = var + 2
  end subroutine incr2

  elemental subroutine incr4(var)
    integer,intent(inout) :: var
    var = var + 4
  end subroutine incr4


end module env_mod
