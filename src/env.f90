module fazang_env
  use, intrinsic :: iso_fortran_env

  implicit none

#ifdef fz_adstack_size
  integer(int32), parameter :: adsize = fz_adstack_size * (1048576)
#else                           ! default 50 MB
  integer(int32), parameter :: adsize = 50 * (1048576)
#endif

  ! real KIND
  integer, parameter :: rk = real64

  ! int KIND
  integer, parameter :: ik = int32

  type :: adstack
     byte :: s(adsize) = 0
     integer(ik) :: i = 1       ! current (vacant) location
     integer(ik) :: j = 0       ! previous (just filled) location
   contains
     procedure incr
  end type adstack

  type(adstack), target :: core_adstack

contains

  ! move according to inserted object size
  subroutine incr(stack, len)
    class(adstack), intent(inout) :: stack
    integer(int32), intent(in) :: len
    stack%j = stack%i
    stack%i = stack%i + len
  end subroutine incr

!   elemental subroutine incr(var, inc)
!     integer,intent(inout) :: var
!     integer,intent(in)    :: inc
!     var = var + inc
!   end subroutine incr

!   elemental subroutine incr1(var)
!     integer,intent(inout) :: var
!     var = var + 1
!   end subroutine incr1

!   elemental subroutine incr2(var)
!     integer,intent(inout) :: var
!     var = var + 2
!   end subroutine incr2

!   elemental subroutine incr4(var)
!     integer,intent(inout) :: var
!     var = var + 4
!   end subroutine incr4


end module fazang_env
