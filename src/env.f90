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

  byte, target :: core_adstack(adsize) = 0

contains

  ! first 4 bytes of stack is stack pointer
  integer(int32) function adstack_index(stack)
    byte, intent(in) :: stack(:)
    adstack_index = transfer(stack(1:4), 0_int32)
  end function adstack_index

  ! first 4 bytes of stack is stack pointer
  subroutine set_adstack_index(stack, i)
    byte, intent(inout) :: stack(:)
    integer(int32), intent(in) :: i
    stack(1:4) = transfer(i, stack(1), 4)
  end subroutine set_adstack_index

  ! first 4 bytes of stack is stack pointer
  subroutine init_adstack(stack)
    byte, intent(inout) :: stack(:)
    call set_adstack_index(stack, 5)
  end subroutine init_adstack

  ! move according to inserted object size
  subroutine incr_adstack(stack, len)
    byte, intent(inout) :: stack(:)
    integer(int32), intent(in) :: len
    integer(int32) :: i
    i = adstack_index(stack)
    call set_adstack_index(stack, i + len)
  end subroutine incr_adstack

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
