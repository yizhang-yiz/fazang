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
     integer(int8) :: s_(adsize) = 0      ! int8 serves as byte
     integer(ik) :: i_ = 1       ! current (vacant) location
     integer(ik) :: j_ = 0       ! previous (just filled) location
   contains
     private
     procedure push_real
     procedure push_real_array
     procedure push_int
     procedure push_int_array
     procedure pop_real
     procedure pop_real_array
     procedure pop_int
     procedure pop_int_array
     procedure, public :: incr
     generic, public :: push => push_real,push_real_array,push_int,push_int_array
     generic, public :: pop => pop_real,pop_real_array,pop_int,pop_int_array
  end type adstack

  type(adstack), target :: core_adstack

  abstract interface
     integer(int32) function chain_op(i)
       use iso_fortran_env
       integer(int32), intent(in) :: i
     end function chain_op
  end interface

contains

  ! move according to inserted object size
  subroutine incr(stack, len, update_tail)
    class(adstack), intent(inout) :: stack
    integer(int32), intent(in) :: len
    logical, intent(in) :: update_tail
    if (update_tail) stack%j_ = stack%i_
    stack%i_ = stack%i_ + len
  end subroutine incr

  subroutine push_real(stack, a)
    class(adstack), intent(inout) :: stack
    real(rk), intent(in) :: a
    integer(ik), parameter :: n = 8 ! 64/8=8
    stack%s_(stack%i_:(stack%i_+n-1)) = transfer(a, 0_int8, n)
    call stack%incr(n, .false.)
  end subroutine push_real

  subroutine push_real_array(stack, a)
    class(adstack), intent(inout) :: stack
    real(rk), intent(in) :: a(:)
    integer(ik), parameter :: n = 8 ! 64/8=8
    stack%s_(stack%i_:(stack%i_+n*size(a)-1)) = transfer(a, 0_int8, n*size(a))
    call stack%incr(n*size(a), .false.)
  end subroutine push_real_array

  subroutine push_int(stack, i)
    class(adstack), intent(inout) :: stack
    integer(ik), intent(in) :: i
    integer(ik), parameter :: n = 4 ! 32/8=4
    stack%s_(stack%i_:(stack%i_+n-1)) = transfer(i, 0_int8, n)
    call stack%incr(n, .false.)
  end subroutine push_int

  subroutine push_int_array(stack, a)
    class(adstack), intent(inout) :: stack
    integer(ik), intent(in) :: a(:)
    integer(ik), parameter :: n = 4 ! 32/8=4
    stack%s_(stack%i_:(stack%i_+n*size(a)-1)) = transfer(a, 0_int8, n*size(a))
    call stack%incr(n*size(a), .false.)
  end subroutine push_int_array

  subroutine pop_real(stack, i, a)
    class(adstack), intent(in) :: stack
    integer(ik), intent(in) :: i
    real(rk), intent(out) :: a
    integer(ik), parameter :: n = 8 ! 64/8=8
    a = transfer(stack%s_(i:(i+n-1)), a)
  end subroutine pop_real

  subroutine pop_real_array(stack, i, a)
    class(adstack), intent(in) :: stack
    integer(ik), intent(in) :: i
    real(rk), intent(out) :: a(:)
    integer(ik), parameter :: n = 8 ! 64/8=8
    a = transfer(stack%s_(i:(i+n*size(a)-1)), a(1), size(a))
  end subroutine pop_real_array

  subroutine pop_int(stack, i, a)
    class(adstack), intent(in) :: stack
    integer(ik), intent(in) :: i
    integer(ik), intent(out) :: a
    integer(ik), parameter :: n = 4 ! 32/8=8
    a = transfer(stack%s_(i:(i+n-1)), a)
  end subroutine pop_int

  subroutine pop_int_array(stack, i, a)
    class(adstack), intent(in) :: stack
    integer(ik), intent(in) :: i
    integer(ik), intent(out) :: a(:)
    integer(ik), parameter :: n = 4 ! 32/8=8
    a = transfer(stack%s_(i:(i+n*size(a)-1)), a(1), size(a))
  end subroutine pop_int_array

  ! subroutine chain()


end module fazang_env
