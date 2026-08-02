module fz_env
  use, intrinsic :: iso_fortran_env
  use, intrinsic :: iso_c_binding

  implicit none

#ifdef fz_adstack_size
  integer(int32), parameter :: adsize = fz_adstack_size * (1048576)
#else                           ! default 50 MB
  integer(int32), parameter :: adsize = 50 * (1048576)
#endif

  integer, parameter :: rk = c_double
  integer, parameter :: ik = c_int32_t
  integer(ik), parameter :: iksize = storage_size(0_c_int32_t)/storage_size(0_c_int8_t)
  integer(ik), parameter :: rksize = storage_size(0_c_double)/storage_size(0_c_int8_t)
  integer(ik), parameter :: max_nest_level = 9

  ! smallest
  real(rk), parameter :: eps = tiny(0.0d0)

  ! log(smallest)
  real(rk), parameter :: log_eps = log(eps)

  type :: adstack
     integer(c_int8_t) :: s_(adsize) = 0      ! int8 serves as byte
     integer(ik) :: i_ = 1       ! current (vacant) location
     integer(ik) :: j_ = 0       ! previous (just filled) location
     integer(ik) :: nest_level = 0, i_nest(max_nest_level)
   contains
     procedure push_real
     procedure push_real_array
     procedure push_int
     procedure push_int_array
     procedure pop_real
     ! procedure pop_real_array
     procedure pop_int
     ! procedure pop_int_array
     procedure :: incr
     generic :: push => push_real,push_real_array,push_int,push_int_array
     procedure :: reboot
  end type adstack

  type(adstack), target :: core_adstack

contains

  ! move according to inserted object size
  subroutine incr(stack, len, update_tail)
    implicit none
    class(adstack), intent(inout) :: stack
    integer(ik), intent(in) :: len
    logical, intent(in) :: update_tail
    if (update_tail) stack%j_ = stack%i_
    if (stack%i_ + len > adsize) error stop
    stack%i_ = stack%i_ + len
  end subroutine incr

  subroutine push_real(stack, a)
    implicit none
    class(adstack), intent(inout) :: stack
    real(rk), intent(in) :: a
    stack%s_(stack%i_:(stack%i_+rksize-1)) = transfer(a, 0_c_int8_t, rksize)
    call stack%incr(rksize, .false.)
  end subroutine push_real

  subroutine push_real_array(stack, a)
    implicit none
    class(adstack), intent(inout) :: stack
    real(rk), intent(in) :: a(:)
    stack%s_(stack%i_:(stack%i_+rksize*size(a)-1)) = transfer(a, 0_c_int8_t, rksize*size(a))
    call stack%incr(rksize*size(a), .false.)
  end subroutine push_real_array

  subroutine push_int(stack, i)
    implicit none
    class(adstack), intent(inout) :: stack
    integer(ik), intent(in) :: i
    stack%s_(stack%i_:(stack%i_+iksize-1)) = transfer(i, 0_c_int8_t, iksize)
    call stack%incr(iksize, .false.)
  end subroutine push_int

  subroutine push_int_array(stack, a)
    implicit none
    class(adstack), intent(inout) :: stack
    integer(ik), intent(in) :: a(:)
    stack%s_(stack%i_:(stack%i_+iksize*size(a)-1)) = transfer(a, 0_c_int8_t, iksize*size(a))
    call stack%incr(iksize*size(a), .false.)
  end subroutine push_int_array

  elemental real(rk) function pop_real(stack, i)
    class(adstack), intent(in) :: stack
    integer(ik), intent(in) :: i
    pop_real = transfer(stack%s_(i:(i+rksize-1)), pop_real)
  end function pop_real

!   pure subroutine pop_real_array(stack, i, a)
!     class(adstack), intent(in) :: stack
!     integer(ik), intent(in) :: i
!     real(rk), intent(out) :: a(:)
!     integer(ik), parameter :: n = 8 ! 64/8=8
!     a = transfer(stack%s_(i:(i+n*size(a)-1)), a(1), size(a))
!   end subroutine pop_real_array

  elemental integer(ik) function pop_int(stack, i)
    implicit none
    class(adstack), intent(in) :: stack
    integer(ik), intent(in) :: i
    pop_int = transfer(stack%s_(i:(i+iksize-1)), pop_int)
  end function pop_int

!   pure subroutine pop_int_array(stack, i, a)
!     class(adstack), intent(in) :: stack
!     integer(ik), intent(in) :: i
!     integer(ik), intent(out) :: a(:)
!     integer(ik), parameter :: n = 4 ! 32/8=8
!     a = transfer(stack%s_(i:(i+n*size(a)-1)), a(1), size(a))
!   end subroutine pop_int_array

  subroutine reboot(this)
    implicit none
    class(adstack), intent(inout) :: this
     this%s_ = 0
     this%i_ = 1
     this%j_ = 0
     this%nest_level = 0
     this%i_nest = 0
   end subroutine reboot
end module fz_env
