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

  real(rk), parameter :: eps = tiny(0.0d0)  ! smallest
  real(rk), parameter :: log_eps = log(eps) ! log(smallest)
  real(rk), parameter :: pi = 4.D0*DATAN(1.D0)
  real(rk), parameter :: log2 = log(2.d0)

  type :: adstack
     integer(c_int8_t) :: s_(adsize) = 0      ! int8 serves as byte
     integer(ik) :: i_ = 1       ! current (vacant) location
     integer(ik) :: nest_level = 0, i_nest(max_nest_level)
     integer(ik) :: nvari = 0
   contains
     procedure push_real
     procedure push_real_array
     procedure push_int
     procedure push_int_array
     procedure push_int_real
     procedure pop_real
     procedure pop_real_array
     procedure pop_int
     procedure pop_int_array
     procedure :: incr
     generic :: push => push_real,push_real_array,push_int,push_int_array,push_int_real
     generic :: pop => pop_real,pop_real_array,pop_int,pop_int_array
     procedure :: reboot
  end type adstack

  type(adstack), target :: core_adstack

contains

  ! move according to inserted object size
  subroutine incr(stack, len)
    implicit none
    class(adstack), intent(inout) :: stack
    integer(ik), intent(in) :: len
    if (stack%i_ + len > adsize) error stop
    stack%i_ = stack%i_ + len
  end subroutine incr

  subroutine push_real(stack, a)
    implicit none
    class(adstack), target, intent(inout) :: stack
    real(rk), intent(in) :: a
    type(c_ptr) :: cp
    real(rk), pointer :: p
    cp = c_loc(stack%s_(stack%i_))
    call c_f_pointer(cp, p); p = a
    call stack%incr(rksize)
  end subroutine push_real

  subroutine push_int_real(stack, i, a)
    implicit none
    class(adstack), target, intent(inout) :: stack
    real(rk), intent(in) :: a
    integer(ik), intent(in) :: i
    type(c_ptr) :: cp
    integer(ik), pointer :: p1
    real(rk), pointer :: p2
    cp = c_loc(stack%s_(stack%i_))
    call c_f_pointer(cp, p1); p1 = i
    cp = c_loc(stack%s_(stack%i_+iksize))
    call c_f_pointer(cp, p2); p2 = a
    call stack%incr(iksize+rksize)
  end subroutine push_int_real

  ! subroutine push_int_real_real(stack, i, a, b)
  !   implicit none
  !   class(adstack), target, intent(inout) :: stack
  !   real(rk), intent(in) :: a
  !   integer(ik), intent(in) :: i
  !   type(c_ptr) :: cp
  !   integer(ik), pointer :: p1
  !   real(rk), pointer :: p2
  !   cp = c_loc(stack%s_(stack%i_))
  !   call c_f_pointer(cp, p1); p1 = i
  !   cp = c_loc(stack%s_(stack%i_+iksize))
  !   call c_f_pointer(cp, p2); p2 = a
  !   cp = c_loc(stack%s_(stack%i_+iksize+rksize))
  !   call c_f_pointer(cp, p2); p2 = b
  !   call stack%incr(rksize)
  ! end subroutine push_int_real_real

  subroutine push_real_array(stack, a)
    implicit none
    class(adstack), target, intent(inout) :: stack
    real(rk), intent(in) :: a(:)
    type(c_ptr) :: cp
    real(rk), pointer :: p(:)
    cp = c_loc(stack%s_(stack%i_))
    call c_f_pointer(cp, p, shape=[size(a)]); p = a
    call stack%incr(rksize*size(a))
  end subroutine push_real_array

  subroutine push_int(stack, i)
    implicit none
    class(adstack), target, intent(inout) :: stack
    integer(ik), intent(in) :: i
    type(c_ptr) :: cp
    integer(ik), pointer :: p
    cp = c_loc(stack%s_(stack%i_))
    call c_f_pointer(cp, p); p = i
    call stack%incr(iksize)
  end subroutine push_int

  subroutine push_int_array(stack, a)
    implicit none
    class(adstack), target, intent(inout) :: stack
    integer(ik), intent(in) :: a(:)
    type(c_ptr) :: cp
    integer(ik), pointer :: p(:)
    cp = c_loc(stack%s_(stack%i_))
    call c_f_pointer(cp, p, shape=[size(a)]); p = a
    call stack%incr(iksize*size(a))
  end subroutine push_int_array

  elemental subroutine pop_real(stack, i, res)
    class(adstack), intent(in) :: stack
    integer(ik), intent(inout) :: i
    real(rk), intent(out) :: res
    res = transfer(stack%s_(i:(i + rksize -1)), res)
    i = i + rksize
  end subroutine pop_real

  elemental subroutine pop_int(stack, i, res)
    implicit none
    class(adstack), intent(in) :: stack
    integer(ik), intent(inout) :: i
    integer(ik), intent(out) :: res
    res = transfer(stack%s_(i:(i + iksize-1)), res)
    i = i + iksize
  end subroutine pop_int

  subroutine reboot(this)
    implicit none
    class(adstack), intent(inout) :: this
     this%i_ = 1
     this%nest_level = 0
     this%i_nest = 0
     this%nvari = 0
   end subroutine reboot

   subroutine reboot_chain()
     implicit none
     call core_adstack%reboot()
   end subroutine reboot_chain

   subroutine pop_real_array(stack, i, n, res)
     implicit none
     class(adstack), intent(in) :: stack
     real(rk), pointer, intent(out) :: res(:)
     integer(ik), intent(in) :: n
     integer(ik), intent(inout) :: i
     type(c_ptr) :: cp
     cp = c_loc(core_adstack%s_(i))
     call c_f_pointer(cp, res, [(n)])
     i = i + rksize * n
   end subroutine pop_real_array

   subroutine pop_int_array(stack, i, n, res)
     implicit none
     class(adstack), intent(in) :: stack
     integer(ik), pointer, intent(out) :: res(:)
     integer(ik), intent(in) :: n
     integer(ik), intent(inout) :: i
     type(c_ptr) :: cp
     cp = c_loc(core_adstack%s_(i))
     call c_f_pointer(cp, res, [(n)])
     i = i + iksize * n
   end subroutine pop_int_array

end module fz_env
