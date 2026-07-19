#include "recover_inc.f90"

module fazang_vari
  use, intrinsic :: iso_fortran_env
  use fazang_env

  private
  public :: vari, set_vari_val_stack, set_vari_real32_stack, assignment(=)
  public :: recover, val, adj

  type :: vari
     real(rk) :: val_
     real(rk) :: adj_ = 0d0
     integer(ik) :: i = 0
     byte, pointer, public :: stack(:) => null()
   contains
     procedure, public :: val
     procedure, public :: adj
     ! procedure, public :: set_val
     ! procedure, public :: set_adj
  end type vari

  interface assignment(=)
     module procedure set_vari_val
     module procedure set_vari_real32
  end interface assignment(=)

contains

  elemental integer(ik) function varisize(v) ! in bytes
    class(vari), intent(in) :: v
    varisize = storage_size(v)/8
  end function varisize

  function new_vari_val(val) result(v)
    real(rk), intent(in) :: val
    type(vari) :: v
    call set_vari_val(v, val)
  end function new_vari_val

  elemental real(rk) function val(this)
    class(vari), intent(in) :: this

    val = this%val_
  end function val

  elemental real(rk) function adj(this)
    class(vari), intent(in) :: this
    adj = this%adj_
  end function adj

  subroutine set_vari_val(this, val)
    implicit none

    class(vari), intent(out) :: this
    real(rk), intent(in) :: val
    call set_vari_val_stack(this, val, core_adstack)
  end subroutine set_vari_val

  subroutine set_vari_real32(this, val)
    implicit none

    class(vari), intent(out) :: this
    real(real32), intent(in) :: val
    call set_vari_real32_stack(this, val, core_adstack)
  end subroutine set_vari_real32

  subroutine set_vari_val_stack(this, val, stack)
    implicit none
    class(vari), intent(out) :: this
    real(rk), intent(in) :: val
    byte, intent(inout) :: stack(:)
    this%val_ = val
    call push(this, stack)
  end subroutine set_vari_val_stack

  subroutine set_vari_real32_stack(this, val, stack)
    implicit none

    class(vari), intent(out) :: this
    real(real32), intent(in) :: val
    byte, intent(inout) :: stack(:)
    this%val_ = val
    call push(this, stack)
  end subroutine set_vari_real32_stack

  subroutine push(v, stack)
    implicit none
    class(vari), intent(inout) :: v
    byte, target, intent(inout) :: stack(:)
    integer :: mysize
    mysize = storage_size(v)/8
    if (v%i == 0 .or. (.not. associated(v%stack))) then
       stack(adstack_index(stack):(adstack_index(stack)+mysize-1)) = transfer(v, 0_int8, mysize)
       v%i = adstack_index(stack)
       v%stack => stack
       call incr_adstack(stack, mysize)
    else
       stack(adstack_index(stack):(adstack_index(stack)+mysize-1)) = transfer(v, 0_int8, mysize)
       v%stack => stack
    endif
  end subroutine push


  ! subroutine recover(v)
  !   implicit none
  !   type(vari), intent(inout) :: v
  !   if (v%i == 0 .or. (.not. associated(v%stack))) error stop 5555
  !   v = transfer(v%stack(v%i:(v%i+sizeof(v))), v)
  ! end subroutine recover

  DEF_RECOVER(v, vari)

end module fazang_vari
