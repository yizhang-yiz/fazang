module vari_mod
  use iso_fortran_env
  use env_mod
  use tape_mod

  implicit none

  private
  public :: vari, adstack, callstack, chain_op, assignment(=)
  public :: n_operand, operand_val, operand_adj, operand_index
  public :: adj, val

  type :: vari
     integer(ik) :: i = 0    ! corresponding id in tape lookup
     procedure(chain_op), pass, pointer :: chain => chain_dummy
   contains
     procedure :: val
     procedure :: adj
     procedure :: n_operand, operand_val, operand_adj, operand_index
     procedure :: data_operand
     procedure :: set_val
     procedure :: set_adj
     procedure :: set_zero_adj
     procedure :: init_dependent
  end type vari

  abstract interface
     subroutine chain_op(this)
       import :: vari
       class(vari), intent(in) :: this
     end subroutine chain_op
  end interface

! due to gfortran bugs I cannot use PDT for
! adstack.
! for now I have to use adstack.f90 with hard-coded stack size (i
! don't want to use pointer/alloc type)

  type :: adstack
     integer(int32) :: head = 1
     type(vari) :: varis(adstack_len)
     type(tape) :: stack
   contains
     procedure :: set_zero_all_adj
     ! procedure :: val
     ! procedure :: adj
     ! procedure :: set_val
     ! procedure :: set_adj
  end type adstack

  type(adstack), target :: callstack

  interface vari
     module procedure :: new_vari_val
     module procedure :: new_vari
     module procedure :: new_vari_val_op
     module procedure :: new_vari_val_dop
     module procedure :: new_vari_val_op_dop
  end interface vari
  
  interface assignment(=)
     module procedure set_vari_0d
  end interface assignment(=)

contains

  function new_vari_val(d) result(vp)
    real(rk), intent(in) :: d
    type(vari), pointer :: vp
    callstack%varis(callstack%head)%i = callstack % stack % push(d)
    vp => callstack%varis(callstack%head)
    call incr1(callstack%head)
  end function new_vari_val

  function new_vari() result(vp)
    type(vari), pointer :: vp
    vp => new_vari_val(0.0d0)
  end function new_vari

  function new_vari_val_op(d, op) result(vp)
    real(rk), intent(in) :: d
    integer(ik), intent(in) :: op(:)
    type(vari), pointer :: vp
    callstack%varis(callstack%head)%i = callstack % stack % push(d, op)
    vp => callstack%varis(callstack%head)
    call incr1(callstack%head)
  end function new_vari_val_op

  function new_vari_val_dop(d, dop) result(vp)
    real(rk), intent(in) :: d
    real(rk), intent(in) :: dop(:)
    type(vari), pointer :: vp
    callstack%varis(callstack%head)%i = callstack % stack % push(d, dop)
    vp => callstack%varis(callstack%head)
    call incr1(callstack%head)
  end function new_vari_val_dop

  function new_vari_val_op_dop(d, op, dop) result(vp)
    real(rk), intent(in) :: d
    integer(ik), intent(in) :: op(:)
    real(rk), intent(in) :: dop(:)
    type(vari), pointer :: vp
    callstack%varis(callstack%head)%i = callstack % stack % push(d,&
         & op, dop)
    vp => callstack%varis(callstack%head)
    call incr1(callstack%head)
  end function new_vari_val_op_dop

  elemental real(rk) function val(this)
    class(vari), intent(in) :: this
    val = callstack%stack%val(this%i)
  end function val

  elemental real(rk) function adj(this)
    class(vari), intent(in) :: this
    adj = callstack%stack%adj(this%i)
  end function adj

  elemental integer(ik) function n_operand(this)
    class(vari), intent(in) :: this
    n_operand = callstack % stack % n_operand(this%i)
  end function n_operand

  pure function operand_val(this)
    class(vari), intent(in) :: this
    real(rk) :: operand_val(this%n_operand())
    operand_val = callstack % stack % operand_val(this%i)
  end function operand_val

  pure function operand_adj(this)
    class(vari), intent(in) :: this
    real(rk) :: operand_adj(this%n_operand())
    operand_adj = callstack % stack % operand_adj(this%i)
  end function operand_adj

  pure function data_operand(this)
    class(vari), intent(in) :: this
    real(rk) :: data_operand(callstack % stack % n_data_operand(this%i))
    data_operand = callstack % stack % data_operand(this%i)
  end function data_operand

  pure function operand_index(this) result(id)
    class(vari), intent(in) :: this
    integer(ik) :: id(this%n_operand())
    id = callstack % stack % operand_index(this%i)
  end function operand_index

  subroutine set_val(this, d)
    class(vari), intent(in) :: this
    real(rk) :: d
    call callstack%stack%set_val(this%i, d)
  end subroutine set_val

  subroutine set_adj(this, d)
    class(vari), intent(in) :: this
    real(rk) :: d
    call callstack%stack%set_adj(this%i, d)
  end subroutine set_adj

  subroutine set_zero_adj(this)
    class(vari), intent(in) :: this
    call this%set_adj(0.0D0)
  end subroutine set_zero_adj

  subroutine init_dependent(this)
    class(vari), intent(in) :: this
    call this%set_adj(1.0D0)
  end subroutine init_dependent

  subroutine set_vari_0d(this, from)
    type(vari), intent(inout) :: this    
    real(rk), intent(in) :: from
    call this%set_val(from)
  end subroutine set_vari_0d

  subroutine set_zero_all_adj (this)
    class(adstack), intent(inout) :: this
    integer(ik) i
    do i = callstack%head - 1, 1, -1
       call this % stack % set_adj(callstack%varis(i)%i, 0.0d0)
    end do

  end subroutine set_zero_all_adj

  subroutine chain_dummy(this)
    class(vari), intent(in) :: this
    integer(ik) i
    i = this%i
  end subroutine chain_dummy

end module vari_mod
