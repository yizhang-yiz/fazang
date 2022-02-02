module vari_mod
  use iso_fortran_env
  use env_mod
  use tape_mod

  implicit none

  private
  public :: vari, adstack, callstack, chain_op, assignment(=)
  public :: n_operand, operand_val, operand_adj, operand_index
  public :: adj, val
  public :: vari_index, set_indexed_val, set_indexed_adj, set_indexed_chain
  public :: get_indexed_val, get_indexed_adj
  public :: vari_at

  type :: vari
     integer(ik) :: i = 0    ! corresponding id in tape lookup
     procedure(chain_op), pass, pointer :: chain => chain_dummy
   contains
     procedure :: val
     procedure :: adj
     procedure :: n_operand, operand_val, operand_adj, operand_index
     procedure :: matrix_operand_1_index, matrix_operand_2_index
     procedure :: data_operand
     procedure :: set_val
     procedure :: set_adj
     procedure :: set_zero_adj
     procedure :: init_dependent
     procedure :: set_operand_adj
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

  elemental function vari_index(i) result(id)
    integer(ik), intent(in) :: i
    integer(ik) :: id
    id = callstack % varis(i) % i
  end function vari_index

  function vari_at(id) result(vp)
    integer(ik), intent(in) :: id
    type(vari), pointer :: vp
    vp => callstack % varis(id)
  end function vari_at

  function new_vari_val(d) result(vp)
    real(rk), intent(in) :: d
    integer(ik) :: vp
    callstack%varis(callstack%head)%i = callstack % stack % push(d)
    vp = callstack%head
    call incr1(callstack%head)
  end function new_vari_val

  function new_vari() result(vp)
    integer(ik) :: vp
    vp = new_vari_val(0.0d0)
  end function new_vari

  function new_vari_val_op(d, op) result(vp)
    real(rk), intent(in) :: d
    integer(ik), intent(in) :: op(:)
    integer(ik) :: vp
    callstack%varis(callstack%head)%i = callstack % stack % push(d, op)
    vp = callstack%head
    call incr1(callstack%head)
  end function new_vari_val_op

  function new_vari_val_dop(d, dop) result(vp)
    real(rk), intent(in) :: d
    real(rk), intent(in) :: dop(:)
    integer(ik) :: vp
    callstack%varis(callstack%head)%i = callstack % stack % push(d, dop)
    vp = callstack%head
    call incr1(callstack%head)
  end function new_vari_val_dop

  function new_vari_val_op_dop(d, op, dop) result(vp)
    real(rk), intent(in) :: d
    integer(ik), intent(in) :: op(:)
    real(rk), intent(in) :: dop(:)
    integer(ik) :: vp
    callstack%varis(callstack%head)%i = callstack % stack % push(d,&
         & op, dop)
    vp = callstack%head
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

  subroutine set_operand_adj(this, d)
    class(vari), intent(in) :: this
    real(rk) :: d(this%n_operand())
    integer(ik) :: id(this%n_operand()), i
    id = this%operand_index()
    do i = 1, size(id)
       call callstack % stack % set_adj(id(i), d(i))
    end do
  end subroutine set_operand_adj

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

  pure function matrix_operand_1_index(this) result(id)
    class(vari), intent(in) :: this
    integer(ik) :: id
    id = callstack % stack % storage(this%i + 4 + 3)
  end function matrix_operand_1_index

  pure function matrix_operand_2_index(this) result(id)
    class(vari), intent(in) :: this
    integer(ik) :: id
    id = callstack % stack % storage(this%i + 4 + 4)
  end function matrix_operand_2_index

  subroutine set_val(this, d)
    class(vari), intent(in) :: this
    real(rk) :: d
    call callstack%stack%set_val(this%i, d)
  end subroutine set_val

  subroutine set_indexed_chain(id, proc)
    integer(ik), intent(in) :: id
    procedure(chain_op) :: proc
    callstack % varis(id) % chain => proc
  end subroutine set_indexed_chain

  elemental function get_indexed_val(id) result(d)
    integer(ik), intent(in) :: id
    real(rk) :: d
    d = callstack % varis(id) % val()
  end function get_indexed_val

  elemental function get_indexed_adj(id) result(d)
    integer(ik), intent(in) :: id
    real(rk) :: d
    d = callstack % varis(id) % adj()
  end function get_indexed_adj

  subroutine set_indexed_val(id, d)
    integer(ik), intent(in) :: id
    real(rk) :: d
    call set_val(callstack % varis(id), d)
  end subroutine set_indexed_val

  subroutine set_adj(this, d)
    class(vari), intent(in) :: this
    real(rk) :: d
    call callstack % stack % set_adj(this%i, d)
  end subroutine set_adj

  subroutine set_indexed_adj(id, d)
    integer(ik), intent(in) :: id
    real(rk) :: d
    call set_adj(callstack % varis(id), d)
  end subroutine set_indexed_adj

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
