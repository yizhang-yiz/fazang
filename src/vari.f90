module vari_mod
  use iso_fortran_env
  use env_mod

  implicit none

  type :: vari
     integer(ik) :: i = 0    ! corresponding id in adstack & operands lookup
     integer(ik) :: n_operand = 0   ! # of operandsa
     procedure(chain_op), pass, pointer :: chain => chain_dummy
   contains
     procedure :: val
     procedure :: adj
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

! due to a gfortran bug I cannot use parameterized type for
! adstack. The following code is correct by f2003 standard and can be
! potentially compiled by Flang

! for now I have to use adstack.f90 with hard-coded stack size (i
! don't want to use pointer/alloc type)

  type :: adstack
     integer(int32) :: head = 1
     integer(int32) :: operand_index_head = 1
     real(real64) :: storage(2, adstack_len) = 0.d0
     type(vari) :: varis(adstack_len)
     integer(ik) :: operand_index_begin(adstack_len) = 0
     integer(ik) :: operand_index(adstack_len * 5) !crude guess
   contains
     procedure :: set_head
     procedure :: reset_head
     procedure :: push_head
     procedure :: push_operand
     procedure :: get_operand_index
     procedure :: set_zero_all_adj
     generic :: set_val => set_val_i, set_val_head
     generic :: set_adj => set_adj_i, set_adj_head
     generic :: val => val_i, val_head
     generic :: adj => adj_i, adj_head
     procedure, private :: val_i
     procedure, private :: val_head
     procedure, private :: adj_i
     procedure, private :: adj_head
     procedure, private :: set_val_i
     procedure, private :: set_val_head
     procedure, private :: set_adj_i
     procedure, private :: set_adj_head
  end type adstack

  type(adstack), target :: callstack

  interface vari
     module procedure :: new_vari_val
     module procedure :: new_vari
  end interface vari
  
  interface assignment(=)
     module procedure set_vari_0d
  end interface assignment(=)

  private :: new_vari_val, new_vari, set_vari_0d, chain_dummy

contains

  function new_vari_val(d) result(vp)
    real(rk), intent(in) :: d
    type(vari), pointer :: vp
    if ( callstack%head == adstack_len ) then
       error stop
    end if
    callstack%storage(1, callstack%head) = d
    callstack%varis(callstack%head)%i = callstack%head
    vp => callstack%varis(callstack%head)
    callstack%head = callstack%head + 1
  end function new_vari_val

  function new_vari() result(vp)
    type(vari), pointer :: vp
    vp => new_vari_val(0.0d0)
  end function new_vari

  pure real(rk) function val(this)
    class(vari), intent(in) :: this
    val = callstack%storage(1, this%i)
  end function val

  pure real(rk) function adj(this)
    class(vari), intent(in) :: this
    adj = callstack%storage(2, this%i)
  end function adj

  subroutine set_val(this, d)
    class(vari), intent(in) :: this
    real(rk) :: d
    callstack%storage(1, this%i) = d
  end subroutine set_val

  subroutine set_adj(this, d)
    class(vari), intent(in) :: this
    real(rk) :: d
    callstack%storage(2, this%i) = d
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

  ! for adstack
  subroutine set_head (this, i)
    class(adstack), intent(inout) :: this
    integer(int32), intent(in) :: i
    this%head = i
  end subroutine set_head

  subroutine reset_head (this)
    class(adstack), intent(inout) :: this
    this%head = 1
  end subroutine reset_head

  subroutine push_head (this)
    class(adstack), intent(inout) :: this
    this%head = this%head + 1
  end subroutine push_head

  subroutine push_operand (this, v0, v1)
    type(vari), intent(in) :: v0, v1
    class(adstack), intent(inout) :: this
    if ( this%operand_index_begin(v0%i) == 0 ) then
       this%operand_index_begin(v0%i) = this%operand_index_head
    end if
    this%operand_index(this%operand_index_head) = v1%i
    this%operand_index_head = this%operand_index_head + 1
  end subroutine push_operand

  pure function get_operand_index (this, v) result(i)
    type(vari), intent(in) :: v
    class(adstack), intent(in) :: this
    integer(ik) :: i(v%n_operand)
    integer(ik) ibegin, j
    if ( v%n_operand > 0 ) then
       ibegin = this%operand_index_begin(v%i)
       do j = 1, v%n_operand
          i(j) = this%operand_index(ibegin + j - 1)
       end do
    end if
  end function get_operand_index

  subroutine set_zero_all_adj (this)
    class(adstack), intent(inout) :: this
    this%storage(2, :) = 0.D0
  end subroutine set_zero_all_adj

  subroutine set_val_i (this, i, d)
    class(adstack), intent(inout) :: this
    real(rk), intent(in) :: d
    integer(ik), intent(in) :: i
    this%storage(1, i) = d
  end subroutine set_val_i

  subroutine set_adj_i (this, i, d)
    class(adstack), intent(inout) :: this
    real(rk), intent(in) :: d
    integer(ik), intent(in) :: i
    this%storage(2, i) = d
  end subroutine set_adj_i

  subroutine set_val_head (this, d)
    class(adstack), intent(inout) :: this
    real(rk), intent(in) :: d
    this%storage(1, this%head) = d
  end subroutine set_val_head

  subroutine set_adj_head (this, d)
    class(adstack), intent(inout) :: this
    real(real64), intent(in) :: d
    this%storage(2, this%head) = d
  end subroutine set_adj_head

  pure real(rk) function val_i (this, i)
    class(adstack), intent(in) :: this
    integer(ik), intent(in) :: i
    val_i = this%storage(1, i)
  end function val_i

  pure real(rk) function adj_i (this, i)
    class(adstack), intent(in) :: this
    integer(ik), intent(in) :: i
    adj_i = this%storage(2, i)
  end function adj_i

  pure real(rk) function val_head (this)
    class(adstack), intent(in) :: this
    val_head = this%storage(1, this%head)
  end function val_head

  pure real(rk) function adj_head (this)
    class(adstack), intent(in) :: this
    adj_head = this%storage(2, this%head)
  end function adj_head

  subroutine chain_dummy(this)
    class(vari), intent(in) :: this
    integer i
    do i = this%i, 0
    end do
  end subroutine chain_dummy

end module vari_mod
