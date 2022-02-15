module fazang_var_mod
  use iso_fortran_env
  use fazang_env_mod
  use fazang_vari_mod, only : vari, callstack, vari_index, get_indexed_val,&
       & get_indexed_adj, set_indexed_val, set_indexed_adj,&
       & set_indexed_chain, chain_op

  implicit none

  private
  public :: var, assignment(=), vi_index, adj, val

  type :: var
     ! we cannot use pointer (=> vari) here because when use move_alloc to
     ! enlarge the storage the address will be changed, thus instead
     ! we store the id in the callstack % varis array
     integer(ik) :: vi
   contains
     procedure :: val
     procedure :: adj
     procedure :: grad, grad_nested
     procedure :: vi_index
     procedure :: set_chain
  end type var

  interface var
     module procedure :: new_var_val
     module procedure :: new_var
     module procedure :: new_var_1d
     module procedure :: new_var_2d
     module procedure :: new_var_val_op
     module procedure :: new_var_val_opid
     module procedure :: new_var_val_dop
     module procedure :: new_var_val_op_dop
  end interface var

  interface assignment(=)
     module procedure set_var_0d
     module procedure set_var_1d
     module procedure set_var_2d
     module procedure set_var_1d_from_0d
     module procedure set_var_2d_from_0d
  end interface assignment(=)

contains

  impure elemental function new_var_val(d) result(v)
    real(rk), intent(in) :: d
    type(var) :: v
    v%vi = vari(d)
  end function new_var_val

  function new_var() result(v)
    type(var) :: v
    v%vi = vari()
  end function new_var

  function new_var_val_op(d, op) result(v)
    real(rk), intent(in) :: d
    type(var) :: v
    type(var), intent(in) :: op(:)
    v%vi = vari(d, vi_index(op))
  end function new_var_val_op

  function new_var_val_opid(d, op) result(v)
    real(rk), intent(in) :: d
    type(var) :: v
    integer(ik), intent(in) :: op(:)
    v%vi = vari(d, op)
  end function new_var_val_opid

  function new_var_val_dop(d, dop) result(v)
    real(rk), intent(in) :: d
    type(var) :: v
    real(rk), intent(in) :: dop(:)
    v%vi = vari(d, dop)
  end function new_var_val_dop

  function new_var_val_op_dop(d, op, dop) result(v)
    real(rk), intent(in) :: d
    type(var) :: v
    type(var), intent(in) :: op(:)
    real(rk), intent(in) :: dop(:)
    v%vi = vari(d, vi_index(op), dop)
  end function new_var_val_op_dop

  function new_var_1d(n) result(v)
    integer(ik), intent(in) :: n
    type(var) :: v(n)
    integer :: i
    do i = 1, n
       v(i) = new_var()
    end do
  end function new_var_1d

  function new_var_2d(m, n) result(v)
    integer(ik), intent(in) :: m, n
    type(var) :: v(m, n)
    integer :: i
    do i = 1, n
       v(:, i) = new_var_1d(m)
    end do
  end function new_var_2d

  subroutine set_var_0d(this, from)
    type(var), intent(inout) :: this    
    real(rk), intent(in) :: from
    call set_indexed_val(this%vi, from)
  end subroutine set_var_0d

  subroutine set_var_1d(this, from)
    integer i
    type(var), intent(inout) :: this(:)
    real(rk), intent(in) :: from(:)
    do i = lbound(this,1), ubound(this,1)
       call set_var_0d(this(i), from(i))
    end do
  end subroutine set_var_1d

  subroutine set_var_1d_from_0d(this, from)
    integer i
    type(var), intent(inout) :: this(:)
    real(rk), intent(in) :: from
    do i = lbound(this,1), ubound(this,1)
       call set_var_0d(this(i), from)
    end do
  end subroutine set_var_1d_from_0d

  subroutine set_var_2d(this, from)
    integer i
    type(var), intent(inout) :: this(:, :)
    real(rk), intent(in) :: from(:, :)
    do i = lbound(this,2), ubound(this,2)
       call set_var_1d(this(:, i), from(:, i))
    end do
  end subroutine set_var_2d

  subroutine set_var_2d_from_0d(this, from)
    integer i
    type(var), intent(inout) :: this(:, :)
    real(rk), intent(in) :: from
    do i = lbound(this,2), ubound(this,2)
       call set_var_1d_from_0d(this(:, i), from)
    end do
  end subroutine set_var_2d_from_0d

  elemental real(rk) function val(this)
    class(var), intent(in) :: this
    val = get_indexed_val(this%vi)
  end function val

  elemental real(rk) function adj(this)
    class(var), intent(in) :: this
    adj = get_indexed_adj(this%vi)
  end function adj

  elemental function vi_index(this) result(s)
    class(var), intent(in) :: this    
    integer(ik) :: s
    s = vari_index(this%vi)
  end function vi_index

  subroutine set_chain(this, proc)
    class(var), intent(in) :: this    
    procedure(chain_op) :: proc
    call set_indexed_chain(this%vi, proc)
  end subroutine set_chain

  subroutine grad(this)
    class(var), intent(in) :: this
    integer i
    call callstack % varis (this%vi) % init_dependent()
    do i = callstack%head - 1, 1, -1
       call callstack%varis(i)%chain()
    end do
  end subroutine grad

  ! only traverse the top nested tape
  subroutine grad_nested(this)
    use fazang_nested_tape_mod
    class(var), intent(in) :: this
    integer i
    call callstack % varis (this%vi) % init_dependent()
    do i = callstack%head - 1, curr_nested_vari_head(), -1
       call callstack%varis(i)%chain()
    end do
  end subroutine grad_nested

end module fazang_var_mod
