module var_mod
  use iso_fortran_env
  use env_mod
  use vari_mod, only : vari, callstack

  implicit none

  private
  public :: var, assignment(=), vari_index, adj, val

  type :: var
     type(vari), pointer :: vi => null()
   contains
     procedure :: val
     procedure :: adj
     procedure :: grad
  end type var

  interface var
     module procedure :: new_var_val
     module procedure :: new_var
     module procedure :: new_var_1d
     module procedure :: new_var_val_1d
     module procedure :: new_var_2d
     module procedure :: new_var_val_2d
     module procedure :: new_var_val_op
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

  function new_var_val(d) result(v)
    real(rk), intent(in) :: d
    type(var) :: v
    v%vi => vari(d)
  end function new_var_val

  function new_var() result(v)
    type(var) :: v
    v%vi => vari()
  end function new_var

  elemental integer(ik) function vari_index(this)
    type(var), intent(in) :: this
    vari_index = this%vi%i
  end function vari_index

  function new_var_val_op(d, op) result(v)
    real(rk), intent(in) :: d
    type(var) :: v
    type(var), intent(in) :: op(:)
    v%vi => vari(d, vari_index(op))
  end function new_var_val_op

  function new_var_val_dop(d, dop) result(v)
    real(rk), intent(in) :: d
    type(var) :: v
    real(rk), intent(in) :: dop(:)
    v%vi => vari(d, dop)
  end function new_var_val_dop

  function new_var_val_op_dop(d, op, dop) result(v)
    real(rk), intent(in) :: d
    type(var) :: v
    type(var), intent(in) :: op(:)
    real(rk), intent(in) :: dop(:)
    v%vi => vari(d, vari_index(op), dop)
  end function new_var_val_op_dop

  function new_var_val_1d(d) result(v)
    real(rk), intent(in) :: d(:)
    type(var) :: v(size(d))
    integer :: i
    do i = 1, size(d)
       v(i) = new_var_val(d(i))
    end do
  end function new_var_val_1d

  function new_var_1d(n) result(v)
    integer(ik), intent(in) :: n
    type(var) :: v(n)
    integer :: i
    do i = 1, n
       v(i) = new_var()
    end do
  end function new_var_1d

  function new_var_val_2d(d) result(v)
    real(rk), intent(in) :: d(:, :)
    type(var) :: v(size(d, 1), size(d, 2))
    integer :: i
    do i = 1, size(d, 2)
       v(:, i) = new_var_val_1d(d(:, i))
    end do
  end function new_var_val_2d

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
    call this%vi%set_val(from)
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
    val = this%vi%val()
  end function val

  elemental real(rk) function adj(this)
    class(var), intent(in) :: this
    adj = this%vi%adj()
  end function adj

  subroutine grad(this)
    class(var), intent(in) :: this
    integer i
    call this%vi%init_dependent()
    do i = callstack%head - 1, 1, -1
       call callstack%varis(i)%chain()
    end do
  end subroutine grad

end module var_mod
