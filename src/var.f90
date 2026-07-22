module fazang_var
  use, intrinsic :: iso_fortran_env
  use fazang_env
  use fazang_vari, vi_val => val, vi_adj => adj
  implicit none

  private
  public :: var, val, adj, grad
  public :: assignment(=), exp, sin

  type :: var
     integer(ik) :: i = 0       ! point to a vari in adstack
  end type var

  interface assignment(=)
     module procedure set_var_val
     module procedure set_var_real32
  end interface assignment(=)

  interface grad
     module procedure grad_v
     module procedure grad_all
  end interface grad

  interface exp
     module procedure exp_v
  end interface exp

  interface sin
     module procedure sin_v
  end interface sin



contains

  subroutine set_var_val(this, val)
    implicit none
    type(var), intent(out) :: this
    real(rk), intent(in) :: val
    type(vari) :: v
    v = val
    this%i = v%i
  end subroutine set_var_val

  subroutine set_var_real32(this, val)
    implicit none
    type(var), intent(out) :: this
    real(real32), intent(in) :: val
    type(vari) :: v
    v = val
    this%i = v%i
  end subroutine set_var_real32

  function exp_v(x) result(v)
    type(var), intent(in) :: x
    type(var) :: v
    v%i = exp_vi(x%i)
  end function exp_v

  function sin_v(x) result(v)
    type(var), intent(in) :: x
    type(var) :: v
    v%i = sin_vi(x%i)
  end function sin_v

  real(rk) function val(v)
    implicit none
    type(var), intent(in) :: v
    type(vari) :: vi
    vi%i = v%i
    val = vi_val(vi)
  end function val

  real(rk) function adj(v)
    implicit none
    type(var), intent(in) :: v
    type(vari) :: vi
    vi%i = v%i
    adj = vi_adj(vi)
  end function adj

  subroutine grad_v(v)
    type(var), intent(in) :: v
    call chain(v%i)
  end subroutine grad_v

  subroutine grad_all()
    call chain(core_adstack%j_)
  end subroutine grad_all

end module fazang_var
