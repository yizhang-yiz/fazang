module fazang_var
  use, intrinsic :: iso_fortran_env
  use fazang_env
  use fazang_vari
  implicit none

  private
  public :: var
  public :: assignment(=)
  public :: val
  public :: set_var_vari
  public :: exp

  type :: var
     class(vari), pointer :: vi
     integer(ik) :: i = 0
  end type var

  interface assignment(=)
     module procedure set_var_val
  end interface assignment(=)

contains

  subroutine set_var_val(this, val)
    implicit none
    type(var), intent(out) :: this
    real(rk), intent(in) :: val
    type(vari), target :: v
    v = val
    this%vi => v
    this%i = v%i
  end subroutine set_var_val

  function exp(x) result(v)
    type(var), intent(in) :: x
    type(var) :: v
    type(exp_v), pointer :: vi
    v%vi => null(vi)
    v%i = exp_vi(x%i)
  end function exp

  subroutine set_var_vari(this, vi, i)
    implicit none
    type(var), intent(out) :: this
    class(vari), pointer, intent(in) :: vi
    integer(ik), intent(in) :: i
    this%vi => null(vi)
    this%i = i
  end subroutine set_var_vari

  real(rk) function val(v)
    implicit none
    type(var), intent(in) :: v
    type(vari) :: vi
    vi%i = v%i
    val = vi%val()
  end function val

end module fazang_var
