#ifndef DEF_VAR_OP_DEFINED
#define DEF_VAR_OP_DEFINED

#define DEF_OP1( NAME ) \
  function NAME/**/_v(x) result(v); \
    type(var), intent(in) :: x; \
    type(var) :: v; \
    v%i = NAME/**/_vi(x%i); \
  end function NAME/**/_v

#define DEF_INTERFACE( NAME ) \
  interface NAME; \
     module procedure NAME/**/_v ; \
  end interface NAME; \
  public :: NAME

#endif

module fazang_var
  use, intrinsic :: iso_fortran_env
  use fazang_env
  use fazang_vari, vi_val => val, vi_adj => adj
  implicit none

  private
  public :: var, val, adj, grad, reset, assignment(=)

  type :: var
     integer(ik) :: i = 0       ! point to a vari in adstack
  end type var

  interface assignment(=)
     module procedure set_var_val
     module procedure set_var_real32
  end interface assignment(=)

  interface grad
     module procedure grad_of
     module procedure grad_all
  end interface grad

  interface reset
     module procedure reset_from
     module procedure reset_all
  end interface reset

  DEF_INTERFACE(exp)

  DEF_INTERFACE(sin)

  DEF_INTERFACE(cos)

  DEF_INTERFACE(tan)

  DEF_INTERFACE(asin)

  DEF_INTERFACE(acos)

  DEF_INTERFACE(atan)

  DEF_INTERFACE(log)

  DEF_INTERFACE(log10)

  DEF_INTERFACE(sqrt)

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

  subroutine grad_of(v)
    type(var), intent(in) :: v
    call chain(v%i)
  end subroutine grad_of

  subroutine grad_all()
    call chain(core_adstack%j_)
  end subroutine grad_all

  subroutine reset_from(v)
    type(var), intent(in) :: v
    call reset_chain(v%i)
  end subroutine reset_from

  subroutine reset_all()
    call reset_chain(core_adstack%j_)
  end subroutine reset_all

  DEF_OP1(exp)

  DEF_OP1(sin)

  DEF_OP1(cos)

  DEF_OP1(tan)

  DEF_OP1(asin)

  DEF_OP1(acos)

  DEF_OP1(atan)

  DEF_OP1(log)

  DEF_OP1(log10)

  DEF_OP1(sqrt)


end module fazang_var
