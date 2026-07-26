#ifndef DEF_VAR_OP_DEFINED
#define DEF_VAR_OP_DEFINED

#define DEF_OP1( NAME ) \
  function NAME/**/_v(x) result(v); \
    implicit none; \
    type(var), intent(in) :: x; \
    type(var) :: v; \
    v%i = NAME/**/_vi(x%i); \
  end function NAME/**/_v

#define DEF_INTERFACE( NAME ) \
  interface NAME; \
     module procedure NAME/**/_v ; \
  end interface NAME; \
  public :: NAME

#define DEF_OP2( NAME ) \
  function NAME/**/_vd(x, b) result(v); \
    implicit none; \
    type(var), intent(in) :: x; \
    real(rk), intent(in) :: b; \
    type(var) :: v; \
    v%i = NAME/**/_vi_d(x%i, b); \
  end function NAME/**/_vd; \
  function NAME/**/_dv(b, x) result(v); \
    implicit none; \
    type(var), intent(in) :: x; \
    real(rk), intent(in) :: b; \
    type(var) :: v; \
    v%i = NAME/**/_d_vi(b, x%i); \
  end function NAME/**/_dv; \
  function NAME/**/_vv(x, y) result(v); \
    implicit none; \
    type(var), intent(in) :: x, y; \
    type(var) :: v; \
    v%i = NAME/**/_vi_vi(x%i, y%i); \
  end function NAME/**/_vv;

#endif

module fz_var
  use, intrinsic :: iso_fortran_env
  use fz_env
  use fz_vari, vi_val => val, vi_adj => adj
  implicit none

  private
  public :: var, val, adj, id, grad, reset, assignment(=)
  public :: operator(+), operator(-), operator(*), operator(/)

  type :: var
     integer(ik) :: i = 0       ! point to a vari in adstack
  end type var

  interface assignment(=)
     module procedure set_var_val
     module procedure set_var_real32
  end interface assignment(=)

  interface operator(+)
     module procedure add_dv
     module procedure add_vd
     module procedure add_vv
     module procedure pos_v
  end interface operator(+)

  interface operator(-)
     module procedure substract_dv
     module procedure substract_vd
     module procedure substract_vv
     module procedure neg_v
  end interface operator(-)

  interface operator(*)
     module procedure multiply_dv
     module procedure multiply_vd
     module procedure multiply_vv
  end interface operator(*)

  interface operator(/)
     module procedure divide_dv
     module procedure divide_vd
     module procedure divide_vv
  end interface operator(/)

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

  DEF_INTERFACE(sinh)

  DEF_INTERFACE(cosh)

  DEF_INTERFACE(tanh)

  ! vec op
  DEF_INTERFACE(sum)

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

  elemental real(rk) function val(v)
    implicit none
    type(var), intent(in) :: v
    type(vari) :: vi
    vi%i = v%i
    val = vi_val(vi)
  end function val

  elemental real(rk) function adj(v)
    implicit none
    type(var), intent(in) :: v
    type(vari) :: vi
    vi%i = v%i
    adj = vi_adj(vi)
  end function adj

  elemental integer(ik) function id(v)
    implicit none
    type(var), intent(in) :: v
    id = v%i
  end function id

  subroutine grad_of(v)
    implicit none
    type(var), intent(in) :: v
    call chain(v%i)
  end subroutine grad_of

  subroutine grad_all()
    implicit none
    call chain(core_adstack%j_)
  end subroutine grad_all

  subroutine reset_from(v)
    implicit none
    type(var), intent(in) :: v
    call reset_chain(v%i)
  end subroutine reset_from

  subroutine reset_all()
    implicit none
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

  DEF_OP1(neg)

  DEF_OP1(pos)

  DEF_OP1(sinh)

  DEF_OP1(cosh)

  DEF_OP1(tanh)

  ! OP2
  DEF_OP2(add)

  DEF_OP2(substract)

  DEF_OP2(multiply)

  DEF_OP2(divide)

  ! vec op
  function sum_v(x) result(v)
    implicit none
    type(var), intent(in) :: x(:)
    type(var) :: v
    v%i = sum_vi(id(x))
  end function sum_v

end module fz_var
