#ifndef DEF_VAR_OP_DEFINED
#define DEF_VAR_OP_DEFINED

#define DEF_OP1( NAME ) \
  function NAME/**/_v(x) result(v); \
    implicit none; \
    type(var), intent(in) :: x; \
    type(var) :: v; \
    v%p => NAME/**/_vi(x%p); \
  end function NAME/**/_v

#define DEF_OP2( NAME ) \
  function NAME/**/_vd(x, b) result(v); \
    implicit none; \
    type(var), intent(in) :: x; \
    real(rk), intent(in) :: b; \
    type(var) :: v; \
    v%p => NAME/**/_vi_d(x%p, b); \
  end function NAME/**/_vd; \
  function NAME/**/_dv(b, x) result(v); \
    implicit none; \
    type(var), intent(in) :: x; \
    real(rk), intent(in) :: b; \
    type(var) :: v; \
    v%p => NAME/**/_d_vi(b, x%p); \
  end function NAME/**/_dv; \
  function NAME/**/_vv(x, y) result(v); \
    implicit none; \
    type(var), intent(in) :: x, y; \
    type(var) :: v; \
    v%p => NAME/**/_vi_vi(x%p, y%p); \
  end function NAME/**/_vv;

#endif

module fz_var
  use, intrinsic :: iso_fortran_env
  use fz_env
  use fz_vari
  use fz_vari_op
  implicit none

  type :: var
     type(vari), pointer :: p => null() ! point to a vari in adstack
  end type var

  interface assignment(=)
     module procedure new_var_val
     module procedure new_var_real32
     module procedure set_var
  end interface assignment(=)

  interface operator(+)
     module procedure add_dv
     module procedure add_vd
     module procedure add_vv
     module procedure pos_v
  end interface operator(+)

  interface operator(-)
     module procedure sub_dv
     module procedure sub_vd
     module procedure sub_vv
     module procedure neg_v
  end interface operator(-)

  interface operator(*)
     module procedure mul_dv
     module procedure mul_vd
     module procedure mul_vv
  end interface operator(*)

  interface operator(/)
     module procedure div_dv
     module procedure div_vd
     module procedure div_vv
  end interface operator(/)

  interface operator(**)
     module procedure pow_dv
     module procedure pow_vd
     module procedure pow_vv
  end interface operator(**)

  interface grad
     module procedure grad_of
     module procedure grad_all
  end interface grad

  interface reset_adj
     module procedure reset_adj_from
     module procedure reset_all_adj
  end interface reset_adj

  interface exp; module procedure exp_v; end interface
  interface log; module procedure log_v; end interface
  interface log10; module procedure log10_v; end interface
  interface sin; module procedure sin_v; end interface
  interface cos; module procedure cos_v; end interface
  interface tan; module procedure tan_v; end interface
  interface asin; module procedure asin_v; end interface
  interface acos; module procedure acos_v; end interface
  interface atan; module procedure atan_v; end interface
  interface sqrt; module procedure sqrt_v; end interface
  interface sinh; module procedure sinh_v; end interface
  interface cosh; module procedure cosh_v; end interface
  interface tanh; module procedure tanh_v; end interface

  interface logit
     module procedure logit_d
     module procedure logit_v
  end interface logit

  interface inv_logit
     module procedure inv_logit_d
     module procedure inv_logit_v
  end interface inv_logit

  ! vec op
  interface sum; module procedure sum_v; end interface

contains

  impure elemental subroutine new_var_val(this, val)
    implicit none
    type(var), intent(out) :: this
    real(rk), intent(in) :: val
    type(vari), pointer :: v
    v = val
    this%p => v
  end subroutine new_var_val

  impure subroutine new_var_real32(this, val)
    implicit none
    type(var), intent(out) :: this
    real(real32), intent(in) :: val
    type(vari), pointer :: v
    v = val
    this%p => v
  end subroutine new_var_real32

  impure subroutine set_var(this, that)
    implicit none
    type(var), intent(out) :: this
    type(var), intent(in) :: that
    this%p => that%p
  end subroutine set_var

  elemental real(rk) function val(v)
    implicit none
    type(var), intent(in) :: v
    val = vi_val(v%p)
  end function val

  elemental real(rk) function adj(v)
    implicit none
    type(var), intent(in) :: v
    adj = vi_adj(v%p)
  end function adj

  subroutine grad_of(v)
    implicit none
    type(var), intent(in) :: v
    call chain(v%p)
  end subroutine grad_of

  subroutine grad_all()
    implicit none
    type(vari), pointer :: p
    call recover(p, core_adstack%j_)
    call chain(p)
  end subroutine grad_all

  subroutine reset_adj_from(v)
    implicit none
    type(var), intent(in) :: v
    call reset_chain(v%p)
  end subroutine reset_adj_from

  subroutine reset_all_adj()
    implicit none
    type(vari), pointer :: p
    call recover(p, core_adstack%j_)
    call reset_chain(p)
  end subroutine reset_all_adj

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
  DEF_OP1(logit)
  DEF_OP1(inv_logit)

  ! OP2
  DEF_OP2(add)
  DEF_OP2(sub)
  DEF_OP2(mul)
  DEF_OP2(div)
  DEF_OP2(pow)

  ! vec op
  function sum_v(x) result(v)
    implicit none
    type(var), intent(in) :: x(:)
    type(var) :: v
    integer(ik) :: i, j
    v%p = sum(val(x))
    v%p%chain = c_funloc(chain_sum)
    call core_adstack%push(size(x))
    do i = 1, size(x)
       call core_adstack%push(x(i)%p%i)
    end do
  end function sum_v

end module fz_var
