#ifndef DEF_VAR_OP_DEFINED
#define DEF_VAR_OP_DEFINED

#define DEF_OP1( NAME ) \
impure elemental function NAME/**/_v(x) result(v); \
    use NAME/**/_fvari_mod; \
    implicit none; \
    type(fvar), intent(in) :: x; \
    type(fvar) :: v; \
    v%i = new_vi(x%i); \
  end function

#define DEF_OP2( NAME ) \
impure elemental function NAME/**/_vd(x, b) result(v); \
    use NAME/**/_fvari_mod; \
    implicit none; \
    type(fvar), intent(in) :: x; \
    real(rk), intent(in) :: b; \
    type(fvar) :: v; \
    v%i = new_vi_d(x%i, b); \
  end function; \
impure elemental function NAME/**/_dv(b, x) result(v); \
    use NAME/**/_fvari_mod; \
    implicit none; \
    type(fvar), intent(in) :: x; \
    real(rk), intent(in) :: b; \
    type(fvar) :: v; \
    v%i = new_d_vi(b, x%i); \
  end function; \
impure elemental function NAME/**/_vv(x, y) result(v); \
    use NAME/**/_fvari_mod; \
    implicit none; \
    type(fvar), intent(in) :: x, y; \
    type(fvar) :: v; \
    v%i = new_vi_vi(x%i, y%i); \
  end function

  ! loglik function with two params
#define DEF_OP2D( NAME ) \
impure elemental function NAME/**/_vd(x, y, d) result(v); \
    use NAME/**/_fvari_mod; \
    implicit none; \
    type(fvar), intent(in) :: x; \
    real(rk), intent(in) :: y, d; \
    type(fvar) :: v; \
    v%i = new_vi_d(x%i, y, d); \
  end function; \
impure elemental function NAME/**/_dv(x, y, d) result(v); \
    use NAME/**/_fvari_mod; \
    implicit none; \
    type(fvar), intent(in) :: y; \
    real(rk), intent(in) :: x, d; \
    type(fvar) :: v; \
    v%i = new_d_vi(x, y%i, d); \
  end function; \
impure elemental function NAME/**/_vv(x, y, d) result(v); \
    use NAME/**/_fvari_mod; \
    implicit none; \
    type(fvar), intent(in) :: x, y; \
    real(rk), intent(in) :: d; \
    type(fvar) :: v; \
    v%i = new_vi_vi(x%i, y%i, d); \
  end function

#endif

module fz_fvar
  use, intrinsic :: iso_fortran_env
  use fz_env
  use fz_fvari
  use fz_prim_op
  implicit none

  ! private
  ! public :: fvar, val, adj, grad, reset_adj, assignment(=)
  ! public :: operator(+), operator(-), operator(*), operator(/)
  ! public :: reboot_chain

  type :: fvar
     integer(ik) :: i ! point to a fvari in adstack
  end type fvar

  interface assignment(=)
     module procedure new_fvar_val
     module procedure new_fvar_real32
     module procedure set_fvar
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

  interface reset_deriv
     module procedure reset_from
     module procedure reset_all_deriv
  end interface reset_deriv

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
  ! interface sinh; module procedure sinh_v; end interface
  ! interface cosh; module procedure cosh_v; end interface
  ! interface tanh; module procedure tanh_v; end interface

  ! interface logit
  !    module procedure logit_d
  !    module procedure logit_v
  ! end interface logit
  ! public :: logit

  ! interface inv_logit
  !    module procedure inv_logit_d
  !    module procedure inv_logit_v
  ! end interface inv_logit
  ! public :: inv_logit

  ! vec op
  ! interface sum; module procedure sum_v; end interface

  ! forward vector operator
  abstract interface
     type(fvar) function fvar_op(x)
       import :: fvar
       type(fvar), intent(in) :: x(:)
     end function fvar_op
  end interface

contains

  impure elemental subroutine new_fvar_val(this, val)
    implicit none
    type(fvar), intent(out) :: this
    real(rk), intent(in) :: val
    call new_vari(this%i, val)
  end subroutine new_fvar_val

  impure subroutine new_fvar_real32(this, val)
    implicit none
    type(fvar), intent(out) :: this
    real(real32), intent(in) :: val
    call new_fvar_val(this, real(val, rk))
  end subroutine new_fvar_real32

  elemental subroutine set_fvar(this, that)
    implicit none
    type(fvar), intent(out) :: this
    type(fvar), intent(in) :: that
    this%i = that%i
  end subroutine set_fvar

  elemental integer(ik) function index(this)
    implicit none
    type(fvar), intent(in) :: this
    index = chains(this%i)%i
  end function index

  impure elemental function val(v) result(v1)
    implicit none
    type(fvar), intent(in) :: v
    real(rk) :: v1
    v1 = vari_val_v_at(index(v))
  end function val

  impure elemental function val_dv(v) result(v1)
    implicit none
    type(fvar), intent(in) :: v
    real(rk) :: v1
    v1 = vi_val_dv(index(v))
  end function val_dv

  impure elemental function adj(v) result(v1)
    implicit none
    type(fvar), intent(in) :: v
    real(rk) :: v1
    v1 = vi_adj_v(chains(v%i)%i)
  end function adj

  impure elemental function adj_dv(v) result(v1)
    implicit none
    type(fvar), intent(in) :: v
    real(rk) :: v1
    v1 = vi_adj_dv(chains(v%i)%i)
  end function adj_dv

  subroutine init_deriv(v)
    implicit none
    type(fvar), intent(in) :: v
    type(vari), pointer :: p
    call recover(chains(v%i)%i, p)
    p%val_%dv = 1.d0
  end subroutine init_deriv

  ! gradience of v, hessian wrt to v1
  subroutine deriv(v)
    implicit none
    type(fvar), intent(in) :: v
    call chain(v%i)
  end subroutine deriv

  subroutine reset_from(v)
    implicit none
    type(fvar), intent(in) :: v
    call reset_chain(v%i)
  end subroutine reset_from

  subroutine reset_all_deriv()
    implicit none
    call reset_chain(core_adstack%nvari)
  end subroutine reset_all_deriv

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

  ! DEF_OP1(sinh)

  ! DEF_OP1(cosh)

  ! DEF_OP1(tanh)

  ! DEF_OP1(logit)

  ! DEF_OP1(inv_logit)

  ! OP2
  DEF_OP2(add)
  DEF_OP2(sub)
  DEF_OP2(mul)
  DEF_OP2(div)

  ! vec op
  ! function sum_v(x) result(v)
  !   implicit none
  !   type(fvar), intent(in) :: x(:)
  !   type(fvar) :: v
  !   integer(ik) :: i, j
  !   v%p = sum(val(x))
  !   v%p%chain = c_funloc(chain_sum)
  !   call core_adstack%push(size(x))
  !   do i = 1, size(x)
  !      call core_adstack%push(x(i)%p%i)
  !   end do
  ! end function sum_v

  ! hessian-vector product
  function hvp(f, x, v) result(res)
    implicit none
    procedure(fvar_op) :: f
    real(rk), intent(in) :: x(:), v(:)
    real(rk) :: res(size(x))
    type(fvar) :: vx(size(x)), a, y

    a = 0.d0
    call init_deriv(a)
    vx = x + a*v(1:size(x))
    y = f(vx)
    call deriv(y)
    res = adj_dv(vx)
  end function hvp

end module fz_fvar
