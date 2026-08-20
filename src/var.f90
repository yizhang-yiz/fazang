#ifndef DEF_VAR_OP_DEFINED
#define DEF_VAR_OP_DEFINED

#define DEF_OP1( NAME ) \
impure elemental function NAME/**/_v(x) result(v); \
    use NAME/**/_vi_mod; \
    implicit none; \
    type(var), intent(in) :: x; \
    type(var) :: v; \
    v%i = new_vi(x%i); \
  end function

#define DEF_OP2( NAME ) \
impure elemental function NAME/**/_vd(x, b) result(v); \
    use NAME/**/_vi_mod; \
    implicit none; \
    type(var), intent(in) :: x; \
    real(rk), intent(in) :: b; \
    type(var) :: v; \
    v%i = new_vi_d(x%i, b); \
  end function; \
impure elemental function NAME/**/_dv(b, x) result(v); \
    use NAME/**/_vi_mod; \
    implicit none; \
    type(var), intent(in) :: x; \
    real(rk), intent(in) :: b; \
    type(var) :: v; \
    v%i = new_d_vi(b, x%i); \
  end function; \
impure elemental function NAME/**/_vv(x, y) result(v); \
    use NAME/**/_vi_mod; \
    implicit none; \
    type(var), intent(in) :: x, y; \
    type(var) :: v; \
    v%i = new_vi_vi(x%i, y%i); \
  end function

  ! loglik function with two params
#define DEF_OP2D( NAME ) \
impure elemental function NAME/**/_vd(x, y, d) result(v); \
    use NAME/**/_vi_mod; \
    implicit none; \
    type(var), intent(in) :: x; \
    real(rk), intent(in) :: y, d; \
    type(var) :: v; \
    v%i = new_vi_d(x%i, y, d); \
  end function; \
impure elemental function NAME/**/_dv(x, y, d) result(v); \
    use NAME/**/_vi_mod; \
    implicit none; \
    type(var), intent(in) :: y; \
    real(rk), intent(in) :: x, d; \
    type(var) :: v; \
    v%i = new_d_vi(x, y%i, d); \
  end function; \
impure elemental function NAME/**/_vv(x, y, d) result(v); \
    use NAME/**/_vi_mod; \
    implicit none; \
    type(var), intent(in) :: x, y; \
    real(rk), intent(in) :: d; \
    type(var) :: v; \
    v%i = new_vi_vi(x%i, y%i, d); \
  end function

#endif

module fz_var
  use, intrinsic :: iso_fortran_env
  use fz_env
  use fz_vari
  use fz_prim_op
  ! use fz_vari_prob
  implicit none

  type :: var
     integer(ik) :: i ! point to a vari in adstack
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
  interface square; module procedure square_v; end interface

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

  ! loglik
  interface normal_lpdf
     module procedure normal_lpdf_vd
     module procedure normal_lpdf_dv
     module procedure normal_lpdf_vv
     module procedure normal_lpdf_d_d_d
  end interface

  interface lognormal_lpdf
     module procedure lognormal_lpdf_vd
     module procedure lognormal_lpdf_dv
     module procedure lognormal_lpdf_vv
     module procedure lognormal_lpdf_d_d_d
  end interface

  interface weibull_lpdf
     module procedure weibull_lpdf_vd
     module procedure weibull_lpdf_dv
     module procedure weibull_lpdf_vv
     module procedure weibull_lpdf_d_d_d
  end interface

  interface cauchy_lpdf
     module procedure cauchy_lpdf_vd
     module procedure cauchy_lpdf_dv
     module procedure cauchy_lpdf_vv
     module procedure cauchy_lpdf_d_d_d
  end interface

  interface gumbel_lpdf
     module procedure gumbel_lpdf_vd
     module procedure gumbel_lpdf_dv
     module procedure gumbel_lpdf_vv
     module procedure gumbel_lpdf_d_d_d
  end interface

  interface laplace_lpdf
     module procedure laplace_lpdf_vd
     module procedure laplace_lpdf_dv
     module procedure laplace_lpdf_vv
     module procedure laplace_lpdf_d_d_d
  end interface laplace_lpdf

  interface logistic_lpdf
     module procedure logistic_lpdf_vd
     module procedure logistic_lpdf_dv
     module procedure logistic_lpdf_vv
     module procedure logistic_lpdf_d_d_d
  end interface

  interface chi_square_lpdf
     module procedure chi_square_lpdf_v
     module procedure chi_square_lpdf_d_d
  end interface

  interface inv_chi_square_lpdf
     module procedure inv_chi_square_lpdf_v
     module procedure inv_chi_square_lpdf_d_d
  end interface

contains

  impure elemental subroutine new_var_val(this, val)
    implicit none
    type(var), intent(out) :: this
    real(rk), intent(in) :: val
    call new_vari(this%i, val)
  end subroutine new_var_val

  impure subroutine new_var_real32(this, val)
    implicit none
    type(var), intent(out) :: this
    real(real32), intent(in) :: val
    call new_var_val(this, real(val, rk))
  end subroutine new_var_real32

  impure subroutine set_var(this, that)
    implicit none
    type(var), intent(out) :: this
    type(var), intent(in) :: that
    this%i = that%i
  end subroutine set_var

  elemental integer(ik) function index(this)
    implicit none
    type(var), intent(in) :: this
    index = chains(this%i)%i
  end function index

  impure elemental real(rk) function val(v)
    implicit none
    type(var), intent(in) :: v
    val = vi_val(chains(v%i)%i)
  end function val

  impure elemental real(rk) function adj(v)
    implicit none
    type(var), intent(in) :: v
    adj = vi_adj(chains(v%i)%i)
  end function adj

  subroutine grad_of(v)
    implicit none
    type(var), intent(in) :: v
    call chain(v%i)
  end subroutine grad_of

  subroutine grad_all()
    implicit none
    type(vari), pointer :: p
    call chain(core_adstack%nvari)
  end subroutine grad_all

  subroutine reset_adj_from(v)
    implicit none
    type(var), intent(in) :: v
    call reset_chain(v%i)
  end subroutine reset_adj_from

  subroutine reset_all_adj()
    implicit none
    type(vari), pointer :: p
    call reset_chain(core_adstack%nvari)
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
  DEF_OP1(square)

  ! OP2
  DEF_OP2(add)
  DEF_OP2(sub)
  DEF_OP2(mul)
  DEF_OP2(div)
  DEF_OP2(pow)

  ! vec op
  function sum_v(x) result(v)
    use sum_vi_mod
    implicit none
    type(var), intent(in) :: x(:)
    type(var) :: v
    call new_vari(v%i, sum(val(x)), index(x))
    chains(v%i)%c => vi_chain_instance
  end function sum_v

  ! loglik
  DEF_OP2D(normal_lpdf)
  DEF_OP2D(lognormal_lpdf)
  DEF_OP2D(weibull_lpdf)
  DEF_OP2D(cauchy_lpdf)
  DEF_OP2D(gumbel_lpdf)
  DEF_OP2D(laplace_lpdf)
  DEF_OP2D(logistic_lpdf)

  impure elemental function chi_square_lpdf_v(x, d) result(v)
    use chi_square_lpdf_vi_mod
    implicit none;
    type(var), intent(in) :: x
    real(rk), intent(in) :: d
    type(var) :: v
    v%i = new_vi(x%i, d)
  end function chi_square_lpdf_v

  impure elemental function inv_chi_square_lpdf_v(x, d) result(v)
    use inv_chi_square_lpdf_vi_mod
    implicit none;
    type(var), intent(in) :: x
    real(rk), intent(in) :: d
    type(var) :: v
    v%i = new_vi(x%i, d)
  end function

end module fz_var
