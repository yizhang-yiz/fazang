#ifndef DEF_VARI_OP1_DEFINED
#define DEF_VARI_OP1_DEFINED

! OP: use vi as vari arg
#define DEF_OP1(NAME, OP, DYDX) \
function NAME/**/_vi (vi) result(v1); \
  implicit none; \
  type(vari), pointer :: vi, v1; \
  v1 = OP; \
  v1%chain = c_funloc( mychain ); \
  call core_adstack%push(vi%i); \
  contains; \
  subroutine mychain (this); \
    implicit none; \
    type(vari), pointer :: this, a; \
    call recover(a, core_adstack%pop_int(this%i+visize)); \
    a%adj_ = a%adj_ + this%adj_ * DYDX; \
  end subroutine mychain; \
end function

! OP = certain operation using (va%val_, vb%val_ , a, b ...)
#define DEF_OP2_VD(NAME, OP, DYDX) \
function NAME/**/_vi_d(va, b) result(v1); \
  implicit none; \
  type(vari), pointer, intent(in) :: va; \
  real(rk), intent(in) :: b; \
  type(vari), pointer :: v1; \
  v1 = OP; \
  v1%chain = c_funloc( mychain ); \
  call core_adstack%push(va%i); \
  call core_adstack%push(b); \
  contains; \
  subroutine mychain (this); \
    implicit none; \
    type(vari), pointer :: this, va; \
    real(rk) :: b; \
    call recover(va, core_adstack%pop_int(this%i+visize)); \
    b = core_adstack%pop_real(this%i+visize+iksize); \
    va%adj_ = va%adj_ + this%adj_ * DYDX; \
  end subroutine mychain; \
end function

#define DEF_OP2_DV(NAME, OP, DYDX) \
function NAME/**/_d_vi(a, vb) result(v1); \
  implicit none; \
  type(vari), pointer, intent(in) :: vb; \
  real(rk), intent(in) :: a; \
  type(vari), pointer :: v1; \
  v1 = OP; \
  v1%chain = c_funloc( mychain ); \
  call core_adstack%push(vb%i); \
  call core_adstack%push(a); \
  contains; \
  subroutine mychain (this); \
    implicit none; \
    type(vari), pointer :: this, vb; \
    real(rk) :: a; \
    call recover(vb, core_adstack%pop_int(this%i+visize)); \
    a = core_adstack%pop_real(this%i+visize+iksize); \
    vb%adj_ = vb%adj_ + this%adj_ * DYDX; \
  end subroutine mychain; \
end function

#define DEF_OP2_VV(NAME, OP, DYDA, DYDB) \
function NAME/**/_vi_vi(va, vb) result(v1); \
  implicit none; \
  type(vari), pointer, intent(in) :: va, vb; \
  type(vari), pointer :: v1; \
  v1 = OP; \
  v1%chain = c_funloc( mychain ); \
  call core_adstack%push(va%i); \
  call core_adstack%push(vb%i); \
  contains; \
  subroutine mychain (this); \
    implicit none; \
    type(vari), pointer :: this, va, vb; \
    call recover(va, core_adstack%pop_int(this%i+visize)); \
    call recover(vb, core_adstack%pop_int(this%i+visize+iksize)); \
    va%adj_ = va%adj_ + this%adj_ * DYDA; \
    vb%adj_ = vb%adj_ + this%adj_ * DYDB; \
  end subroutine mychain; \
end function

#endif

module fz_vari
  use, intrinsic :: iso_fortran_env
  use fz_env
  use, intrinsic :: iso_c_binding

  type, bind(c) :: vari
     real(rk) :: val_
     real(rk) :: adj_ = 0d0
     integer(ik) :: i = 0 ! this vari location in storage
     integer(ik) :: j = 0 ! vari before i in storage (for rev pass)
     type(c_funptr) :: chain = c_null_funptr
  end type vari

  integer(ik), parameter :: visize = storage_size(vari(0.d0))/8

  interface assignment(=)
     module procedure new_vari_val
     module procedure new_vari_real32
  end interface assignment(=)

  abstract interface
     subroutine chain_op(p)
       import
       implicit none
       type(vari), pointer, intent(in) :: p
     end subroutine chain_op

     real(rk) function op1(x)
       use fz_env
       real(rk), intent(in) :: x
     end function op1

     real(rk) function op2(a, b)
       use fz_env
       real(rk), intent(in) :: a, b
     end function op2
  end interface

contains

  subroutine recover(p, i)
    type(vari), pointer, intent(out) :: p
    integer(ik), intent(in) :: i
    type(c_ptr) :: cp
    p => null()
    if (i > 0) then
       cp = c_loc(core_adstack%s_(i))
       call c_f_pointer(cp, p)
    end if
  end subroutine recover

  elemental real(rk) function val(this)
    type(vari), intent(in) :: this
    val = this%val_
  end function val

  elemental real(rk) function adj(this)
    type(vari), intent(in) :: this
    adj = this%adj_
  end function adj

  subroutine new_vari_val(this, val)
    implicit none
    type(vari), pointer, intent(out) :: this
    real(rk), intent(in) :: val
    call recover(this, core_adstack%i_)
    this%val_ = val
    this%adj_ = 0.d0
    this%i = core_adstack%i_
    this%j = core_adstack%j_
    this%chain = c_funloc(chain_dummy)
    call core_adstack%incr(visize, .true.)
  end subroutine new_vari_val

  subroutine new_vari_real32(this, val)
    implicit none
    type(vari), pointer, intent(out) :: this
    real(c_float), intent(in) :: val
    call recover(this, core_adstack%i_)
    this%val_ = val
    this%adj_ = 0.d0
    this%chain = c_funloc(chain_dummy)
    this%i = core_adstack%i_
    this%j = core_adstack%j_
    call core_adstack%incr(visize, .true.)
  end subroutine new_vari_real32

  ! skip chain and return previous vi in AD stack
  subroutine chain_dummy(p)
    implicit none
    type(vari), pointer :: p
  end subroutine

  subroutine reset_chain(p)
    implicit none
    type(vari), pointer, intent(in) :: p
    type(vari), pointer :: p1
    p1 => p
    do while (associated(p1))
       p1%adj_ = 0.d0
       call recover(p1, p1%j)
    enddo
  end subroutine reset_chain

  subroutine chain(p)
    implicit none
    type(vari), pointer, intent(in) :: p
    type(vari), pointer :: p1
    procedure(chain_op), pointer :: p1_chain
    p%adj_ = 1.0d0
    p1 => p
    do while (associated(p1))
       call c_f_procpointer(p1%chain, p1_chain)
       call p1_chain(p1)
       call recover(p1, p1%j)
    enddo
  end subroutine chain

  DEF_OP1(exp, dexp(vi%val_), (this%val_))

  DEF_OP1(sin, dsin(vi%val_), (cos(a%val_)))

  DEF_OP1(cos, dcos(vi%val_), (-sin(a%val_)))

  DEF_OP1(tan, dtan(vi%val_), (1.d0/(cos(a%val_)*cos(a%val_))))

  DEF_OP1(asin, dasin(vi%val_), (1.d0/sqrt(1.d0-a%val_*a%val_)))

  DEF_OP1(acos, dacos(vi%val_), (-1.d0/sqrt(1.d0-a%val_*a%val_)))

  DEF_OP1(atan, datan(vi%val_), (1.d0/(1.d0+a%val_*a%val_)))

  DEF_OP1(log, dlog(vi%val_), (1.d0/a%val_))

  DEF_OP1(log10, dlog10(vi%val_), (1.d0/(a%val_*dlog(10.d0))))

  DEF_OP1(sqrt, dsqrt(vi%val_), (0.5d0/dsqrt(a%val_)))

  DEF_OP1(neg, (-vi%val_), (-1.d0))

  DEF_OP1(pos, (vi%val_), (1.d0))

  DEF_OP1(sinh, dsinh(vi%val_), (dcosh(a%val_)))

  DEF_OP1(cosh, dcosh(vi%val_), (dsinh(a%val_)))

  DEF_OP1(tanh, dtanh(vi%val_), (1.d0/(dcosh(a%val_)*dcosh(a%val_))) )

  elemental real(rk) function logit_d(d)
    implicit none
    real(rk), intent(in) :: d
    logit_d = log(d / (1.d0 - d))
  end function logit_d
  DEF_OP1(logit, logit_d(vi%val_), (1.d0 / (a%val_ - a%val_ * a%val_)) )

  elemental function inv_logit_d(d) result(s)
    use fz_constants, only : log_eps
    implicit none
    real(rk), intent(in) :: d
    real(rk) :: s, exp_d
    if ( d < 0.d0 ) then
       exp_d = exp(d)
       if (d < log_eps) then
          s = exp_d
       else
          s = exp_d / (1.d0 + exp_d);
       endif
    else
       s = 1.d0/(1.d0 + exp(-d))
    endif
  end function inv_logit_d
  DEF_OP1(inv_logit, inv_logit_d(vi%val_), (this%val_ * (1.d0 - this%val_)) )

  DEF_OP2_VD(add, (va%val_ + b), (1.d0))
  DEF_OP2_DV(add, (vb%val_ + a), (1.d0))
  DEF_OP2_VV(add, (vb%val_ + va%val_), (1.d0), (1.d0))

  DEF_OP2_VD(sub, (va%val_ - b), (1.d0))
  DEF_OP2_DV(sub, (a - vb%val_), (-1.d0))
  DEF_OP2_VV(sub, (va%val_ - vb%val_), (1.d0), (-1.d0))

  DEF_OP2_VD(mul, (va%val_ * b), (b))
  DEF_OP2_DV(mul, (a * vb%val_), (a))
  DEF_OP2_VV(mul, (va%val_ * vb%val_), (vb%val_), (va%val_))

  DEF_OP2_VD(div, (va%val_/b), (1.d0/b))
  DEF_OP2_DV(div, (a/vb%val_), (-this%val_/vb%val_))
  DEF_OP2_VV(div, (va%val_/vb%val_), (1.d0/vb%val_), (-this%val_/vb%val_))

  subroutine chain_sum (this)
    implicit none
    type(vari), pointer, intent(in) :: this
    type(vari), pointer :: va
    integer(ik) :: i, j, n
    n = core_adstack%pop_int(this%i + visize)
    do i = 1, n
       call recover(va, core_adstack%pop_int(this%i + visize + i*iksize))
       va%adj_ = va%adj_ + this%adj_
    enddo
  end subroutine chain_sum

end module fz_vari
