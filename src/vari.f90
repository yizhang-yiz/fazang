#ifndef DEF_VARI_OP1_DEFINED
#define DEF_VARI_OP1_DEFINED

#define DEF_OP1(FUNC, NAME, CHAIN) \
  integer(ik) function NAME (ia); \
    implicit none; \
    integer(ik), intent(in) :: ia; \
    procedure(op1) :: op; \
    procedure(chain_op) :: chain_op1; \
    type(vari) :: vi, v1; \
    call recover(vi, ia); \
    v1%val_ = FUNC(vi%val_); \
    v1%chain => CHAIN; \
    call push(v1); \
    call core_adstack%push(ia); \
    NAME = v1%i; \
  end function NAME

#define DEF_CHAIN_OP1(NAME, DYDX) \
  integer(ik) function NAME (i); \
    implicit none; \
    integer(ik), intent(in) :: i; \
    type(vari) :: this, a; \
    call recover(this, i); \
    call recover_op1_v(a, i); \
    a%adj_ = a%adj_ + this%adj_ * DYDX; \
    call push(a); \
    NAME = this%j; \
  end function NAME

#define DEF_CHAIN_OP2_VD(NAME, DYDX) \
  integer(ik) function NAME (i); \
    implicit none; \
    integer(ik), intent(in) :: i; \
    type(vari) :: this, a; \
    real(rk) :: b; \
    call recover(this, i); \
    call recover_op2_v1(a, i); \
    call recover_op2_b(b ,i); \
    a%adj_ = a%adj_ + this%adj_ * DYDX; \
    call push(a); \
    NAME = this%j; \
  end function NAME

#define DEF_CHAIN_OP2_VV(NAME, DYDA, DYDB) \
  integer(ik) function NAME (i); \
    implicit none; \
    integer(ik), intent(in) :: i; \
    type(vari) :: this, a, b; \
    call recover(this, i); \
    call recover_op2_v1(a, i); \
    call recover_op2_v2(b, i); \
    a%adj_ = a%adj_ + this%adj_ * DYDA; \
    b%adj_ = b%adj_ + this%adj_ * DYDB; \
    call push(a); \
    call push(b); \
    NAME = this%j; \
  end function NAME

#define DEF_OP2(FUNC, NAMEvd, CHAINvd, NAMEdv, CHAINdv, NAMEvv, CHAINvv) \
  integer(ik) function NAMEvd(i, b); \
    integer(ik), intent(in) :: i; \
    real(rk), intent(in) :: b; \
    NAMEvd = op2_vd(i, b, FUNC, CHAINvd, .false., .true.); \
  end function NAMEvd; \
  integer(ik) function NAMEdv(b, i); \
    integer(ik), intent(in) :: i; \
    real(rk), intent(in) :: b; \
    NAMEdv = op2_vd(i, b, FUNC, CHAINdv, .true., .true.); \
  end function NAMEdv; \
  integer(ik) function NAMEvv(i, j); \
    integer(ik), intent(in) :: i, j; \
    NAMEvv = op2_vv(i, j, FUNC, CHAINvv); \
  end function NAMEvv

#define DEF_OP2_SKIP_REAL(FUNC, NAMEvd, CHAINvd, NAMEdv, CHAINdv, NAMEvv, CHAINvv) \
  integer(ik) function NAMEvd(i, b); \
    integer(ik), intent(in) :: i; \
    real(rk), intent(in) :: b; \
    NAMEvd = op2_vd(i, b, FUNC, CHAINvd, .false., .false.); \
  end function NAMEvd; \
  integer(ik) function NAMEdv(b, i); \
    integer(ik), intent(in) :: i; \
    real(rk), intent(in) :: b; \
    NAMEdv = op2_vd(i, b, FUNC, CHAINdv, .true., .false.); \
  end function NAMEdv; \
  integer(ik) function NAMEvv(i, j); \
    integer(ik), intent(in) :: i, j; \
    NAMEvv = op2_vv(i, j, FUNC, CHAINvv); \
  end function NAMEvv

#endif

module fz_vari
  use, intrinsic :: iso_fortran_env
  use fz_env

  type :: vari
     sequence
     real(rk) :: val_
     real(rk) :: adj_ = 0d0
     integer(ik) :: i = 0 ! my index in storage
     integer(ik) :: j = 0 ! vari before i in storage (for rev pass)
     procedure(chain_op), nopass, pointer :: chain => chain_dummy
  end type vari

  integer(ik), parameter :: visize = storage_size(vari(0.d0))/8

  interface assignment(=)
     module procedure set_vari_val
     module procedure set_vari_real32
  end interface assignment(=)

  abstract interface
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

  pure subroutine recover(this, i)
    type(vari), intent(inout) :: this
    integer(ik), intent(in) :: i
    this = transfer(core_adstack%s_(i:(i+visize-1)), this)
  end subroutine recover

  subroutine recover_prev_id(id, i)
    integer(ik), intent(out) :: id
    integer(ik), intent(in) :: i
    ! skip val, adj, i, and jump to j
    call core_adstack%pop(i + rksize + rksize + iksize, id)
  end subroutine recover_prev_id

  ! recover operand
  subroutine recover_op1_v(this, i)
    type(vari), intent(inout) :: this
    integer(ik), intent(in) :: i
    integer(ik) :: j
    call core_adstack%pop(i+visize, j)
    call recover(this, j)
  end subroutine recover_op1_v

  subroutine recover_op2_v1(this, i)
    type(vari), intent(inout) :: this
    integer(ik), intent(in) :: i
    call recover_op1_v(this, i)
  end subroutine recover_op2_v1

  subroutine recover_op2_b(b, i)
    real(rk), intent(out) :: b
    integer(ik), intent(in) :: i
    call core_adstack%pop(i+visize+iksize, b) ! skip vi & vi arg
  end subroutine recover_op2_b

  subroutine recover_op2_v2(this, i)
    type(vari), intent(inout) :: this
    integer(ik), intent(in) :: i
    integer(ik) :: j
    call core_adstack%pop(i+visize+iksize, j)
    call recover(this, j)
  end subroutine recover_op2_v2

  elemental real(rk) function val(this)
    type(vari), intent(in) :: this
    type(vari) :: vi
    integer(ik) :: i
    i = this%i
    call recover(vi, i)
    val = vi%val_
  end function val

  elemental real(rk) function adj(this)
    type(vari), intent(in) :: this
    type(vari) :: vi
    integer(ik) :: i
    i = this%i
    call recover(vi, i)
    adj = vi%adj_
  end function adj

  subroutine set_vari_val(this, val)
    implicit none

    type(vari), intent(inout) :: this
    real(rk), intent(in) :: val
    this%val_ = val
    call push(this)
  end subroutine set_vari_val

  subroutine set_vari_real32(this, val)
    implicit none

    type(vari), intent(inout) :: this
    real(real32), intent(in) :: val
    this%val_ = val
    call push(this)
  end subroutine set_vari_real32

  ! push is the only call that creates new vari
  ! push checks if v is new in that v%i == 0
  subroutine push(vi)
    implicit none
    type(vari), intent(inout) :: vi
    integer(ik) :: i
    if (vi%i /= 0) then
       i = vi%i
       core_adstack%s_(i:(i+visize-1)) = transfer(vi, 0_int8, visize)
    else
       i = core_adstack%i_
       vi%i = core_adstack%i_
       vi%j = core_adstack%j_
       core_adstack%s_(i:(i+visize-1)) = transfer(vi, 0_int8, visize)
       call core_adstack%incr(visize, .true.)
    endif
  end subroutine push

  ! skip chain and return previous vi in AD stack
  integer(ik) function chain_dummy(i)
    integer(ik), intent(in) :: i
    call recover_prev_id(chain_dummy, i)
  end function chain_dummy

  subroutine reset_chain(id)
    integer(ik), intent(in) :: id
    type(vari) :: vi
    integer(ik) :: k
    k = id
    do while (k/=0)
       call recover(vi, k)
       vi%adj_ = 0.d0
       k = vi%j
       call push(vi)
    enddo
  end subroutine reset_chain

  subroutine chain(id)
    integer(ik), intent(in) :: id
    type(vari) :: vi
    integer(ik) :: k
    call recover(vi, id)
    vi%adj_ = 1.0d0
    call push(vi)
    k = id
    do while (k/=1)
       call recover(vi, k)
       k = vi%chain(k)
    enddo
  end subroutine chain

  DEF_CHAIN_OP1(chain_exp, (this%val_))
  DEF_OP1(dexp, exp_vi, chain_exp)

  DEF_CHAIN_OP1(chain_sin, (cos(a%val_)))
  DEF_OP1(dsin, sin_vi, chain_sin)

  DEF_CHAIN_OP1(chain_cos, (-sin(a%val_)))
  DEF_OP1(dcos, cos_vi, chain_cos)

  DEF_CHAIN_OP1(chain_tan, (1.d0/(cos(a%val_)*cos(a%val_))))
  DEF_OP1(dtan, tan_vi, chain_tan)

  DEF_CHAIN_OP1(chain_asin, (1.d0/sqrt(1.d0-a%val_*a%val_)))
  DEF_OP1(dasin, asin_vi, chain_asin)

  DEF_CHAIN_OP1(chain_acos, (-1.d0/sqrt(1.d0-a%val_*a%val_)))
  DEF_OP1(dacos, acos_vi, chain_acos)

  DEF_CHAIN_OP1(chain_atan, (1.d0/(1.d0+a%val_*a%val_)))
  DEF_OP1(datan, atan_vi, chain_atan)

  DEF_CHAIN_OP1(chain_log, (1.d0/a%val_))
  DEF_OP1(dlog, log_vi, chain_log)

  DEF_CHAIN_OP1(chain_log10, (1.d0/(a%val_*dlog(10.d0))))
  DEF_OP1(dlog10, log10_vi, chain_log10)

  DEF_CHAIN_OP1(chain_sqrt, (0.5d0/dsqrt(a%val_)))
  DEF_OP1(dsqrt, sqrt_vi, chain_sqrt)

  DEF_CHAIN_OP1(chain_neg, (-1.d0))
  DEF_OP1(-1*, neg_vi, chain_neg)

  DEF_CHAIN_OP1(chain_pos, (1.d0))
  DEF_OP1(1*, pos_vi, chain_pos)

  DEF_CHAIN_OP1(chain_sinh, (dcosh(a%val_)))
  DEF_OP1(dsinh, sinh_vi, chain_sinh)

  DEF_CHAIN_OP1(chain_cosh, (dsinh(a%val_)))
  DEF_OP1(dcosh, cosh_vi, chain_cosh)

  DEF_CHAIN_OP1(chain_tanh, (1.d0/(dcosh(a%val_)*dcosh(a%val_))) )
  DEF_OP1(dtanh, tanh_vi, chain_tanh)

  integer(ik) function op2_vv(ia, ib, op, chain_op2)
    implicit none
    integer(ik), intent(in) :: ia, ib
    procedure(op2) :: op
    procedure(chain_op) :: chain_op2
    type(vari) :: va, vb, v1
    call recover(va, ia)
    call recover(vb, ib)
    v1%val_ = op(va%val_, vb%val_)
    v1%chain => chain_op2
    call push(v1)
    call core_adstack%push(ia)  ! push operand
    call core_adstack%push(ib)  ! push operand
    op2_vv = v1%i
  end function op2_vv

  integer(ik) function op2_vd(ia, b, op, chain_op2, reverse_op, save_real)
    implicit none
    integer(ik), intent(in) :: ia
    real(rk), intent(in) :: b
    logical, intent(in) :: reverse_op, save_real
    procedure(op2) :: op
    procedure(chain_op) :: chain_op2
    type(vari) :: va, v1
    call recover(va, ia)
    if (reverse_op) then
       v1%val_ = op(b, va%val_)
    else
       v1%val_ = op(va%val_, b)
    endif
    v1%chain => chain_op2
    call push(v1)
    call core_adstack%push(ia)  ! push operand
    op2_vd = v1%i
    if (save_real) call core_adstack%push(b) ! push data operand
  end function op2_vd

  real(rk) function add_helper(a, b)
    real(rk), intent(in) :: a, b
    add_helper = a + b
  end function add_helper

  DEF_CHAIN_OP2_VD(chain_add_vi_d, (1.d0))
  DEF_CHAIN_OP2_VD(chain_add_d_vi, (1.d0))
  DEF_CHAIN_OP2_VV(chain_add_vi_vi, (1.d0), (1.d0))
  DEF_OP2_SKIP_REAL(add_helper, add_vi_d, chain_add_vi_d, add_d_vi, chain_add_d_vi, add_vi_vi, chain_add_vi_vi)

  real(rk) function substract_helper(a, b)
    real(rk), intent(in) :: a, b
    substract_helper = a - b
  end function substract_helper

  DEF_CHAIN_OP2_VD(chain_substract_vi_d, (1.d0))
  DEF_CHAIN_OP2_VD(chain_substract_d_vi, (-1.d0))
  DEF_CHAIN_OP2_VV(chain_substract_vi_vi, (1.d0), (-1.d0))
  DEF_OP2_SKIP_REAL(substract_helper, substract_vi_d, chain_substract_vi_d, substract_d_vi, chain_substract_d_vi, substract_vi_vi, chain_substract_vi_vi)

  real(rk) function multiply_helper(a, b)
    real(rk), intent(in) :: a, b
    multiply_helper = a * b
  end function multiply_helper
  DEF_CHAIN_OP2_VD(chain_multiply_vi_d, (b))
  DEF_CHAIN_OP2_VD(chain_multiply_d_vi, (b))
  DEF_CHAIN_OP2_VV(chain_multiply_vi_vi, (b%val_), (a%val_))
  DEF_OP2(multiply_helper, multiply_vi_d, chain_multiply_vi_d, multiply_d_vi, chain_multiply_d_vi, multiply_vi_vi, chain_multiply_vi_vi)

  real(rk) function divide_helper(a, b)
    real(rk), intent(in) :: a, b
    divide_helper = a / b
  end function divide_helper
  DEF_CHAIN_OP2_VD(chain_divide_vi_d, (1.d0/b))
  DEF_CHAIN_OP2_VD(chain_divide_d_vi, (-this%val_/a%val_))
  DEF_CHAIN_OP2_VV(chain_divide_vi_vi, (1.d0/b%val_), (-this%val_/b%val_))
  DEF_OP2(divide_helper, divide_vi_d, chain_divide_vi_d, divide_d_vi, chain_divide_d_vi, divide_vi_vi, chain_divide_vi_vi)

end module fz_vari
