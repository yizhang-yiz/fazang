#ifndef DEF_VARI_OP1_DEFINED
#define DEF_VARI_OP1_DEFINED

#define DEF_OP1(NAME, FUNC, CHAIN) \
  integer(ik) function NAME (ia); \
    implicit none; \
    intrinsic :: FUNC; \
    integer(ik), intent(in) :: ia; \
    NAME = op1_vi(ia, FUNC, CHAIN ); \
  end function NAME

#define DEF_CHAIN_OP1(NAME, DYDX) \
  integer(ik) function NAME (i); \
    implicit none; \
    integer(ik), intent(in) :: i; \
    type(vari) :: this, a; \
    call recover(this, i); \
    call recover(a, int_after_vi(i)); \
    a%adj_ = a%adj_ + this%adj_ * DYDX; \
    call push(a); \
    NAME = this%j; \
  end function NAME

#endif

module fazang_vari
  use, intrinsic :: iso_fortran_env
  use fazang_env

  type :: vari
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
       use fazang_env
       real(rk), intent(in) :: x
     end function op1
  end interface

contains

  subroutine recover(this, i)
    type(vari), intent(inout) :: this
    integer(ik), intent(in) :: i
    this = transfer(core_adstack%s_(i:(i+visize-1)), this)
  end subroutine recover

  real(rk) function val(this)
    type(vari), intent(in) :: this
    type(vari) :: vi
    integer(ik) :: i
    i = this%i
    call recover(vi, i)
    val = vi%val_
  end function val

  real(rk) function adj(this)
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

  integer(ik) function int_after_vi(i)
    integer(ik), intent(in) :: i
    type(vari) :: vi
    call core_adstack%pop(i+visize, int_after_vi)
  end function int_after_vi

  integer(ik) function chain_dummy(i)
    integer(ik), intent(in) :: i
  end function chain_dummy

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

  integer(ik) function op1_vi(ia, op, chain_op1)
    implicit none
    integer(ik), intent(in) :: ia
    procedure(op1) :: op
    procedure(chain_op) :: chain_op1
    type(vari) :: v0, v1
    call recover(v0, ia)
    v1%val_ = op(v0%val_)
    v1%chain => chain_op1
    call push(v1)
    call core_adstack%push(ia)
    op1_vi = v1%i
  end function op1_vi

  DEF_CHAIN_OP1(chain_exp, (this%val_))
  DEF_OP1(exp_vi, dexp, chain_exp)

  DEF_CHAIN_OP1(chain_sin, (cos(a%val_)))
  DEF_OP1(sin_vi, dsin, chain_sin)

end module fazang_vari
