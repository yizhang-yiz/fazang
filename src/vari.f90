#ifndef VARI_RECOVER_DEFINED
#define VARI_RECOVER_DEFINED

#define VARI_I_RECOVER(v, i) transfer(v%stack%s(i:(i+varisize(v)-1)), v)
#define VARI_RECOVER(v) transfer(v%stack%s(v%i:(v%i+varisize(v)-1)), v)

#endif

module fazang_vari
  use, intrinsic :: iso_fortran_env
  use fazang_env

  private
  public :: vari, assignment(=)
  ! public :: val, adj
  public :: varisize, push, recover
  public :: vari_v, exp_v, exp_vi

  type :: vari
     private
     real(rk) :: val_
     real(rk) :: adj_ = 0d0
     integer(ik), public :: i = 0 ! my index in storage
     integer(ik), public :: j = 0 ! vari before i in storage (for rev pass)
     type(adstack), pointer, public :: stack => core_adstack
     procedure(chain_op), public, nopass, pointer :: chain => chain_dummy
   contains
     procedure, public :: val
     procedure, public :: adj
     procedure, public :: set_val
     procedure, public :: set_adj
  end type vari

  ! interface vari
  !    module procedure new_vari_val
  ! end interface vari

  interface assignment(=)
     module procedure set_vari_val
     module procedure set_vari_real32
  end interface assignment(=)

  abstract interface
     subroutine chain_op(i)
       use fazang_env
       integer(ik), intent(in) :: i
     end subroutine chain_op
  end interface

  abstract interface
     real(rk) function op1(x)
       use fazang_env
       real(rk), intent(in) :: x
     end function op1
  end interface

  type, extends(vari) :: vari_v
     integer(ik) :: ia
  end type vari_v

  type, extends(vari_v) :: exp_v
  end type exp_v

contains

  elemental integer(ik) function varisize(v) ! in bytes
    class(vari), intent(in) :: v
    varisize = storage_size(v)/8
  end function varisize

  ! function new_vari_val(val) result(v)
  !   real(rk), intent(in) :: val
  !   type(vari) :: v
  !   call set_vari_val(v, val)
  ! end function new_vari_val

  real(rk) function val(this)
    class(vari), intent(in) :: this
    type(vari) :: v
    v = recover(this%i)
    val = v%val_
  end function val

  real(rk) function adj(this)
    class(vari), intent(in) :: this
    type(vari) :: v
    v = recover(this%i)
    adj = v%adj_
  end function adj

  subroutine set_vari_val(this, val)
    implicit none

    class(vari), intent(inout) :: this
    real(rk), intent(in) :: val
    this%val_ = val
    call push(this)
  end subroutine set_vari_val

  subroutine set_vari_real32(this, val)
    implicit none

    class(vari), intent(inout) :: this
    real(real32), intent(in) :: val
    this%val_ = val
    call push(this)
  end subroutine set_vari_real32

  subroutine set_val(this, val)
    implicit none
    class(vari), intent(inout) :: this
    real(rk), intent(in) :: val
    type(vari) :: v
    v = recover(this%i)
    v%val_ = val
    call push(v)
  end subroutine set_val

  subroutine set_adj(this, val)
    implicit none
    class(vari), intent(inout) :: this
    real(rk), intent(in) :: val
    type(vari) :: v
    v = recover(this%i)
    v%adj_ = val
    call push(v)
  end subroutine set_adj

  ! push is the only call that creates new vari
  ! push checks if v is new in that v%i == 0
  subroutine push(v)
    implicit none
    class(vari), intent(inout) :: v
    integer(ik) :: nb
    nb = varisize(v)
    if (v%i /= 0) then
       v%stack%s(v%i:(v%i+nb-1)) = transfer(v, 0_int8, nb)
    else
       v%i = v%stack%i
       v%j = v%stack%j
       v%stack%s(v%stack%i:(v%stack%i+nb-1)) = transfer(v, 0_int8, nb)
       call v%stack%incr(nb)
    endif
  end subroutine push

  function recover(i) result(v)
    integer(ik), intent(in) :: i
    type(vari) :: v
    v = VARI_I_RECOVER(v, i)
  end function recover

  subroutine chain_dummy(i)
    integer(ik), intent(in) :: i
  end subroutine chain_dummy

  integer(ik) function op1_vi(ia, op, chain_op1)
    implicit none
    integer(ik), intent(in) :: ia
    procedure(op1) :: op
    procedure(chain_op) :: chain_op1
    type(vari) :: v0
    type(exp_v) :: res
    v0 = recover(ia)
    res%val_ = op(v0%val_)
    res%ia = ia
    res%chain => chain_op1
    call push(res)
    op1_vi = res%i
  end function op1_vi

  integer(ik) function exp_vi(ia)
    implicit none
    intrinsic :: dexp
    integer(ik), intent(in) :: ia
    exp_vi = op1_vi(ia, dexp, chain_exp)
  end function exp_vi

  subroutine chain_exp(i)
    implicit none
    integer(ik), intent(in) :: i
    type(vari) :: this, a
    type(exp_v) :: v
    this = recover(i)
    v = transfer(this%stack%s(i:(i+varisize(v)-1)), v)
    a = recover(v%ia)
    call a%set_adj(adj(a) + adj(v) * val(v))
  end subroutine chain_exp


end module fazang_vari
