module fz_vari
  use, intrinsic :: iso_fortran_env
  use, intrinsic :: iso_c_binding
  use fz_env

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
    implicit none
    type(vari), pointer, intent(out) :: p
    integer(ik), intent(in) :: i
    type(c_ptr) :: cp
    p => null()
    if (i > 0) then
       cp = c_loc(core_adstack%s_(i))
       call c_f_pointer(cp, p)
    end if
  end subroutine recover

  elemental real(rk) function vi_val(this)
    implicit none
    type(vari), intent(in) :: this
    vi_val = this%val_
  end function vi_val

  impure elemental real(rk) function vi_val_at(i)
    implicit none
    integer(ik), intent(in) :: i
    type(vari), pointer :: p
    call recover(p, i)
    vi_val_at = vi_val(p)
  end function vi_val_at

  elemental real(rk) function vi_adj(this)
    implicit none
    type(vari), intent(in) :: this
    vi_adj = this%adj_
  end function vi_adj

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

end module fz_vari
