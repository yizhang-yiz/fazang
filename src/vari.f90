module fz_vari
  use, intrinsic :: iso_fortran_env
  use, intrinsic :: iso_c_binding
  use fz_env

  type, bind(c) :: vari
     real(rk) :: val_
     real(rk) :: adj_ = 0d0
  end type vari

  integer(ik), parameter :: visize = storage_size(vari(0.d0))/storage_size(0_c_int8_t)

  type :: chain_base
   contains
     procedure, nopass :: chain => chain_dummy
  end type chain_base
  type(chain_base), target :: chain_base_instance

  type :: chain_wrapper
     class(chain_base), pointer :: c => null()
  end type

  type(chain_wrapper) :: chains(adsize/8)

  interface new_vari
     module procedure new_vari_val
     module procedure new_vari_real32
     module procedure new_vari_int
     module procedure new_vari_int_int
     module procedure new_vari_int2
     module procedure new_vari_int_real
     module procedure new_vari_ivec
  end interface new_vari

  interface vi_val
     module procedure vari_val
     module procedure vari_val_at
     module procedure real_val
  end interface vi_val

  interface vi_adj
     module procedure vari_adj
     module procedure vari_adj_at
  end interface vi_adj

  interface recover
     module procedure recover_vari
     module procedure recover_parent
     module procedure recover_parent2
     module procedure recover_parent_real
  end interface recover

  abstract interface
     subroutine chain_op(ip)
       import
       implicit none
       integer(ik), intent(in) :: ip
     end subroutine chain_op
  end interface

contains

  subroutine recover_vari(i, p)
    implicit none
    type(vari), pointer, intent(out) :: p
    integer(ik), intent(in) :: i
    p => null()
    if (i > 0) call c_f_pointer(c_loc(core_adstack%s_(core_adstack%id(i))), p)
  end subroutine recover_vari

  subroutine recover_parent(ip, p, p1)
    implicit none
    integer(ik), intent(in) :: ip
    type(vari), pointer, intent(out) :: p, p1
    integer(ik), pointer :: i
    call c_f_pointer(c_loc(core_adstack%s_(core_adstack%id(ip))), p)
    call c_f_pointer(c_loc(core_adstack%s_(core_adstack%id(ip) + visize)), i)
    call c_f_pointer(c_loc(core_adstack%s_(core_adstack%id(i))), p1)
  end subroutine recover_parent

  subroutine recover_parent2(ip, p, p1, p2)
    implicit none
    integer(ik), intent(in) :: ip
    type(vari), pointer, intent(out) :: p, p1, p2
    integer(ik), pointer :: i
    type(c_ptr) :: cp
    cp = c_loc(core_adstack%s_(core_adstack%id(ip)))
    call c_f_pointer(cp, p)
    cp = c_loc(core_adstack%s_(core_adstack%id(ip) + visize))
    call c_f_pointer(cp, i)
    cp = c_loc(core_adstack%s_(core_adstack%id(i)))
    call c_f_pointer(cp, p1)
    cp = c_loc(core_adstack%s_(core_adstack%id(ip) + visize + iksize))
    call c_f_pointer(cp, i)
    cp = c_loc(core_adstack%s_(core_adstack%id(i)))
    call c_f_pointer(cp, p2)
  end subroutine recover_parent2

  subroutine recover_parent_real(ip, p, p1, b)
    implicit none
    integer(ik), intent(in) :: ip
    type(vari), pointer, intent(out) :: p, p1
    real(rk), intent(out) :: b
    integer(ik), pointer :: i
    real(rk), pointer :: pb
    type(c_ptr) :: cp
    cp = c_loc(core_adstack%s_(core_adstack%id(ip)))
    call c_f_pointer(cp, p)
    cp = c_loc(core_adstack%s_(core_adstack%id(ip) + visize))
    call c_f_pointer(cp, i)
    cp = c_loc(core_adstack%s_(core_adstack%id(i)))
    call c_f_pointer(cp, p1)
    cp = c_loc(core_adstack%s_(core_adstack%id(ip)+visize+iksize))
    call c_f_pointer(cp, pb)
    b = pb
  end subroutine recover_parent_real

  impure elemental real(rk) function vari_val_at(ip)
    implicit none
    integer(ik), intent(in) :: ip
    type(vari), pointer :: this
    call recover(ip, this)
    vari_val_at = this%val_
  end function vari_val_at

  elemental real(rk) function vari_val(vi)
    implicit none
    type(vari), intent(in) :: vi
    vari_val = vi%val_
  end function vari_val

  elemental real(rk) function real_val(x)
    implicit none
    real(rk), intent(in) :: x
    real_val = x
  end function real_val

  impure elemental real(rk) function vari_adj_at(ip)
    implicit none
    integer(ik), intent(in) :: ip
    type(vari), pointer :: this
    call recover(ip, this)
    vari_adj_at = this%adj_
  end function vari_adj_at

  elemental real(rk) function vari_adj(vi)
    implicit none
    type(vari), intent(in) :: vi
    vari_adj = vi%adj_
  end function vari_adj

  ! move according to inserted object size
  subroutine add_vari(id, p)
    implicit none
    integer(ik), intent(out) :: id
    type(vari), pointer, intent(out) :: p
    type(c_ptr) :: cp
    core_adstack%nvari = core_adstack%nvari + 1
    core_adstack%id(core_adstack%nvari) = core_adstack%i_
    cp = c_loc(core_adstack%s_(core_adstack%i_))
    call c_f_pointer(cp, p)
    id = core_adstack%nvari
    core_adstack%i_ = core_adstack%i_ + visize
  end subroutine add_vari

  subroutine new_vari_val(this, val)
    implicit none
    integer(ik), intent(out) :: this
    type(vari), pointer :: vp
    real(rk), intent(in) :: val
    call add_vari(this, vp)
    vp%val_ = val
    chains(this)%c => chain_base_instance
  end subroutine new_vari_val

  subroutine new_vari_int(this, v1, i, vi)
    implicit none
    integer(ik), intent(out) :: this
    integer(ik), intent(in) :: i
    type(vari), pointer, intent(out) :: v1, vi
    integer(ik), pointer :: ip
    type(c_ptr) :: cp
    call add_vari(this, v1)
    cp = c_loc(core_adstack%s_(core_adstack%id(i)))
    call c_f_pointer(cp, vi)
    cp = c_loc(core_adstack%s_(core_adstack%i_))
    call c_f_pointer(cp, ip)
    ip = i
    core_adstack%i_ = core_adstack%i_ + iksize
  end subroutine new_vari_int

  subroutine new_vari_ivec(this, v1, val, vec)
    implicit none
    integer(ik), intent(out) :: this
    integer(ik), target, intent(in) :: vec(:)
    real(rk), intent(in) :: val
    type(vari), pointer, intent(out) :: v1
    integer(ik), pointer :: ip, ipv(:)
    type(c_ptr) :: cp
    call add_vari(this, v1)
    v1%val_ = val
    cp = c_loc(core_adstack%s_(core_adstack%i_))
    call c_f_pointer(cp, ipv, [(size(vec))])
    ipv = vec
    core_adstack%i_ = core_adstack%i_ + iksize*size(vec)
  end subroutine new_vari_ivec

  subroutine new_vari_int_int(this, v1, i, vi, j)
    implicit none
    integer(ik), intent(out) :: this
    integer(ik), intent(in) :: i, j
    type(vari), pointer, intent(out) :: v1, vi
    integer(ik), pointer :: ip
    type(c_ptr) :: cp
    call add_vari(this, v1)
    cp = c_loc(core_adstack%s_(core_adstack%id(i)))
    call c_f_pointer(cp, vi)
    cp = c_loc(core_adstack%s_(core_adstack%i_))
    call c_f_pointer(cp, ip)
    ip = i
    core_adstack%i_ = core_adstack%i_ + iksize
    cp = c_loc(core_adstack%s_(core_adstack%i_))
    call c_f_pointer(cp, ip)
    ip = j
    core_adstack%i_ = core_adstack%i_ + iksize
  end subroutine new_vari_int_int

  subroutine new_vari_int_real(this, v1, i, vi, b)
    implicit none
    integer(ik), intent(out) :: this
    integer(ik), intent(in) :: i
    type(vari), pointer, intent(out) :: v1, vi
    integer(ik), pointer :: ip
    real(rk), intent(in) :: b
    real(rk), pointer :: rp
    type(c_ptr) :: cp
    call add_vari(this, v1)
    cp = c_loc(core_adstack%s_(core_adstack%id(i)))
    call c_f_pointer(cp, vi)
    cp = c_loc(core_adstack%s_(core_adstack%i_))
    call c_f_pointer(cp, ip)
    ip = i
    core_adstack%i_ = core_adstack%i_ + iksize
    cp = c_loc(core_adstack%s_(core_adstack%i_))
    call c_f_pointer(cp, rp)
    rp = b
    core_adstack%i_ = core_adstack%i_ + rksize
  end subroutine new_vari_int_real

  subroutine new_vari_int2(this, v1, ia, va, ib, vb)
    implicit none
    integer(ik), intent(out) :: this
    integer(ik), intent(in) :: ia, ib
    type(vari), pointer, intent(out) :: v1, va, vb
    integer(ik), pointer :: ip
    type(c_ptr) :: cp
    call add_vari(this, v1)
    cp = c_loc(core_adstack%s_(core_adstack%id(ia)))
    call c_f_pointer(cp, va)
    cp = c_loc(core_adstack%s_(core_adstack%id(ib)))
    call c_f_pointer(cp, vb)
    cp = c_loc(core_adstack%s_(core_adstack%i_))
    call c_f_pointer(cp, ip)
    ip = ia
    core_adstack%i_ = core_adstack%i_ + iksize
    cp = c_loc(core_adstack%s_(core_adstack%i_))
    call c_f_pointer(cp, ip)
    ip = ib
    core_adstack%i_ = core_adstack%i_ + iksize
  end subroutine new_vari_int2

  subroutine new_vari_real32(this, val)
    implicit none
    integer(ik), intent(out) :: this
    real(c_float), intent(in) :: val
    call new_vari_val(this, real(val, rk))
  end subroutine new_vari_real32

  ! skip chain and return previous vi in AD stack
  subroutine chain_dummy(ip)
    implicit none
    integer(ik), intent(in) :: ip
  end subroutine

  subroutine reset_chain(ip)
    implicit none
    integer(ik), intent(in) :: ip
    type(vari), pointer :: p1
    integer(ik) :: i
    call recover(i, p1)
    do i = ip, 1, -1
       call recover(i, p1)
       p1%adj_ = 0.d0
    enddo
  end subroutine reset_chain

  subroutine chain(ip)
    implicit none
    integer(ik), intent(in) :: ip
    type(vari), pointer :: p1
    procedure(chain_op), pointer :: p1_chain
    integer(ik) :: i
    call recover(ip, p1)
    p1%adj_ = 1.0d0
    do i = ip, 1, -1
       call chains(i)%c%chain(i)
    enddo
  end subroutine chain

end module fz_vari
