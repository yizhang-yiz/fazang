#define VARI_BASE real(rk) :: val_; real(rk) :: adj_ = 0d0

module fz_vari
  use, intrinsic :: iso_fortran_env
  use, intrinsic :: iso_c_binding
  use fz_env

  type, bind(c) :: vari
     VARI_BASE
  end type vari

  type, bind(c) :: v_vari
     VARI_BASE
     integer(ik) :: ia = 0
  end type v_vari

  type, bind(c) :: vd_vari
     VARI_BASE
     integer(ik) :: ia = 0
     real(rk) :: b = 0.d0
  end type vd_vari

  type, bind(c) :: vv_vari
     VARI_BASE
     integer(ik) :: ia = 0, ib = 0
  end type vv_vari

  type, bind(c) :: vdd_vari
     VARI_BASE
     integer(ik) :: ia = 0
     real(rk) :: b = 0, c = 0
  end type vdd_vari

  type, bind(c) :: vvd_vari
     VARI_BASE
     integer(ik) :: ia = 0, ib = 0
     real(rk) :: c = 0
  end type vvd_vari

  integer(ik), parameter :: visize = storage_size(vari(0.d0))/storage_size(0_c_int8_t)
  integer(ik), parameter :: v_visize = storage_size(v_vari(0.d0))/storage_size(0_c_int8_t)
  integer(ik), parameter :: vd_visize = storage_size(vd_vari(0.d0))/storage_size(0_c_int8_t)
  integer(ik), parameter :: vv_visize = storage_size(vv_vari(0.d0))/storage_size(0_c_int8_t)
  integer(ik), parameter :: vdd_visize = storage_size(vdd_vari(0.d0))/storage_size(0_c_int8_t)
  integer(ik), parameter :: vvd_visize = storage_size(vvd_vari(0.d0))/storage_size(0_c_int8_t)

  type :: chain_base
   contains
     procedure, nopass :: chain => chain_dummy
  end type chain_base
  type(chain_base), target :: chain_base_instance

  ! wari (wrapper of vari) has two components: raw storage loc & chain
  type :: wari
     integer(ik) :: i
     class(chain_base), pointer :: c => null()
  end type

  type(wari) :: chains(adsize/8)

  ! "new" handles vari creation, the id is the corresponding index in "chains" array
  interface new_vari
     module procedure new_vari_val
     module procedure new_v_vari
     module procedure new_vd_vari
     module procedure new_vv_vari
     module procedure new_vec_vari
     module procedure new_vdd_vari
     module procedure new_vvd_vari
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

  ! recover happens during chaining, the id's are internal storage loc
  interface recover
     module procedure recover_vari
     module procedure recover_v_vari
     module procedure recover_vd_vari
     module procedure recover_vv_vari
     module procedure recover_vi_vari
     module procedure recover_vdd_vari
     module procedure recover_vvd_vari
  end interface recover

  abstract interface
     subroutine chain_op(ip)
       import
       implicit none
       integer(ik), intent(in) :: ip
     end subroutine chain_op
  end interface

contains

  subroutine recover_vari(ip, p)
    implicit none
    type(vari), pointer, intent(out) :: p
    integer(ik), intent(in) :: ip
    call c_f_pointer(c_loc(core_adstack%s_(ip)), p)
  end subroutine recover_vari

  subroutine recover_v_vari(ip, p, p1)
    implicit none
    integer(ik), intent(in) :: ip
    type(v_vari), pointer, intent(out) :: p
    type(vari), pointer, intent(out) :: p1
    call c_f_pointer(c_loc(core_adstack%s_(ip)), p)
    call c_f_pointer(c_loc(core_adstack%s_(p%ia)), p1)
  end subroutine recover_v_vari

  subroutine recover_vv_vari(ip, p, p1, p2)
    implicit none
    integer(ik), intent(in) :: ip
    type(vv_vari), pointer, intent(out) :: p
    type(vari), pointer, intent(out) :: p1, p2
    call c_f_pointer(c_loc(core_adstack%s_(ip)), p)
    call c_f_pointer(c_loc(core_adstack%s_(p%ia)), p1)
    call c_f_pointer(c_loc(core_adstack%s_(p%ib)), p2)
  end subroutine recover_vv_vari

  subroutine recover_vdd_vari(ip, p, p1, b, c)
    implicit none
    integer(ik), intent(in) :: ip
    type(vdd_vari), pointer, intent(out) :: p
    type(vari), pointer, intent(out) :: p1
    real(rk), intent(out) :: b, c
    call c_f_pointer(c_loc(core_adstack%s_(ip)), p)
    call c_f_pointer(c_loc(core_adstack%s_(p%ia)), p1)
    b = p%b; c = p%c
  end subroutine recover_vdd_vari

  subroutine recover_vvd_vari(ip, p, p1, p2, c)
    implicit none
    integer(ik), intent(in) :: ip
    type(vvd_vari), pointer, intent(out) :: p
    type(vari), pointer, intent(out) :: p1, p2
    real(rk), intent(out) :: c
    call c_f_pointer(c_loc(core_adstack%s_(ip)), p)
    call c_f_pointer(c_loc(core_adstack%s_(p%ia)), p1)
    call c_f_pointer(c_loc(core_adstack%s_(p%ib)), p2)
    c = p%c
  end subroutine recover_vvd_vari

  subroutine recover_vd_vari(ip, p, p1, b)
    implicit none
    integer(ik), intent(in) :: ip
    type(vd_vari), pointer, intent(out) :: p
    type(vari), pointer, intent(out) :: p1
    real(rk), pointer :: rp
    real(rk), intent(out) :: b
    call c_f_pointer(c_loc(core_adstack%s_(ip)), p); b = p%b
    call c_f_pointer(c_loc(core_adstack%s_(p%ia)), p1)
  end subroutine recover_vd_vari

  subroutine recover_vi_vari(ip, p, p1, n)
    implicit none
    integer(ik), intent(in) :: ip
    type(v_vari), pointer, intent(out) :: p
    type(vari), pointer, intent(out) :: p1
    real(rk), pointer :: rp
    integer(ik), intent(out) :: n
    integer(ik), pointer :: np
    call c_f_pointer(c_loc(core_adstack%s_(ip)), p)
    call c_f_pointer(c_loc(core_adstack%s_(p%ia)), p1)
    call c_f_pointer(c_loc(core_adstack%s_(ip + v_visize)), np)
    n = np
  end subroutine recover_vi_vari

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

  integer(ik) function add_vari(vari_size)
    implicit none
    integer(ik), intent(in) :: vari_size
    core_adstack%nvari = core_adstack%nvari + 1
    add_vari = core_adstack%nvari
    chains(add_vari)%i = core_adstack%i_
    core_adstack%i_ = core_adstack%i_ +  vari_size
  end function add_vari

  subroutine new_vari_val(this, val)
    implicit none
    integer(ik), intent(out) :: this
    type(vari), pointer :: vp
    real(rk), intent(in) :: val
    this = add_vari(visize)
    chains(this)%c => chain_base_instance
    call c_f_pointer(c_loc(core_adstack%s_(chains(this)%i)), vp)
    vp = vari(val, 0.d0)
  end subroutine new_vari_val

  subroutine new_v_vari(this, val, i)
    implicit none
    integer(ik), intent(out) :: this
    integer(ik), intent(in) :: i
    real(rk), intent(in) :: val
    type(v_vari), pointer :: vp
    this = add_vari(v_visize)
    call c_f_pointer(c_loc(core_adstack%s_(chains(this)%i)), vp)
    vp = v_vari(val, 0.d0, chains(i)%i)
  end subroutine new_v_vari

  subroutine new_vd_vari(this, val, i, b)
    implicit none
    integer(ik), intent(out) :: this
    integer(ik), intent(in) :: i
    real(rk), intent(in) :: val
    real(rk), intent(in) :: b
    type(vd_vari), pointer :: vp
    this = add_vari(vd_visize)
    call c_f_pointer(c_loc(core_adstack%s_(chains(this)%i)), vp)
    vp = vd_vari(val, 0.d0, chains(i)%i, b)
  end subroutine new_vd_vari

  subroutine new_vi_vari(this, val, i, n)
    implicit none
    integer(ik), intent(out) :: this
    integer(ik), intent(in) :: i, n
    real(rk), intent(in) :: val
    type(v_vari), pointer :: vp
    integer(ik), pointer :: ip
    this = add_vari(v_visize)
    call c_f_pointer(c_loc(core_adstack%s_(chains(this)%i)), vp)
    vp = v_vari(val, 0.d0, chains(i)%i)
    call c_f_pointer(c_loc(core_adstack%s_(core_adstack%i_)), ip)
    ip = n
    core_adstack%i_ = core_adstack%i_ + iksize
  end subroutine new_vi_vari

  subroutine new_vv_vari(this, val, i, j)
    implicit none
    integer(ik), intent(out) :: this
    integer(ik), intent(in) :: i, j
    real(rk), intent(in) :: val
    type(vv_vari), pointer :: vp
    this = add_vari(vv_visize)
    call c_f_pointer(c_loc(core_adstack%s_(chains(this)%i)), vp)
    vp = vv_vari(val, 0.d0, chains(i)%i, chains(j)%i)
  end subroutine new_vv_vari

  subroutine new_vdd_vari(this, val, i, b, c)
    implicit none
    integer(ik), intent(out) :: this
    integer(ik), intent(in) :: i
    real(rk), intent(in) :: val
    type(vdd_vari), pointer :: vp
    real(rk), intent(in) :: b, c
    this = add_vari(vdd_visize)
    call c_f_pointer(c_loc(core_adstack%s_(chains(this)%i)), vp)
    vp = vdd_vari(val, 0.d0, chains(i)%i, b, c)
  end subroutine new_vdd_vari

  subroutine new_vvd_vari(this, val, i, j, c)
    implicit none
    integer(ik), intent(out) :: this
    integer(ik), intent(in) :: i, j
    real(rk), intent(in) :: val
    type(vvd_vari), pointer :: vp
    real(rk), intent(in) :: c
    this = add_vari(vvd_visize)
    call c_f_pointer(c_loc(core_adstack%s_(chains(this)%i)), vp)
    vp = vvd_vari(val, 0.d0, chains(i)%i, chains(j)%i, c)
  end subroutine new_vvd_vari

  subroutine new_vec_vari(this, val, vec)
    implicit none
    integer(ik), intent(out) :: this
    integer(ik), target, intent(in) :: vec(:)
    type(v_vari), pointer :: vp
    real(rk), intent(in) :: val
    integer(ik), pointer :: ipv(:)
    this = add_vari(v_visize)
    call c_f_pointer(c_loc(core_adstack%s_(chains(this)%i)), vp)
    vp = v_vari(val, 0.d0, size(vec))
    call c_f_pointer(c_loc(core_adstack%s_(core_adstack%i_)), ipv, [(size(vec))])
    ipv = vec
    core_adstack%i_ = core_adstack%i_ + iksize*size(vec)
  end subroutine new_vec_vari

  subroutine new_general_vari(this, val, vec, data, idata)
    implicit none
    integer(ik), intent(out) :: this
    integer(ik), target, intent(in) :: vec(:), idata(:)
    real(rk), target, intent(in) :: data(:)
    real(rk), intent(in) :: val
    type(vari), pointer :: vp
    integer(ik), pointer :: ipv(:)
    real(rk), pointer :: pv(:)
    integer(ik) :: nv, nd, ni
    nv = size(vec); nd = size(data); ni = size(idata)
    this = add_vari(visize)
    call c_f_pointer(c_loc(core_adstack%s_(chains(this)%i)), vp)
    vp = vari(val, 0.d0)
    call c_f_pointer(c_loc(core_adstack%s_(core_adstack%i_)), ipv, [(1)])
    ipv(1) = nv
    core_adstack%i_ = core_adstack%i_ + iksize
    if (nv > 0) then            ! vari dependencies
       call c_f_pointer(c_loc(core_adstack%s_(core_adstack%i_)), ipv, [(nv)])
       ipv = vec
       core_adstack%i_ = core_adstack%i_ + iksize*nv
    end if

    call c_f_pointer(c_loc(core_adstack%s_(core_adstack%i_)), ipv, [(1)])
    ipv(1) = nd
    core_adstack%i_ = core_adstack%i_ + iksize
    if (nd > 0) then
       call c_f_pointer(c_loc(core_adstack%s_(core_adstack%i_)), pv, [(nd)])
       pv = data
       core_adstack%i_ = core_adstack%i_ + rksize*nd
    endif

    call c_f_pointer(c_loc(core_adstack%s_(core_adstack%i_)), ipv, [(1)])
    ipv(1) = ni
    core_adstack%i_ = core_adstack%i_ + iksize
    if (ni > 0) then
       call c_f_pointer(c_loc(core_adstack%s_(core_adstack%i_)), ipv, [(ni)])
       ipv = idata
       core_adstack%i_ = core_adstack%i_ + iksize*ni
    end if
  end subroutine new_general_vari

  subroutine recover_general_vari(ip, p, vec, data, idata)
    implicit none
    integer(ik), intent(in) :: ip
    type(vari), pointer, intent(out) :: p
    integer(ik), pointer, intent(out) :: vec(:), idata(:)
    real(rk), pointer, intent(out) :: data(:)
    integer(ik), pointer :: isize
    integer(ik) :: head
    head = ip
    call c_f_pointer(c_loc(core_adstack%s_(head)), p)
    head = head + visize

    call c_f_pointer(c_loc(core_adstack%s_(head)), isize)
    head = head + iksize
    if (isize > 0) then
       call c_f_pointer(c_loc(core_adstack%s_(head)), vec, [(isize)])
       head = head + iksize * isize
    endif

    call c_f_pointer(c_loc(core_adstack%s_(head)), isize)
    head = head + iksize
    if (isize > 0) then
       call c_f_pointer(c_loc(core_adstack%s_(head)), data, [(isize)])
       head = head + rksize * isize
    endif

    call c_f_pointer(c_loc(core_adstack%s_(head)), isize)
    head = head + iksize
    if (isize > 0) then
       call c_f_pointer(c_loc(core_adstack%s_(head)), idata, [(isize)])
       head = head + iksize * isize
    endif
  end subroutine recover_general_vari

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
    integer(ik) :: j
    do j = ip, 1, -1
       call recover(chains(j)%i, p1)
       p1%adj_ = 0.d0
    enddo
  end subroutine reset_chain

  subroutine chain(ip)
    implicit none
    integer(ik), intent(in) :: ip
    type(vari), pointer :: p1
    integer(ik) :: j
    call recover(chains(ip)%i, p1)
    p1%adj_ = 1.0d0
    do j = ip, 1, -1
       call chains(j)%c%chain(chains(j)%i)
    enddo
  end subroutine chain

end module fz_vari
