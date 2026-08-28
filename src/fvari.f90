#define FVARI_BASE type(dual) :: val_; type(dual) :: adj_ = dual(0d0, 0d0)

module fz_fvari
  use, intrinsic :: iso_fortran_env
  use, intrinsic :: iso_c_binding
  use fz_env
  use fz_dual
  use fz_vari, only : add_vari, chain_base, chain_base_instance, chains

  type, bind(c) :: vari
     FVARI_BASE
  end type vari

  type, bind(c) :: v_vari
     FVARI_BASE
     integer(ik) :: ia = 0
  end type v_vari

  type, bind(c) :: vd_vari
     FVARI_BASE
     integer(ik) :: ia = 0
     real(rk) :: b = 0.d0
  end type vd_vari

  type, bind(c) :: vv_vari
     FVARI_BASE
     integer(ik) :: ia = 0, ib = 0
  end type vv_vari

  type, bind(c) :: vdd_vari
     FVARI_BASE
     integer(ik) :: ia = 0
     real(rk) :: b = 0, c = 0
  end type vdd_vari

  type, bind(c) :: vvd_vari
     FVARI_BASE
     integer(ik) :: ia = 0, ib = 0
     real(rk) :: c = 0
  end type vvd_vari

  integer(ik), parameter :: visize = storage_size(vari(dual(0.d0, 0.d0)))/storage_size(0_c_int8_t)
  integer(ik), parameter :: v_visize = storage_size(v_vari(dual(0.d0, 0.d0)))/storage_size(0_c_int8_t)
  integer(ik), parameter :: vd_visize = storage_size(vd_vari(dual(0.d0, 0.d0)))/storage_size(0_c_int8_t)
  integer(ik), parameter :: vv_visize = storage_size(vv_vari(dual(0.d0, 0.d0)))/storage_size(0_c_int8_t)
  integer(ik), parameter :: vdd_visize = storage_size(vdd_vari(dual(0.d0, 0.d0)))/storage_size(0_c_int8_t)
  integer(ik), parameter :: vvd_visize = storage_size(vvd_vari(dual(0.d0, 0.d0)))/storage_size(0_c_int8_t)

  ! "new" handles vari creation, the id is the corresponding index in "chains" array
  interface new_vari
     module procedure new_vari_val
     module procedure new_vari_dual
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

  interface vi_val_v
     module procedure vari_val_v
     module procedure vari_val_v_at
  end interface vi_val_v

  interface vi_val_dv
     module procedure vari_val_dv
     module procedure vari_val_dv_at
  end interface vi_val_dv

  interface vi_adj_v
     module procedure vari_adj_v
     module procedure vari_adj_v_at
  end interface vi_adj_v

  interface vi_adj_dv
     module procedure vari_adj_dv
     module procedure vari_adj_dv_at
  end interface vi_adj_dv

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

  elemental function vari_val(this) result(v)
    implicit none
    type(vari), intent(in) :: this
    type(dual) :: v
    v = this%val_
  end function vari_val

  impure elemental function vari_val_at(ip) result(v)
    implicit none
    integer(ik), intent(in) :: ip
    type(vari), pointer :: this
    type(dual) :: v
    call recover(ip, this)
    v = vari_val(this)
  end function vari_val_at

  elemental real(rk) function real_val(x)
    implicit none
    real(rk), intent(in) :: x
    real_val = x
  end function real_val

  elemental function vari_adj(this) result(v)
    implicit none
    type(vari), intent(in) :: this
    type(dual) :: v
    v = this%adj_
  end function vari_adj

  impure elemental function vari_adj_at(ip) result(v)
    implicit none
    integer(ik), intent(in) :: ip
    type(vari), pointer :: this
    type(dual) :: v
    call recover(ip, this)
    v = vari_adj(this)
  end function vari_adj_at

  elemental function vari_val_v(this) result(v)
    implicit none
    type(vari), intent(in) :: this
    real(rk) :: v
    v = this%val_%v
  end function vari_val_v

  impure elemental function vari_val_v_at(ip) result(v)
    implicit none
    integer(ik), intent(in) :: ip
    type(vari), pointer :: this
    real(rk) :: v
    call recover(ip, this)
    v = vi_val_v(this)
  end function vari_val_v_at

  elemental function vari_val_dv(this) result(v)
    implicit none
    type(vari), intent(in) :: this
    real(rk) :: v
    v = this%val_%dv
  end function vari_val_dv

  impure elemental function vari_val_dv_at(ip) result(v)
    implicit none
    integer(ik), intent(in) :: ip
    type(vari), pointer :: this
    real(rk) :: v
    call recover(ip, this)
    v = vi_val_dv(this)
  end function vari_val_dv_at

  elemental function vari_adj_v(this) result(v)
    implicit none
    type(vari), intent(in) :: this
    real(rk) :: v
    v = this%adj_%v
  end function vari_adj_v

  impure elemental function vari_adj_v_at(ip) result(v)
    implicit none
    integer(ik), intent(in) :: ip
    type(vari), pointer :: this
    real(rk) :: v
    call recover(ip, this)
    v = vi_adj_v(this)
  end function vari_adj_v_at

  elemental function vari_adj_dv(this) result(v)
    implicit none
    type(vari), intent(in) :: this
    real(rk) :: v
    v = this%adj_%dv
  end function vari_adj_dv

  impure elemental function vari_adj_dv_at(ip) result(v)
    implicit none
    integer(ik), intent(in) :: ip
    type(vari), pointer :: this
    real(rk) :: v
    call recover(ip, this)
    v = vi_adj_dv(this)
  end function vari_adj_dv_at

  subroutine new_vari_val(this, val)
    implicit none
    integer(ik), intent(out) :: this
    type(vari), pointer :: vp
    real(rk), intent(in) :: val
    this = add_vari(visize)
    chains(this)%c => chain_base_instance
    call c_f_pointer(c_loc(core_adstack%s_(chains(this)%i)), vp)
    vp = vari(dual(val), dual(0.d0))
  end subroutine new_vari_val

  subroutine new_vari_dual(this, val)
    implicit none
    integer(ik), intent(out) :: this
    type(vari), pointer :: vp
    type(dual), intent(in) :: val
    this = add_vari(visize)
    chains(this)%c => chain_base_instance
    call c_f_pointer(c_loc(core_adstack%s_(chains(this)%i)), vp)
    vp = vari(val, dual(0.d0))
  end subroutine new_vari_dual

  subroutine new_v_vari(this, val, i)
    implicit none
    integer(ik), intent(out) :: this
    integer(ik), intent(in) :: i
    type(dual), intent(in) :: val
    type(v_vari), pointer :: vp
    this = add_vari(v_visize)
    call c_f_pointer(c_loc(core_adstack%s_(chains(this)%i)), vp)
    vp = v_vari(val, dual(0.d0), chains(i)%i)
  end subroutine new_v_vari

  subroutine new_vd_vari(this, val, i, b)
    implicit none
    integer(ik), intent(out) :: this
    integer(ik), intent(in) :: i
    type(dual), intent(in) :: val
    real(rk), intent(in) :: b
    type(vd_vari), pointer :: vp
    this = add_vari(vd_visize)
    call c_f_pointer(c_loc(core_adstack%s_(chains(this)%i)), vp)
    vp = vd_vari(val, dual(0.d0), chains(i)%i, b)
  end subroutine new_vd_vari

  subroutine new_vi_vari(this, val, i, n)
    implicit none
    integer(ik), intent(out) :: this
    integer(ik), intent(in) :: i, n
    type(dual), intent(in) :: val
    type(v_vari), pointer :: vp
    integer(ik), pointer :: ip
    this = add_vari(v_visize)
    call c_f_pointer(c_loc(core_adstack%s_(chains(this)%i)), vp)
    vp = v_vari(val, dual(0.d0), chains(i)%i)
    call c_f_pointer(c_loc(core_adstack%s_(core_adstack%i_)), ip)
    ip = n
    core_adstack%i_ = core_adstack%i_ + iksize
  end subroutine new_vi_vari

  subroutine new_vv_vari(this, val, i, j)
    implicit none
    integer(ik), intent(out) :: this
    integer(ik), intent(in) :: i, j
    type(dual), intent(in) :: val
    type(vv_vari), pointer :: vp
    this = add_vari(vv_visize)
    call c_f_pointer(c_loc(core_adstack%s_(chains(this)%i)), vp)
    vp = vv_vari(val, dual(0.d0), chains(i)%i, chains(j)%i)
  end subroutine new_vv_vari

  subroutine new_vdd_vari(this, val, i, b, c)
    implicit none
    integer(ik), intent(out) :: this
    integer(ik), intent(in) :: i
    type(dual), intent(in) :: val
    type(vdd_vari), pointer :: vp
    real(rk), intent(in) :: b, c
    this = add_vari(vdd_visize)
    call c_f_pointer(c_loc(core_adstack%s_(chains(this)%i)), vp)
    vp = vdd_vari(val, dual(0.d0), chains(i)%i, b, c)
  end subroutine new_vdd_vari

  subroutine new_vvd_vari(this, val, i, j, c)
    implicit none
    integer(ik), intent(out) :: this
    integer(ik), intent(in) :: i, j
    type(dual), intent(in) :: val
    type(vvd_vari), pointer :: vp
    real(rk), intent(in) :: c
    this = add_vari(vvd_visize)
    call c_f_pointer(c_loc(core_adstack%s_(chains(this)%i)), vp)
    vp = vvd_vari(val, dual(0.d0), chains(i)%i, chains(j)%i, c)
  end subroutine new_vvd_vari

  subroutine new_vec_vari(this, val, vec)
    implicit none
    integer(ik), intent(out) :: this
    integer(ik), target, intent(in) :: vec(:)
    type(v_vari), pointer :: vp
    type(dual), intent(in) :: val
    integer(ik), pointer :: ipv(:)
    this = add_vari(v_visize)
    call c_f_pointer(c_loc(core_adstack%s_(chains(this)%i)), vp)
    vp = v_vari(val, dual(0.d0), size(vec))
    call c_f_pointer(c_loc(core_adstack%s_(core_adstack%i_)), ipv, [(size(vec))])
    ipv = vec
    core_adstack%i_ = core_adstack%i_ + iksize*size(vec)
  end subroutine new_vec_vari

  subroutine new_vari_real32(this, val)
    implicit none
    integer(ik), intent(out) :: this
    real(c_float), intent(in) :: val
    call new_vari_val(this, real(val, rk))
  end subroutine new_vari_real32

  ! skip chain and return previous vi in AD stack
  subroutine chain_dummy(p)
    implicit none
    type(vari), pointer :: p
  end subroutine

  subroutine reset_chain(ip)
    implicit none
    integer(ik), intent(in) :: ip
    type(vari), pointer :: p1
    integer(ik) :: j
    do j = ip, 1, -1
       call recover(chains(j)%i, p1)
       p1%val_%dv = 0.d0
       p1%adj_ = dual(0.d0, 0.d0)
    enddo
  end subroutine reset_chain

  subroutine chain(ip)
    implicit none
    integer(ik), intent(in) :: ip
    type(vari), pointer :: p1
    integer(ik) :: j
    call recover(chains(ip)%i, p1)
    p1%adj_ = dual(1.0d0, 0.0d0)
    do j = ip, 1, -1
       call chains(j)%c%chain(chains(j)%i)
    enddo
  end subroutine chain

end module fz_fvari
