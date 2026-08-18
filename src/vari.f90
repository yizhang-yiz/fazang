module fz_vari
  use, intrinsic :: iso_fortran_env
  use, intrinsic :: iso_c_binding
  use fz_env

  type, bind(c) :: vari
     real(rk) :: val_
     real(rk) :: adj_ = 0d0
     integer(ik) :: j = 0 ! vari before i in storage (for rev pass)
     type(c_funptr) :: chain = c_null_funptr
  end type vari

  integer(ik), parameter :: visize = storage_size(vari(0.d0))/storage_size(0_c_int8_t)

  interface new_vari
     module procedure new_vari_val
     module procedure new_vari_real32
     module procedure new_vari_int
     module procedure new_vari_int_int
     module procedure new_vari_int2
     module procedure new_vari_int_real
     module procedure new_vari_ivec
  end interface new_vari

  interface recover
     module procedure recover_vari
     module procedure recover_parent
     module procedure recover_parent2
     module procedure recover_parent_real
  end interface recover

  abstract interface
     subroutine chain_op(ip, p)
       import
       implicit none
       integer(ik), intent(in) :: ip
       type(vari), pointer, intent(in) :: p
     end subroutine chain_op
  end interface

contains

  subroutine recover_vari(i, p)
    implicit none
    type(vari), pointer, intent(out) :: p
    integer(ik), intent(in) :: i
    type(c_ptr) :: cp
    p => null()
    if (i > 0) then
       cp = c_loc(core_adstack%s_(i))
       call c_f_pointer(cp, p)
    end if
  end subroutine recover_vari

  subroutine recover_parent(ip, p, p1)
    implicit none
    integer(ik), intent(in) :: ip
    type(vari), pointer, intent(in) :: p
    type(vari), pointer, intent(out) :: p1
    integer(ik), pointer :: i
    type(c_ptr) :: cp
    cp = c_loc(core_adstack%s_(ip + visize))
    call c_f_pointer(cp, i)
    if (i > 0) then
       cp = c_loc(core_adstack%s_(i))
       call c_f_pointer(cp, p1)
    end if
  end subroutine recover_parent

  subroutine recover_parent2(ip, p, p1, p2)
    implicit none
    integer(ik), intent(in) :: ip
    type(vari), pointer, intent(in) :: p
    type(vari), pointer, intent(out) :: p1, p2
    integer(ik), pointer :: i
    type(c_ptr) :: cp
    cp = c_loc(core_adstack%s_(ip + visize))
    call c_f_pointer(cp, i)
    if (i > 0) then
       cp = c_loc(core_adstack%s_(i))
       call c_f_pointer(cp, p1)
    end if
    cp = c_loc(core_adstack%s_(ip+visize+iksize))
    call c_f_pointer(cp, i)
    if (i > 0) then
       cp = c_loc(core_adstack%s_(i))
       call c_f_pointer(cp, p2)
    end if
  end subroutine recover_parent2

  subroutine recover_parent_real(ip, p, p1, b)
    implicit none
    integer(ik), intent(in) :: ip
    type(vari), pointer, intent(in) :: p
    type(vari), pointer, intent(out) :: p1
    real(rk), intent(out) :: b
    integer(ik), pointer :: i
    real(rk), pointer :: pb
    type(c_ptr) :: cp
    cp = c_loc(core_adstack%s_(ip+visize))
    call c_f_pointer(cp, i)
    if (i > 0) then
       cp = c_loc(core_adstack%s_(i))
       call c_f_pointer(cp, p1)
    end if
    cp = c_loc(core_adstack%s_(ip+visize+iksize))
    call c_f_pointer(cp, pb)
    b = pb
  end subroutine recover_parent_real

  impure elemental real(rk) function vi_val(ip)
    implicit none
    integer(ik), intent(in) :: ip
    type(vari), pointer :: this
    call recover(ip, this)
    vi_val = this%val_
  end function vi_val

  impure elemental real(rk) function vi_val_at(i)
    implicit none
    integer(ik), intent(in) :: i
    vi_val_at = vi_val(i)
  end function vi_val_at

  impure elemental real(rk) function vi_adj(ip)
    implicit none
    integer(ik), intent(in) :: ip
    type(vari), pointer :: this
    call recover(ip, this)
    vi_adj = this%adj_
  end function vi_adj

  subroutine new_vari_val(this, val)
    implicit none
    integer(ik), intent(out) :: this
    type(vari), pointer :: vp
    real(rk), intent(in) :: val
    this = core_adstack%i_
    call recover(this, vp)
    vp%val_ = val
    vp%j = core_adstack%j_
    vp%chain = c_funloc(chain_dummy)
    core_adstack%j_ = core_adstack%i_
    core_adstack%i_ = core_adstack%i_ + visize
  end subroutine new_vari_val

  subroutine new_vari_int(this, v1, i, vi)
    implicit none
    integer(ik), intent(out) :: this
    integer(ik), intent(in) :: i
    type(vari), pointer, intent(out) :: v1, vi
    integer(ik), pointer :: ip
    type(c_ptr) :: cp
    this = core_adstack%i_
    cp = c_loc(core_adstack%s_(this))
    call c_f_pointer(cp, v1)
    v1%j = core_adstack%j_
    cp = c_loc(core_adstack%s_(i))
    call c_f_pointer(cp, vi)
    cp = c_loc(core_adstack%s_(this + visize))
    call c_f_pointer(cp, ip)
    ip = i
    core_adstack%j_ = core_adstack%i_
    core_adstack%i_ = core_adstack%i_ + visize + iksize
  end subroutine new_vari_int

  subroutine new_vari_ivec(this, v1, val, vec)
    implicit none
    integer(ik), intent(out) :: this
    integer(ik), target, intent(in) :: vec(:)
    real(rk), intent(in) :: val
    type(vari), pointer, intent(out) :: v1
    integer(ik), pointer :: ip, ipv(:)
    type(c_ptr) :: cp
    this = core_adstack%i_
    cp = c_loc(core_adstack%s_(this))
    call c_f_pointer(cp, v1)
    v1%val_ = val
    v1%j = core_adstack%j_
    cp = c_loc(core_adstack%s_(this + visize))
    ipv => vec
    call c_f_pointer(cp, ipv, [(size(vec))])
    core_adstack%j_ = core_adstack%i_
    core_adstack%i_ = core_adstack%i_ + visize + iksize*size(vec)
  end subroutine new_vari_ivec

  subroutine new_vari_int_int(this, v1, i, vi, j)
    implicit none
    integer(ik), intent(out) :: this
    integer(ik), intent(in) :: i, j
    type(vari), pointer, intent(out) :: v1, vi
    integer(ik), pointer :: ip
    type(c_ptr) :: cp
    this = core_adstack%i_
    cp = c_loc(core_adstack%s_(this))
    call c_f_pointer(cp, v1)
    v1%j = core_adstack%j_
    cp = c_loc(core_adstack%s_(i))
    call c_f_pointer(cp, vi)
    cp = c_loc(core_adstack%s_(this + visize))
    call c_f_pointer(cp, ip)
    ip = i
    cp = c_loc(core_adstack%s_(this + visize + iksize))
    call c_f_pointer(cp, ip)
    ip = j
    core_adstack%j_ = core_adstack%i_
    core_adstack%i_ = core_adstack%i_ + visize + iksize + iksize
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
    this = core_adstack%i_
    cp = c_loc(core_adstack%s_(this))
    call c_f_pointer(cp, v1)
    v1%j = core_adstack%j_
    cp = c_loc(core_adstack%s_(i))
    call c_f_pointer(cp, vi)
    cp = c_loc(core_adstack%s_(this + visize))
    call c_f_pointer(cp, ip)
    ip = i
    cp = c_loc(core_adstack%s_(this + visize + iksize))
    call c_f_pointer(cp, rp)
    rp = b
    core_adstack%j_ = core_adstack%i_
    core_adstack%i_ = core_adstack%i_ + visize + iksize + rksize
  end subroutine new_vari_int_real

  subroutine new_vari_int2(this, v1, ia, va, ib, vb)
    implicit none
    integer(ik), intent(out) :: this
    integer(ik), intent(in) :: ia, ib
    type(vari), pointer, intent(out) :: v1, va, vb
    integer(ik), pointer :: ip
    type(c_ptr) :: cp
    this = core_adstack%i_
    cp = c_loc(core_adstack%s_(this))
    call c_f_pointer(cp, v1)
    v1%j = core_adstack%j_
    cp = c_loc(core_adstack%s_(ia))
    call c_f_pointer(cp, va)
    cp = c_loc(core_adstack%s_(ib))
    call c_f_pointer(cp, vb)
    cp = c_loc(core_adstack%s_(this + visize))
    call c_f_pointer(cp, ip)
    ip = ia
    cp = c_loc(core_adstack%s_(this + visize + iksize))
    call c_f_pointer(cp, ip)
    ip = ib
    core_adstack%j_ = core_adstack%i_
    core_adstack%i_ = core_adstack%i_ + visize + iksize + iksize
  end subroutine new_vari_int2

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
    integer(ik) :: i
    i = ip
    call recover(i, p1)
    do while (associated(p1))
       p1%adj_ = 0.d0
       i = p1%j
       call recover(i, p1)
    enddo
  end subroutine reset_chain

  subroutine chain(ip)
    implicit none
    integer(ik), intent(in) :: ip
    type(vari), pointer :: p1
    procedure(chain_op), pointer :: p1_chain
    integer(ik) :: i
    i = ip
    call recover(i, p1)
    p1%adj_ = 1.0d0
    do while (associated(p1))
       call c_f_procpointer(p1%chain, p1_chain)
       call p1_chain(i, p1)
       i = p1%j
       call recover(i, p1)
    enddo
  end subroutine chain

end module fz_vari
