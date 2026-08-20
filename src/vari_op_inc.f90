#ifndef DEF_VARI_OP1_DEFINED
#define DEF_VARI_OP1_DEFINED

! OP: use vi as vari arg
#define DEF_VARI1_MOD(VARITYPE, NAME, OP, DYDX) \
module NAME/**/_vi_mod; \
  use fz_env; \
  use fz_prim_op; \
  implicit none; \
  type, extends(chain_base) :: vi_chain; \
   contains; \
   procedure, nopass :: chain => chain_impl; \
  end type; \
  type( vi_chain ), target :: vi_chain_instance; \
contains; \
  subroutine chain_impl(ip); \
    implicit none; \
    integer(ik), intent(in) :: ip; \
    type(v_vari), pointer :: this; \
    type(VARITYPE), pointer :: a; \
    call recover(ip, this, a); \
    a%adj_ = a%adj_ + this%adj_ * DYDX; \
  end subroutine; \
  function new_vi (i) result(iout); \
    implicit none; \
    integer(ik), intent(in) :: i; \
    integer(ik) :: iout; \
    type(VARITYPE), pointer :: vi; \
    call c_f_pointer(c_loc(core_adstack%s_(chains(i)%i)), vi); \
    call new_vari(iout, OP, i); \
    chains(iout)%c => vi_chain_instance; \
  end function; \
end module

#define DEF_VARI2_MOD(VARITYPE, NAME, OP, DYDA, DYDB) \
module NAME/**/_vi_mod; \
  use fz_env; \
  use fz_prim_op; \
  implicit none; \
  type, extends(chain_base) :: vd_chain; \
   contains; \
   procedure, nopass :: chain => chain_vd_impl; \
  end type; \
  type( vd_chain ), target :: vd_chain_instance; \
  type, extends(chain_base) :: dv_chain; \
   contains; \
   procedure, nopass :: chain => chain_dv_impl; \
  end type; \
  type( dv_chain ), target :: dv_chain_instance; \
  type, extends(chain_base) :: vv_chain; \
   contains; \
   procedure, nopass :: chain => chain_vv_impl; \
  end type; \
  type( vv_chain ), target :: vv_chain_instance; \
contains; \
  subroutine chain_vd_impl(ip); \
    implicit none; \
    integer(ik), intent(in) :: ip; \
    type(vd_vari), pointer :: this; \
    type(VARITYPE), pointer :: a; \
    real(rk) :: b; \
    call recover(ip, this, a, b); \
    a%adj_ = a%adj_ + this%adj_ * DYDA; \
  end subroutine; \
  subroutine chain_dv_impl(ip); \
    implicit none; \
    integer(ik), intent(in) :: ip; \
    type(vd_vari), pointer :: this; \
    type(VARITYPE), pointer :: b; \
    real(rk) :: a; \
    call recover(ip, this, b, a); \
    b%adj_ = b%adj_ + this%adj_ * DYDB; \
  end subroutine chain_dv_impl; \
  subroutine chain_vv_impl(ip); \
    implicit none; \
    integer(ik), intent(in) :: ip; \
    type(vv_vari), pointer :: this; \
    type(VARITYPE), pointer :: a, b; \
    call recover(ip, this, a, b); \
    a%adj_ = a%adj_ + this%adj_ * DYDA; \
    b%adj_ = b%adj_ + this%adj_ * DYDB; \
  end subroutine chain_vv_impl; \
  function new_vi_d(ia, b) result(iout); \
    implicit none; \
    integer(ik), intent(in) :: ia; \
    real(rk), intent(in) :: b; \
    integer(ik) :: iout; \
    type(VARITYPE), pointer :: a; \
    call c_f_pointer(c_loc(core_adstack%s_(chains(ia)%i)), a); \
    call new_vari(iout, OP, ia, b); \
    chains(iout)%c => vd_chain_instance; \
  end function new_vi_d; \
  function new_d_vi(a, ib) result(iout); \
    implicit none; \
    integer(ik), intent(in) :: ib; \
    real(rk), intent(in) :: a; \
    integer(ik) :: iout; \
    type(VARITYPE), pointer :: b; \
    call c_f_pointer(c_loc(core_adstack%s_(chains(ib)%i)), b); \
    call new_vari(iout, OP, ib, a); \
    chains(iout)%c => dv_chain_instance; \
  end function new_d_vi; \
  function new_vi_vi(ia, ib) result(iout); \
    implicit none; \
    integer(ik), intent(in) :: ia, ib; \
    integer(ik) :: iout; \
    type(VARITYPE), pointer :: a, b; \
    call c_f_pointer(c_loc(core_adstack%s_(chains(ia)%i)), a); \
    call c_f_pointer(c_loc(core_adstack%s_(chains(ib)%i)), b); \
    call new_vari(iout, OP, ia, ib); \
    chains(iout)%c => vv_chain_instance; \
  end function; \
end module

#define DEF_VARI1_INT_MOD(VARITYPE, NAME, OP, DYDX) \
module NAME/**/_vi_mod; \
  use fz_env; \
  use fz_prim_op; \
  implicit none; \
  type, extends(chain_base) :: vi_chain; \
   contains; \
   procedure, nopass :: chain => chain_impl; \
  end type; \
  type( vi_chain ), target :: vi_chain_instance; \
contains; \
  subroutine chain_impl(ip); \
    implicit none; \
    integer(ik), intent(in) :: ip; \
    type(v_vari), pointer :: this; \
    type(vari), pointer :: a; \
    integer(ik) :: n; \
    call recover(ip, this, a, n); \
    a%adj_ = a%adj_ + this%adj_ * DYDX; \
  end subroutine; \
  function new_vi (i, n) result(iout); \
    implicit none; \
    integer(ik), intent(in) :: i, n; \
    integer(ik) :: iout; \
    type(VARITYPE), pointer :: vi; \
    type(c_ptr) :: cp; \
    cp = c_loc(core_adstack%s_(chains(i)%i)); \
    call c_f_pointer(cp, vi); \
    call new_vi_vari(iout, OP, i, n); \
    chains(iout)%c => vi_chain_instance; \
  end function; \
end module

#define DEF_VARI1_REAL_MOD(VARITYPE, NAME, OP, DYDX) \
module NAME/**/_vi_mod; \
  use fz_env; \
  use fz_prim_op; \
  implicit none; \
  type, extends(chain_base) :: vi_chain; \
   contains; \
   procedure, nopass :: chain => chain_impl; \
  end type; \
  type( vi_chain ), target :: vi_chain_instance; \
contains; \
  subroutine chain_impl(ip); \
    implicit none; \
    integer(ik), intent(in) :: ip; \
    type(vd_vari), pointer :: this; \
    type(vari), pointer :: a; \
    real(rk) :: b; \
    call recover(ip, this, a, b); \
    a%adj_ = a%adj_ + this%adj_ * DYDX; \
  end subroutine; \
  function new_vi (i, b) result(iout); \
    implicit none; \
    integer(ik), intent(in) :: i; \
    real(rk), intent(in) :: b; \
    integer(ik) :: iout; \
    type(VARITYPE), pointer :: vi; \
    type(c_ptr) :: cp; \
    cp = c_loc(core_adstack%s_(chains(i)%i)); \
    call c_f_pointer(cp, vi); \
    call new_vari(iout, OP, i, b); \
    chains(iout)%c => vi_chain_instance; \
  end function; \
end module

#define DEF_VARI2_REAL_MOD(VARITYPE, NAME, OP, DYDA, DYDB) \
module NAME/**/_vi_mod; \
  use fz_env; \
  use fz_prim_op; \
  implicit none; \
  type, extends(chain_base) :: vd_chain; \
   contains; \
   procedure, nopass :: chain => chain_vd_impl; \
  end type; \
  type( vd_chain ), target :: vd_chain_instance; \
  type, extends(chain_base) :: dv_chain; \
   contains; \
   procedure, nopass :: chain => chain_dv_impl; \
  end type; \
  type( dv_chain ), target :: dv_chain_instance; \
  type, extends(chain_base) :: vv_chain; \
   contains; \
   procedure, nopass :: chain => chain_vv_impl; \
  end type; \
  type( vv_chain ), target :: vv_chain_instance; \
contains; \
  subroutine chain_vd_impl(ip); \
    implicit none; \
    integer(ik), intent(in) :: ip; \
    type(vdd_vari), pointer :: this; \
    type(VARITYPE), pointer :: a; \
    real(rk) :: b, c; \
    call recover(ip, this, a, b, c); \
    a%adj_ = a%adj_ + this%adj_ * DYDA; \
  end subroutine; \
  subroutine chain_dv_impl(ip); \
    implicit none; \
    integer(ik), intent(in) :: ip; \
    type(vdd_vari), pointer :: this; \
    type(VARITYPE), pointer :: b; \
    real(rk) :: a, c; \
    call recover(ip, this, b, a, c); \
    b%adj_ = b%adj_ + this%adj_ * DYDB; \
  end subroutine chain_dv_impl; \
  subroutine chain_vv_impl(ip); \
    implicit none; \
    integer(ik), intent(in) :: ip; \
    type(vvd_vari), pointer :: this; \
    type(VARITYPE), pointer :: a, b; \
    real(rk) :: c; \
    call recover(ip, this, a, b, c); \
    a%adj_ = a%adj_ + this%adj_ * DYDA; \
    b%adj_ = b%adj_ + this%adj_ * DYDB; \
  end subroutine chain_vv_impl; \
  function new_vi_d(ia, b, c) result(iout); \
    implicit none; \
    integer(ik), intent(in) :: ia; \
    real(rk), intent(in) :: b, c; \
    integer(ik) :: iout; \
    type(VARITYPE), pointer :: a; \
    type(c_ptr) :: cp; \
    cp = c_loc(core_adstack%s_(chains(ia)%i)); \
    call c_f_pointer(cp, a); \
    call new_vari(iout, OP, ia, b, c); \
    chains(iout)%c => vd_chain_instance; \
  end function new_vi_d; \
  function new_d_vi(a, ib, c) result(iout); \
    implicit none; \
    integer(ik), intent(in) :: ib; \
    real(rk), intent(in) :: a, c; \
    integer(ik) :: iout; \
    type(VARITYPE), pointer :: b; \
    type(c_ptr) :: cp; \
    cp = c_loc(core_adstack%s_(chains(ib)%i)); \
    call c_f_pointer(cp, b); \
    call new_vari(iout, OP, ib, a, c); \
    chains(iout)%c => dv_chain_instance; \
  end function new_d_vi; \
  function new_vi_vi(ia, ib, c) result(iout); \
    implicit none; \
    integer(ik), intent(in) :: ia, ib; \
    integer(ik) :: iout; \
    type(VARITYPE), pointer :: a, b; \
    real(rk), intent(in) :: c; \
    type(c_ptr) :: cp; \
    cp = c_loc(core_adstack%s_(chains(ia)%i)); \
    call c_f_pointer(cp, a); \
    cp = c_loc(core_adstack%s_(chains(ib)%i)); \
    call c_f_pointer(cp, b); \
    call new_vari(iout, OP, ia, ib, c); \
    chains(iout)%c => vv_chain_instance; \
  end function; \
end module

#endif
