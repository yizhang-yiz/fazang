#ifndef DEF_VARI_OP1_DEFINED
#define DEF_VARI_OP1_DEFINED

! OP: use vi as vari arg
#define DEF_VARI1_MOD(VARITYPE, NAME, OP, DYDX) \
module NAME/**/_vi_mod; \
  use fz_env; \
  use fz_real_op; \
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
    type(VARITYPE), pointer :: this, a; \
    call recover(ip, this, a); \
    a%adj_ = a%adj_ + this%adj_ * DYDX; \
  end subroutine; \
  function new_vi (i) result(iout); \
    implicit none; \
    integer(ik), intent(in) :: i; \
    integer(ik) :: iout; \
    integer(ik), pointer :: ip; \
    type(VARITYPE), pointer :: vi, v1; \
    call new_vari(iout, v1, i, vi); \
    v1%val_ = OP; \
    chains(iout)%c => vi_chain_instance; \
  end function; \
end module

#define DEF_VARI2_MOD(VARITYPE, NAME, OP, DYDA, DYDB) \
module NAME/**/_vi_mod; \
  use fz_env; \
  use fz_vari; \
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
    type(VARITYPE), pointer :: this, a; \
    real(rk) :: b; \
    call recover(ip, this, a, b); \
    a%adj_ = a%adj_ + this%adj_ * DYDA; \
  end subroutine; \
  subroutine chain_dv_impl(ip); \
    implicit none; \
    integer(ik), intent(in) :: ip; \
    type(VARITYPE), pointer :: this, b; \
    real(rk) :: a; \
    call recover(ip, this, b, a); \
    b%adj_ = b%adj_ + this%adj_ * DYDB; \
  end subroutine chain_dv_impl; \
  subroutine chain_vv_impl(ip); \
    implicit none; \
    integer(ik), intent(in) :: ip; \
    type(VARITYPE), pointer :: this, a, b; \
    call recover(ip, this, a, b); \
    a%adj_ = a%adj_ + this%adj_ * DYDA; \
    b%adj_ = b%adj_ + this%adj_ * DYDB; \
  end subroutine chain_vv_impl; \
  function new_vi_d(ia, b) result(iout); \
    implicit none; \
    integer(ik), intent(in) :: ia; \
    real(rk), intent(in) :: b; \
    integer(ik) :: iout; \
    type(VARITYPE), pointer :: a, v1; \
    call new_vari(iout, v1, ia, a, b); \
    v1%val_ = OP; \
    chains(iout)%c => vd_chain_instance; \
  end function new_vi_d; \
  function new_d_vi(a, ib) result(iout); \
    implicit none; \
    integer(ik), intent(in) :: ib; \
    real(rk), intent(in) :: a; \
    integer(ik) :: iout; \
    type(VARITYPE), pointer :: b, v1; \
    call new_vari(iout, v1, ib, b, a); \
    v1%val_ = OP; \
    chains(iout)%c => dv_chain_instance; \
  end function new_d_vi; \
  function new_vi_vi(ia, ib) result(iout); \
    implicit none; \
    integer(ik), intent(in) :: ia, ib; \
    integer(ik) :: iout; \
    type(VARITYPE), pointer :: a, b, v1; \
    call new_vari(iout, v1, ia, a, ib, b); \
    v1%val_ = OP; \
    chains(iout)%c => vv_chain_instance; \
  end function; \
end module


! OP = certain operation using (va%val_, vb%val_ , a, b ...)

! #define DEF_OP2_VI(VARITYPE, NAME, OP, DYDX) \
! function NAME/**/_vi_d(ia, b) result(iout); \
!   implicit none; \
!   integer(ik), intent(in) :: ia; \
!   integer(ik), intent(in) :: b; \
!   type(VARITYPE), pointer :: va, v1; \
!   call new_vari(iout, v1, ia, va, b); \
!   v1%val_ = OP; \
!   v1%chain = c_funloc( mychain ); \
!   call core_adstack%push([va%i, b]); \
!   contains; \
!   subroutine mychain (this); \
!     implicit none; \
!     type(VARITYPE), pointer :: this, va; \
!     integer(ik) :: b, i, j; \
!     i = this%i+visize; \
!     call core_adstack%pop(i, j); \
!     call core_adstack%pop(i, b); \
!     call recover(va, j); \
!     va%adj_ = va%adj_ + this%adj_ * DYDX; \
!   end subroutine mychain; \
! end function

! #define DEF_OP3_VDD(VARITYPE, NAME, OP, DYDX) \
! function NAME/**/_vi_d_d(va, b, c) result(v1); \
!   implicit none; \
!   type(VARITYPE), pointer, intent(in) :: va; \
!   real(rk), intent(in) :: b, c; \
!   type(VARITYPE), pointer :: v1; \
!   v1 = OP; \
!   v1%chain = c_funloc( mychain ); \
!   call core_adstack%push(va%i); \
!   call core_adstack%push(b); \
!   call core_adstack%push(c); \
!   contains; \
!   subroutine mychain (this); \
!     implicit none; \
!     type(VARITYPE), pointer :: this, va; \
!     real(rk) :: b, c; \
!     integer(ik) :: i, j; \
!     i = this%i+visize; \
!     call core_adstack%pop(i, j); \
!     call recover(va, j); \
!     call core_adstack%pop(i, b); \
!     call core_adstack%pop(i, c); \
!     va%adj_ = va%adj_ + this%adj_ * DYDX; \
!   end subroutine mychain; \
! end function

! #define DEF_OP3_DVD(VARITYPE, NAME, OP, DYDX) \
! function NAME/**/_d_vi_d(a, vb, c) result(v1); \
!   implicit none; \
!   type(VARITYPE), pointer, intent(in) :: vb; \
!   real(rk), intent(in) :: a, c; \
!   type(VARITYPE), pointer :: v1; \
!   v1 = OP; \
!   v1%chain = c_funloc( mychain ); \
!   call core_adstack%push(vb%i); \
!   call core_adstack%push(a); \
!   call core_adstack%push(c); \
!   contains; \
!   subroutine mychain (this); \
!     implicit none; \
!     type(VARITYPE), pointer :: this, vb; \
!     real(rk) :: a, c; \
!     integer(ik) :: i, j; \
!     i = this%i+visize; \
!     call core_adstack%pop(i, j); \
!     call recover(vb, j); \
!     call core_adstack%pop(i, a); \
!     call core_adstack%pop(i, c); \
!     vb%adj_ = vb%adj_ + this%adj_ * DYDX; \
!   end subroutine mychain; \
! end function

! #define DEF_OP3_VVD(VARITYPE, NAME, OP, DYDA, DYDB) \
! function NAME/**/_vi_vi_d(va, vb, c) result(v1); \
!   implicit none; \
!   type(VARITYPE), pointer, intent(in) :: va, vb; \
!   real(rk), intent(in) :: c; \
!   type(VARITYPE), pointer :: v1; \
!   v1 = OP; \
!   v1%chain = c_funloc( mychain ); \
!   call core_adstack%push(va%i); \
!   call core_adstack%push(vb%i); \
!   call core_adstack%push(c); \
!   contains; \
!   subroutine mychain (this); \
!     implicit none; \
!     type(VARITYPE), pointer :: this, va, vb; \
!     real(rk) :: c; \
!     integer(ik) :: i, j; \
!     i = this%i+visize; \
!     call core_adstack%pop(i, j); \
!     call recover(va, j); \
!     call core_adstack%pop(i, j); \
!     call recover(vb, j); \
!     call core_adstack%pop(i, c); \
!     va%adj_ = va%adj_ + this%adj_ * DYDA; \
!     vb%adj_ = vb%adj_ + this%adj_ * DYDB; \
!   end subroutine mychain; \
! end function

#endif
