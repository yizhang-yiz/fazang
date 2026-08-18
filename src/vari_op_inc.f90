#ifndef DEF_VARI_OP1_DEFINED
#define DEF_VARI_OP1_DEFINED

! OP: use vi as vari arg
#define DEF_OP1(VARITYPE, NAME, OP, DYDX) \
function NAME/**/_vi (i) result(iout); \
  implicit none; \
  integer(ik), intent(in) :: i; \
  integer(ik) :: iout; \
  integer(ik), pointer :: ip; \
  type(VARITYPE), pointer :: vi, v1; \
  call new_vari(iout, v1, i, vi); \
  v1%val_ = OP; \
  v1%chain = c_funloc( mychain ); \
  contains; \
  subroutine mychain (ip, this); \
    implicit none; \
    integer(ik), intent(in) :: ip; \
    type(VARITYPE), pointer :: this, a; \
    call recover(ip, a); \
    a%adj_ = a%adj_ + this%adj_ * DYDX; \
  end subroutine; \
end function

! OP = certain operation using (va%val_, vb%val_ , a, b ...)
#define DEF_OP2_VD(VARITYPE, NAME, OP, DYDX) \
function NAME/**/_vi_d(ia, b) result(iout); \
  implicit none; \
  integer(ik), intent(in) :: ia; \
  real(rk), intent(in) :: b; \
  integer(ik) :: iout; \
  type(VARITYPE), pointer :: va, v1; \
  call new_vari(iout, v1, ia, va, b); \
  v1%val_ = OP; \
  v1%chain = c_funloc( mychain ); \
  contains; \
  subroutine mychain (ip, this); \
    implicit none; \
    integer(ik), intent(in) :: ip; \
    type(VARITYPE), pointer :: this, va; \
    real(rk) :: b; \
    call recover(ip, this, va, b); \
    va%adj_ = va%adj_ + this%adj_ * DYDX; \
  end subroutine mychain; \
end function

#define DEF_OP2_DV(VARITYPE, NAME, OP, DYDX) \
function NAME/**/_d_vi(a, ib) result(iout); \
  implicit none; \
  integer(ik), intent(in) :: ib; \
  real(rk), intent(in) :: a; \
  integer(ik) :: iout; \
  type(VARITYPE), pointer :: vb, v1; \
  call new_vari(iout, v1, ib, vb, a); \
  v1%val_ = OP; \
  v1%chain = c_funloc( mychain ); \
  contains; \
  subroutine mychain (ip, this); \
    implicit none; \
    integer(ik), intent(in) :: ip; \
    type(VARITYPE), pointer :: this, vb; \
    real(rk) :: a; \
    call recover(ip, this, vb, a); \
    vb%adj_ = vb%adj_ + this%adj_ * DYDX; \
  end subroutine mychain; \
end function

#define DEF_OP2_VV(VARITYPE, NAME, OP, DYDA, DYDB) \
function NAME/**/_vi_vi(ia, ib) result(iout); \
  implicit none; \
  integer(ik), intent(in) :: ia, ib; \
  integer(ik) :: iout; \
  type(VARITYPE), pointer :: va, vb, v1; \
  call new_vari(iout, v1, ia, va, ib, vb); \
  v1%val_ = OP; \
  v1%chain = c_funloc( mychain ); \
  contains; \
  subroutine mychain (ip, this); \
    implicit none; \
    integer(ik), intent(in) :: ip; \
    type(VARITYPE), pointer :: this, va, vb; \
    call recover(ip, this, va, vb); \
    va%adj_ = va%adj_ + this%adj_ * DYDA; \
    vb%adj_ = vb%adj_ + this%adj_ * DYDB; \
  end subroutine mychain; \
end function

#define DEF_OP2_VI(VARITYPE, NAME, OP, DYDX) \
function NAME/**/_vi_d(ia, b) result(iout); \
  implicit none; \
  integer(ik), intent(in) :: ia; \
  integer(ik), intent(in) :: b; \
  type(VARITYPE), pointer :: va, v1; \
  call new_vari(iout, v1, ia, va, b); \
  v1%val_ = OP; \
  v1%chain = c_funloc( mychain ); \
  call core_adstack%push([va%i, b]); \
  contains; \
  subroutine mychain (this); \
    implicit none; \
    type(VARITYPE), pointer :: this, va; \
    integer(ik) :: b, i, j; \
    i = this%i+visize; \
    call core_adstack%pop(i, j); \
    call core_adstack%pop(i, b); \
    call recover(va, j); \
    va%adj_ = va%adj_ + this%adj_ * DYDX; \
  end subroutine mychain; \
end function

#define DEF_OP3_VDD(VARITYPE, NAME, OP, DYDX) \
function NAME/**/_vi_d_d(va, b, c) result(v1); \
  implicit none; \
  type(VARITYPE), pointer, intent(in) :: va; \
  real(rk), intent(in) :: b, c; \
  type(VARITYPE), pointer :: v1; \
  v1 = OP; \
  v1%chain = c_funloc( mychain ); \
  call core_adstack%push(va%i); \
  call core_adstack%push(b); \
  call core_adstack%push(c); \
  contains; \
  subroutine mychain (this); \
    implicit none; \
    type(VARITYPE), pointer :: this, va; \
    real(rk) :: b, c; \
    integer(ik) :: i, j; \
    i = this%i+visize; \
    call core_adstack%pop(i, j); \
    call recover(va, j); \
    call core_adstack%pop(i, b); \
    call core_adstack%pop(i, c); \
    va%adj_ = va%adj_ + this%adj_ * DYDX; \
  end subroutine mychain; \
end function

#define DEF_OP3_DVD(VARITYPE, NAME, OP, DYDX) \
function NAME/**/_d_vi_d(a, vb, c) result(v1); \
  implicit none; \
  type(VARITYPE), pointer, intent(in) :: vb; \
  real(rk), intent(in) :: a, c; \
  type(VARITYPE), pointer :: v1; \
  v1 = OP; \
  v1%chain = c_funloc( mychain ); \
  call core_adstack%push(vb%i); \
  call core_adstack%push(a); \
  call core_adstack%push(c); \
  contains; \
  subroutine mychain (this); \
    implicit none; \
    type(VARITYPE), pointer :: this, vb; \
    real(rk) :: a, c; \
    integer(ik) :: i, j; \
    i = this%i+visize; \
    call core_adstack%pop(i, j); \
    call recover(vb, j); \
    call core_adstack%pop(i, a); \
    call core_adstack%pop(i, c); \
    vb%adj_ = vb%adj_ + this%adj_ * DYDX; \
  end subroutine mychain; \
end function

#define DEF_OP3_VVD(VARITYPE, NAME, OP, DYDA, DYDB) \
function NAME/**/_vi_vi_d(va, vb, c) result(v1); \
  implicit none; \
  type(VARITYPE), pointer, intent(in) :: va, vb; \
  real(rk), intent(in) :: c; \
  type(VARITYPE), pointer :: v1; \
  v1 = OP; \
  v1%chain = c_funloc( mychain ); \
  call core_adstack%push(va%i); \
  call core_adstack%push(vb%i); \
  call core_adstack%push(c); \
  contains; \
  subroutine mychain (this); \
    implicit none; \
    type(VARITYPE), pointer :: this, va, vb; \
    real(rk) :: c; \
    integer(ik) :: i, j; \
    i = this%i+visize; \
    call core_adstack%pop(i, j); \
    call recover(va, j); \
    call core_adstack%pop(i, j); \
    call recover(vb, j); \
    call core_adstack%pop(i, c); \
    va%adj_ = va%adj_ + this%adj_ * DYDA; \
    vb%adj_ = vb%adj_ + this%adj_ * DYDB; \
  end subroutine mychain; \
end function

#endif
