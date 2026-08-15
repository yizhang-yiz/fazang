#ifndef DEF_VARI_OP1_DEFINED
#define DEF_VARI_OP1_DEFINED

! OP: use vi as vari arg
#define DEF_OP1(VARITYPE, NAME, OP, DYDX) \
function NAME/**/_vi (vi) result(v1); \
  implicit none; \
  type(VARITYPE), pointer :: vi, v1; \
  v1 = OP; \
  v1%chain = c_funloc( mychain ); \
  call core_adstack%push(vi%i); \
  contains; \
  subroutine mychain (this); \
    implicit none; \
    type(VARITYPE), pointer, intent(in) :: this; \
    type(VARITYPE), pointer :: a; \
    call recover_parent(this, a); \
    a%adj_ = a%adj_ + this%adj_ * DYDX; \
  end subroutine mychain; \
end function

! OP = certain operation using (va%val_, vb%val_ , a, b ...)
#define DEF_OP2_VD(VARITYPE, NAME, OP, DYDX) \
function NAME/**/_vi_d(va, b) result(v1); \
  implicit none; \
  type(VARITYPE), pointer, intent(in) :: va; \
  real(rk), intent(in) :: b; \
  type(VARITYPE), pointer :: v1; \
  v1 = OP; \
  v1%chain = c_funloc( mychain ); \
  call core_adstack%push(va%i, b); \
  contains; \
  subroutine mychain (this); \
    implicit none; \
    type(VARITYPE), pointer :: this, va; \
    real(rk) :: b; \
    call recover_parent_real(this, va, b); \
    va%adj_ = va%adj_ + this%adj_ * DYDX; \
  end subroutine mychain; \
end function

#define DEF_OP2_DV(VARITYPE, NAME, OP, DYDX) \
function NAME/**/_d_vi(a, vb) result(v1); \
  implicit none; \
  type(VARITYPE), pointer, intent(in) :: vb; \
  real(rk), intent(in) :: a; \
  type(VARITYPE), pointer :: v1; \
  v1 = OP; \
  v1%chain = c_funloc( mychain ); \
  call core_adstack%push(vb%i, a); \
  contains; \
  subroutine mychain (this); \
    implicit none; \
    type(VARITYPE), pointer :: this, vb; \
    real(rk) :: a; \
    call recover_parent_real(this, vb, a); \
    vb%adj_ = vb%adj_ + this%adj_ * DYDX; \
  end subroutine mychain; \
end function

#define DEF_OP2_VV(VARITYPE, NAME, OP, DYDA, DYDB) \
function NAME/**/_vi_vi(va, vb) result(v1); \
  implicit none; \
  type(VARITYPE), pointer, intent(in) :: va, vb; \
  type(VARITYPE), pointer :: v1; \
  v1 = OP; \
  v1%chain = c_funloc( mychain ); \
  call core_adstack%push([va%i, vb%i]); \
  contains; \
  subroutine mychain (this); \
    implicit none; \
    type(VARITYPE), pointer, intent(in) :: this; \
    type(VARITYPE), pointer :: va, vb; \
    call recover_parent2(this, va, vb); \
    va%adj_ = va%adj_ + this%adj_ * DYDA; \
    vb%adj_ = vb%adj_ + this%adj_ * DYDB; \
  end subroutine mychain; \
end function

#define DEF_OP2_VI(VARITYPE, NAME, OP, DYDX) \
function NAME/**/_vi_d(va, b) result(v1); \
  implicit none; \
  type(VARITYPE), pointer, intent(in) :: va; \
  integer(ik), intent(in) :: b; \
  type(VARITYPE), pointer :: v1; \
  v1 = OP; \
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
