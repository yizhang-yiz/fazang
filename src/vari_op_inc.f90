#ifndef DEF_VARI_OP1_DEFINED
#define DEF_VARI_OP1_DEFINED

! OP: use vi as vari arg
#define DEF_OP1(VARITYPE, NAME, OP, DYDX) \
function NAME/**/_vi (vi) result(v1); \
  implicit none; \
  VARITYPE, pointer :: vi, v1; \
  v1 = OP; \
  v1%chain = c_funloc( mychain ); \
  call core_adstack%push(vi%i); \
  contains; \
  subroutine mychain (this); \
    implicit none; \
    VARITYPE, pointer :: this, a; \
    call recover(a, core_adstack%pop_int(this%i+visize)); \
    a%adj_ = a%adj_ + this%adj_ * DYDX; \
  end subroutine mychain; \
end function

! OP = certain operation using (va%val_, vb%val_ , a, b ...)
#define DEF_OP2_VD(VARITYPE, NAME, OP, DYDX) \
function NAME/**/_vi_d(va, b) result(v1); \
  implicit none; \
  VARITYPE, pointer, intent(in) :: va; \
  real(rk), intent(in) :: b; \
  VARITYPE, pointer :: v1; \
  v1 = OP; \
  v1%chain = c_funloc( mychain ); \
  call core_adstack%push(va%i); \
  call core_adstack%push(b); \
  contains; \
  subroutine mychain (this); \
    implicit none; \
    VARITYPE, pointer :: this, va; \
    real(rk) :: b; \
    call recover(va, core_adstack%pop_int(this%i+visize)); \
    b = core_adstack%pop_real(this%i+visize+iksize); \
    va%adj_ = va%adj_ + this%adj_ * DYDX; \
  end subroutine mychain; \
end function

#define DEF_OP2_DV(VARITYPE, NAME, OP, DYDX) \
function NAME/**/_d_vi(a, vb) result(v1); \
  implicit none; \
  VARITYPE, pointer, intent(in) :: vb; \
  real(rk), intent(in) :: a; \
  VARITYPE, pointer :: v1; \
  v1 = OP; \
  v1%chain = c_funloc( mychain ); \
  call core_adstack%push(vb%i); \
  call core_adstack%push(a); \
  contains; \
  subroutine mychain (this); \
    implicit none; \
    VARITYPE, pointer :: this, vb; \
    real(rk) :: a; \
    call recover(vb, core_adstack%pop_int(this%i+visize)); \
    a = core_adstack%pop_real(this%i+visize+iksize); \
    vb%adj_ = vb%adj_ + this%adj_ * DYDX; \
  end subroutine mychain; \
end function

#define DEF_OP2_VV(VARITYPE, NAME, OP, DYDA, DYDB) \
function NAME/**/_vi_vi(va, vb) result(v1); \
  implicit none; \
  VARITYPE, pointer, intent(in) :: va, vb; \
  VARITYPE, pointer :: v1; \
  v1 = OP; \
  v1%chain = c_funloc( mychain ); \
  call core_adstack%push(va%i); \
  call core_adstack%push(vb%i); \
  contains; \
  subroutine mychain (this); \
    implicit none; \
    VARITYPE, pointer :: this, va, vb; \
    call recover(va, core_adstack%pop_int(this%i+visize)); \
    call recover(vb, core_adstack%pop_int(this%i+visize+iksize)); \
    va%adj_ = va%adj_ + this%adj_ * DYDA; \
    vb%adj_ = vb%adj_ + this%adj_ * DYDB; \
  end subroutine mychain; \
end function

#endif
