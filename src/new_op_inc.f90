#ifndef DEF_NEW_OP_DEFINED
#define DEF_NEW_OP_DEFINED

#define NEW_OP(NAME, OP, JAC) \
function NAME(x) result(v); \
  use fz_vari_builder; \
  implicit none; \
  type(var) :: v; \
  type(var), intent(in) :: x(:); \
  call set_val_no_args(OP, x, v); \
  v%p%chain = c_funloc(mychain); \
contains; \
  subroutine mychain (this); \
    use fz_vari_builder; \
    implicit none; \
    type(vari), pointer :: this; \
    type(vari_builder) :: builder; \
    builder = vari_builder(this); \
    call set_adj_jac_no_args(JAC, this%adj_, builder%pv, builder%nv); \
  end subroutine mychain; \
end function

#endif
