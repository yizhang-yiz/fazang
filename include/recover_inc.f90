#ifndef DEF_RECOVER_DEFINED
#define DEF_RECOVER_DEFINED

#define DEF_RECOVER(v, subtype) \
  subroutine recover(v); \
    implicit none; \
    type(subtype), intent(inout) :: v; \
    if (v%i == 0 .or. (.not. associated(v%stack))) error stop 5555; \
    v = transfer(v%stack(v%i:(v%i+sizeof(v))), v); \
  end subroutine recover

#endif
