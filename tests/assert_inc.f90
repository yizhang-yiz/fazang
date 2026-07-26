#ifndef FZ_ASSERT_DEFINED
#define FZ_ASSERT_DEFINED

#define ASSERT_TOL( A, B, TOL ) \
  if (.not. abs( (A) - (B) ) < (TOL) ) then; \
     write(*, *) "test failed at:", __LINE__ ;\
     error stop ;\
  endif

#define ASSERT( COND ) \
  if (.not. (COND) ) then; \
     write(*, *) "test failed at:", __LINE__ ;\
     error stop ;\
  endif

#endif
