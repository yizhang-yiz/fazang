#ifndef FZ_ASSERT_DEFINED
#define FZ_ASSERT_DEFINED

#define ASSERT_TOL( A, B, TOL ) \
  if (.not. abs( (A) - (B) ) < (TOL) ) then; \
     write(*, *) "test failed at:", __LINE__ ;\
     error stop ;\
  endif

#endif

program fz_op1_test
  use fazang_env
  use fazang_var

  implicit none

  type(var) :: a, b, c, d
  integer(ik) :: i, j
  integer(ik) :: k

  a = 0.5d0
  b = exp(a)
  c = sin(b)
  call grad(c)

  ASSERT_TOL( val(c), dsin(dexp(val(a))), 1.d-12 )
  ASSERT_TOL( adj(a), -0.1283465274185981d0, 1.d-12 )

  d = log(cos(c))
  call reset()
  ASSERT_TOL( adj(d), 0.d0, 1.d-12 )
  ASSERT_TOL( adj(c), 0.d0, 1.d-12 )
  ASSERT_TOL( adj(a), 0.d0, 1.d-12 )

  call grad(d)
  ASSERT_TOL( val(d), dlog(dcos(val(c))), 1.d-12 )
  ASSERT_TOL( adj(a), 0.198559967222446865d0, 1.d-12 )

end program fz_op1_test
