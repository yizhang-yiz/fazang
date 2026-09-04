#include "assert_inc.f90"

program nest_test
  use fz_var
  implicit none
  real(rk), parameter :: tol = 1.d-14

  type(var) :: a, b, c
  a = 3.d0
  b = 5.d0
  c = b/a
  ASSERT( core_adstack%nvari == 3 )

  block
    type(var) :: a, b, c
    call begin_nest()
    a = 5.d0
    b = 27.d0
    c= a*b
    call grad(c)
    ASSERT_TOL( adj(c), 1.d0, tol )
    ASSERT_TOL( adj(a), val(b), tol )
    ASSERT_TOL( adj(b), val(a), tol )
    ASSERT( core_adstack%nvari == 6 )
    call end_nest()
    ASSERT( core_adstack%nvari == 3 )
  end block

  ASSERT_TOL(adj(a), 0.d0, tol)
  ASSERT_TOL(adj(b), 0.d0, tol)
  ASSERT_TOL(adj(c), 0.d0, tol)

  call grad(c)
  ASSERT_TOL( adj(a), -val(c)/val(a), tol )
  ASSERT_TOL( adj(b), 1.d0/val(a), tol )
  call reset_adj()
  ASSERT( core_adstack%nvari == 3 )

  block
    type(var) :: a, b, c
    call begin_nest()
    a = 5.d0
    b = 27.d0
    c= a*b
    ASSERT( core_adstack%nvari == 6 )
    ASSERT( c%i == 6 )
    block
      type(var) :: d, e, f
      call begin_nest()
      d = 2.d0
      e = 3.d0
      f = exp(d * e)
      ASSERT( f%i == 10 )
      call grad(f)
      ASSERT_TOL( adj(d), val(f)*val(e), tol )
      call reset_adj(f)
      ASSERT_TOL( adj(d), 0.d0, tol )
      call end_nest
    end block

    call grad(c)
    ASSERT_TOL( adj(c), 1.d0, tol )
    ASSERT_TOL( adj(a), val(b), tol )
    ASSERT_TOL( adj(b), val(a), tol )
    ASSERT( core_adstack%nvari == 6 )

    call end_nest()
    ASSERT( core_adstack%nvari == 3 )
  end block

  call grad(c)
  ASSERT_TOL( adj(a), -val(c)/val(a), tol )
  ASSERT_TOL( adj(b), 1.d0/val(a), tol )
  ASSERT(core_adstack%i_ == visize + visize + vv_visize + 1)

end program nest_test
