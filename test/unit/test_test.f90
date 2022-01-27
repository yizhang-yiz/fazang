program test_basic
  
  use test_mod
  implicit none

  call expect_near(1.0d0, 1.001d0, 0.01d0)
  call expect_float_eq(1.0d0, 1.0d0)
  call expect_true(.true.)
  call expect_eq(1, 1)

end program test_basic
