program adstack_test
  use, intrinsic :: iso_fortran_env
  use vari_mod
  use test_mod
  implicit none

  type(adstack) x

  call expect_eq(x%head, 1)

  call x%set_val(0.1d0)
  call x%set_adj(0.5d0)

  call expect_float_eq(x%val(), 0.1d0)
  call expect_float_eq(x%adj(), 0.5d0)
  call expect_eq(x%head, 1)

  call x%push_head()
  call x%set_val(1.1d0)
  call x%set_adj(1.5d0)
  call expect_float_eq(x%val(x%head), 1.1d0)
  call expect_float_eq(x%adj(x%head), 1.5d0)
  call expect_eq(x%head, 2)

  call x%set_adj(1024, 99.d0)
  call expect_float_eq(x%adj(1024), 99.d0)  

end program adstack_test
