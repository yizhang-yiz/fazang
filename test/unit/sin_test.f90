program sin_test
  use, intrinsic :: iso_fortran_env
  use env_mod
  use test_mod
  use vari_mod
  use sin_mod
  implicit none

  type(vari) :: x, y1, y2, y3, y4
  real(rk) :: z1, z2
  integer i, j

  x = vari(1.4d0)
  y1 = sin(x)
  call y1%init_dependent()
  call y1%chain()
  call expect_float_eq(y1%val(), sin(1.4d0))
  call expect_float_eq(y1%adj(), 1.0d0)
  call expect_float_eq(x%adj(), cos(1.4d0))
  call expect_eq(callstack%head, 3)

  z1 = x%val()
  z2 = sin(z1)
  call expect_float_eq(y1%val(), z2)

  ! write(*, *) "taki test: ", x%val(), y1%val()
  ! write(*, *) "taki test: ", z(1)

end program sin_test
