program cos_test
  use, intrinsic :: iso_fortran_env
  use env_mod
  use test_mod
  use vari_mod
  use cos_mod
  implicit none

  type(vari) :: x, y1, y2, y3
  real(rk) :: z1, z2

  x = vari(1.4d0)
  y1 = cos(x)
  call y1%init_dependent()
  call y1%chain()
  call expect_float_eq(y1%val(), cos(1.4d0))
  call expect_float_eq(y1%adj(), 1.0d0)
  call expect_float_eq(x%adj(), -sin(1.4d0))
  call expect_eq(callstack%head, 3)

  z1 = x%val()
  z2 = cos(z1)
  call expect_float_eq(y1%val(), z2)

  y3 = vari(1.5d0)
  y2 = cos(y1)
  call callstack%set_zero_all_adj()
  call expect_float_eq(y1%adj(), 0.0d0)
  call expect_float_eq(x%adj(), 0.0d0)
  call expect_float_eq(y2%val(), cos(cos(1.4d0)))
  
  call expect_eq(callstack%operand_index_begin(1), 0)
  call expect_eq(callstack%operand_index_begin(2), 1)
  call expect_eq(callstack%operand_index_begin(3), 0)
  call expect_eq(callstack%operand_index_begin(4), 2)
  call expect_eq(callstack%operand_index(1), 1)
  call expect_eq(callstack%operand_index(2), 2)
  call expect_eq(callstack%operand_index(3), 0)
  call expect_eq(callstack%operand_index(4), 0)

  call y2%init_dependent()
  call y2%chain()
  call expect_float_eq(y3%adj(), 0.0d0)
  call expect_float_eq(x%adj(), 0.0d0)
  call expect_float_eq(y2%adj(), 1.0d0)
  call expect_float_eq(y1%adj(), -sin(cos(1.4d0)))

end program cos_test
