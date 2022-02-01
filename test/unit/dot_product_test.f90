#include "test_macro.fi"

program dot_product_test
  use, intrinsic :: iso_fortran_env
  use test_mod
  use vari_mod, only : vari, adstack
  use env_mod
  use var_mod
  use grad_mod
  use dot_product_mod
  use add_mod
  implicit none

  type(var) :: x(4), y(4), z
  real(rk) :: a(4) = [1.d0, 47.d0, 3.d0, 53.d0], b

  x = var([1.d0, 2.d0, 3.d0, 4.d0])
  y = var([4.d0, 3.d0, 2.d0, 1.d0])
  z = dot_product(x, y)
  EXPECT_DBL_EQ(z%val(), 20.d0)
  call z%vi%set_adj(2.5d0)
  call z%vi%chain()
  EXPECT_DBL_EQ(z%adj(), 2.5d0)
  EXPECT_DBL_EQ(x(1)%adj(), 2.5d0 * 4.d0)
  EXPECT_DBL_EQ(x(2)%adj(), 2.5d0 * 3.d0)
  EXPECT_DBL_EQ(x(3)%adj(), 2.5d0 * 2.d0)
  EXPECT_DBL_EQ(x(4)%adj(), 2.5d0 * 1.d0)
  EXPECT_DBL_EQ(y(1)%adj(), 2.5d0 * 1.d0)
  EXPECT_DBL_EQ(y(2)%adj(), 2.5d0 * 2.d0)
  EXPECT_DBL_EQ(y(3)%adj(), 2.5d0 * 3.d0)
  EXPECT_DBL_EQ(y(4)%adj(), 2.5d0 * 4.d0)

  call set_zero_all_adj()
  z = dot_product(a, x)
  EXPECT_DBL_EQ(z%val(), dot_product(val(x), a))
  call z%vi%set_adj(2.5d0)
  call z%vi%chain()
  EXPECT_DBL_EQ(z%adj(), 2.5d0)  
  EXPECT_EQ(z%vi%n_operand(), 4)
  EXPECT_DBL_EQ(x(1)%adj(), 2.5d0 * a(1))
  EXPECT_DBL_EQ(x(2)%adj(), 2.5d0 * a(2))
  EXPECT_DBL_EQ(x(3)%adj(), 2.5d0 * a(3))
  EXPECT_DBL_EQ(x(4)%adj(), 2.5d0 * a(4))

  call set_zero_all_adj()
  z = dot_product(y, a)
  EXPECT_DBL_EQ(z%val(), dot_product(val(y), a))
  call z%vi%set_adj(2.5d0)
  call z%vi%chain()
  EXPECT_DBL_EQ(z%adj(), 2.5d0)  
  EXPECT_EQ(z%vi%n_operand(), 4)
  EXPECT_DBL_EQ(y(1)%adj(), 2.5d0 * a(1))
  EXPECT_DBL_EQ(y(2)%adj(), 2.5d0 * a(2))
  EXPECT_DBL_EQ(y(3)%adj(), 2.5d0 * a(3))
  EXPECT_DBL_EQ(y(4)%adj(), 2.5d0 * a(4))

  ! double version should also work
  b = dot_product(a, a)
  EXPECT_DBL_EQ(b, 5028.d0)

end program dot_product_test
