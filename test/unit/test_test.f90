#include "test_macro.fi"

program test_basic
  
  use fazang_test_mod
  implicit none

  EXPECT_NEAR(1.0d0, 1.001d0, 0.01d0)
  EXPECT_FLOAT_EQ(1.0d0, 1.0d0)
  EXPECT_TRUE(.true.)
  EXPECT_EQ(1, 1)

end program test_basic
