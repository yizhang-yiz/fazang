#include "test_macro.fi"

program digamma_test
  use, intrinsic :: iso_fortran_env
  use fazang_test_mod
  use fazang_digamma_mod
  implicit none

  EXPECT_DBL_EQ(digamma(-0.3d0), 2.113309779635398718584726d0)
  EXPECT_DBL_EQ(digamma(0.3d0), -3.502524222200132988964494d0)
  EXPECT_DBL_EQ(digamma(2.3d0), 0.6000398803639695751380696d0)
  EXPECT_DBL_EQ(digamma(3.0d0), 0.9227843350984671393934879d0)
  EXPECT_DBL_EQ(digamma(5.0d0), 1.5061176684318004727268212d0)
  EXPECT_DBL_EQ(digamma(2000.0d0), 7.6006524387087495489711445d0)
  EXPECT_DBL_EQ(digamma(888000.0d0), 13.69672645891113837300161d0)

end program digamma_test
