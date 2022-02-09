#include "../test_macro.fi"

program normal_lpdf_test
  use, intrinsic :: iso_fortran_env
  use fazang_test_mod
  use fazang_grad_mod
  use fazang_env_mod
  use fazang_var_mod
  use fazang_normal_lpdf_mod
  use fazang_add_mod
  implicit none

  type(var) :: mu, sigma
  type(var) :: lp1, lp2, lp3, lp4
  real(rk) :: y(2) = [0.8d0, -0.7d0]

  mu = var(1.1d0)
  sigma = var(2.3d0)

  lp1 = normal_lpdf([0.8d0], mu, sigma)
  EXPECT_DBL_EQ(lp1%val(), -1.7603542723968655d0)
  lp2 = normal_lpdf([-0.7d0], mu, sigma)
  EXPECT_DBL_EQ(lp2%val(), -2.0580858413949752d0)

  lp3 = normal_lpdf(y, mu, sigma)
  EXPECT_DBL_EQ(lp3%val(), lp1%val() + lp2%val())

  call lp1%grad()
  EXPECT_DBL_EQ(mu%adj(), -0.056710775047259d0)
  EXPECT_DBL_EQ(sigma%adj(), -0.427385551080792d0)

  call set_zero_all_adj()
  call lp3%grad()  
  EXPECT_DBL_EQ(mu%adj(), -0.396975425330813d0)
  EXPECT_DBL_EQ(sigma%adj(), -0.595874085641489d0)

  lp4 = normal_lpdf(y, mu, sigma%val())
  EXPECT_DBL_EQ(lp4%val(), lp1%val() + lp2%val())
  call set_zero_all_adj()
  call lp4%grad()  
  EXPECT_DBL_EQ(mu%adj(), -0.396975425330813d0)

  lp4 = normal_lpdf(y, mu%val(), sigma)
  EXPECT_DBL_EQ(lp4%val(), lp1%val() + lp2%val())
  call set_zero_all_adj()
  call lp4%grad()  
  EXPECT_DBL_EQ(sigma%adj(), -0.595874085641489d0)

end program normal_lpdf_test
