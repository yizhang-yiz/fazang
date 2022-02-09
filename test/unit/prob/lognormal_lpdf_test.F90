#include "../test_macro.fi"

program lognormal_lpdf_test
  use, intrinsic :: iso_fortran_env
  use fazang_test_mod
  use fazang_grad_mod
  use fazang_env_mod
  use fazang_var_mod
  use fazang_lognormal_lpdf_mod
  use fazang_add_mod
  implicit none

  type(var) :: mu, sigma
  type(var) :: lp1, lp2, lp3, lp4
  real(rk) :: y(2) = [0.8d0, 1.7d0]

  mu = var(1.1d0)
  sigma = var(2.3d0)

  lp1 = lognormal_lpdf([0.8d0], mu, sigma)
  EXPECT_DBL_EQ(lp1%val(), -1.6941775317995158d0)
  lp2 = lognormal_lpdf([1.7d0], mu, sigma)
  EXPECT_DBL_EQ(lp2%val(), -2.3131171348473649d0)

  lp3 = lognormal_lpdf(y, mu, sigma)
  EXPECT_DBL_EQ(lp3%val(), lp1%val() + lp2%val())

  call set_zero_all_adj()
  call lp3%grad()
  EXPECT_DBL_EQ(mu%adj(), -0.357753364886964d0)
  EXPECT_DBL_EQ(sigma%adj(), -0.699030735113594d0)

  lp4 = lognormal_lpdf(y, mu, sigma%val())
  EXPECT_DBL_EQ(lp4%val(), lp1%val() + lp2%val())
  call set_zero_all_adj()
  call lp4%grad()  
  EXPECT_DBL_EQ(mu%adj(), -0.357753364886964d0)

  lp4 = lognormal_lpdf(y, mu%val(), sigma)
  EXPECT_DBL_EQ(lp4%val(), lp1%val() + lp2%val())
  call set_zero_all_adj()
  call lp4%grad()  
  EXPECT_DBL_EQ(sigma%adj(), -0.699030735113594d0)

end program lognormal_lpdf_test
