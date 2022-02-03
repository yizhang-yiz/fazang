module fazang_arith_mod
  use, intrinsic :: iso_fortran_env
  use fazang_env_mod
  use fazang_var_mod, only : var
  
  implicit none

contains
  elemental logical function is_inf(d)
    real(rk), intent(in) :: d
    is_inf = d > huge(d)
  end function is_inf

  elemental logical function is_neg_inf(d)
    real(rk), intent(in) :: d
    is_neg_inf = d < -huge(d)
  end function is_neg_inf

  elemental logical function is_nan(d)
    real(rk), intent(in) :: d
    is_nan = d /= d
  end function is_nan


end module fazang_arith_mod
