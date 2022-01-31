module fazang
  use env_mod
  use tape_mod
  use vari_mod, only : vari, adstack, callstack
  use var_mod
  use grad_mod

  ! op 1
  use sin_mod
  use cos_mod
  use tan_mod
  use asin_mod
  use acos_mod
  use atan_mod
  use sqrt_mod
  use exp_mod
  use log_mod

  ! op 2
  use rel_operator_mod
  use add_mod
  use sub_mod
  use mul_mod
  use div_mod

  ! op *
  use sum_mod
  use dot_product_mod

  implicit none

end module fazang
