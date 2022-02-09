module fazang
  use fazang_env_mod
  use fazang_arith_mod

  ! core data structure
  use fazang_tape_mod
  use fazang_nested_tape_mod
  use fazang_vari_mod, only : vari, adstack, callstack
  use fazang_var_mod
  use fazang_value_of_mod

  ! gradient & jacobian
  use fazang_grad_mod

  ! op 1
  use fazang_sin_mod
  use fazang_cos_mod
  use fazang_tan_mod
  use fazang_asin_mod
  use fazang_acos_mod
  use fazang_atan_mod
  use fazang_sqrt_mod
  use fazang_exp_mod
  use fazang_log_mod
  use fazang_inv_mod
  use fazang_square_mod
  use fazang_inv_square_mod
  use fazang_inv_sqrt_mod
  use fazang_logit_mod
  use fazang_inv_logit_mod
  use fazang_erf_mod
  use fazang_erfc_mod
  use fazang_sinh_mod
  use fazang_cosh_mod
  use fazang_tanh_mod
  use fazang_asinh_mod
  use fazang_acosh_mod
  use fazang_atanh_mod

  ! op 2
  use fazang_rel_operator_mod
  use fazang_add_mod
  use fazang_sub_mod
  use fazang_mul_mod
  use fazang_div_mod
  use fazang_pow_mod
  use fazang_hypot_mod

  ! op *
  use fazang_sum_mod
  use fazang_norm2_mod
  use fazang_log_sum_exp_mod
  use fazang_dot_product_mod
  use fazang_matmul_mod

  ! prob
  use fazang_normal_lpdf_mod
  use fazang_lognormal_lpdf_mod

  implicit none

end module fazang
