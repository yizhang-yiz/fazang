module fazang
  use fz_env, only : ik, rk, eps, log_eps, reboot_chain
  use fz_var, only: var,new_var_val,new_var_real32,set_var,&
    var_val           => val,&
    var_adj           => adj,&
    var_add_dv        => add_dv,&
    var_add_vd        => add_vd,&
    var_add_vv        => add_vv,&
    var_pos_v         => pos_v,&
    var_sub_dv        => sub_dv,&
    var_sub_vd        => sub_vd,&
    var_sub_vv        => sub_vv,&
    var_neg_v         => neg_v,&
    var_mul_dv        => mul_dv,&
    var_mul_vd        => mul_vd,&
    var_mul_vv        => mul_vv,&
    var_div_dv        => div_dv,&
    var_div_vd        => div_vd,&
    var_div_vv        => div_vv,&
    var_pow_dv        => pow_dv,&
    var_pow_vd        => pow_vd,&
    var_pow_vv        => pow_vv,&
    var_grad_of       => grad_of,&
    var_grad_all      => grad_all,&
    reset_adj_from,&
    reset_all_adj,&
    var_exp_v         => exp_v,&
    var_log_v         => log_v,&
    var_log10_v       => log10_v,&
    var_sin_v         => sin_v,&
    var_cos_v         => cos_v,&
    var_tan_v         => tan_v,&
    var_asin_v        => asin_v,&
    var_acos_v        => acos_v,&
    var_atan_v        => atan_v,&
    var_sqrt_v        => sqrt_v,&
    var_sinh_v        => sinh_v,&
    var_cosh_v        => cosh_v,&
    var_tanh_v        => tanh_v,&
    var_logit_d       => logit_d,&
    var_logit_v       => logit_v,&
    var_inv_logit_d   => inv_logit_d,&
    var_inv_logit_v   => inv_logit_v,&
    var_sum_v         => sum_v

  use fz_fvar, only: fvar, new_fvar_val,new_fvar_real32,set_fvar,&
    val_dv, adj_dv, &
    fvar_val           => val,&
    fvar_adj           => adj,&
    fvar_add_dv        => add_dv,&
    fvar_add_vd        => add_vd,&
    fvar_add_vv        => add_vv,&
    fvar_pos_v         => pos_v,&
    fvar_sub_dv        => sub_dv,&
    fvar_sub_vd        => sub_vd,&
    fvar_sub_vv        => sub_vv,&
    fvar_neg_v         => neg_v,&
    fvar_mul_dv        => mul_dv,&
    fvar_mul_vd        => mul_vd,&
    fvar_mul_vv        => mul_vv,&
    fvar_div_dv        => div_dv,&
    fvar_div_vd        => div_vd,&
    fvar_div_vv        => div_vv,&
    fvar_grad_of       => grad_of,&
    fvar_grad_all      => grad_all,&
    reset_from,&
    reset_all_deriv,&
    init_deriv,&
    fvar_exp_v         => exp_v,&
    fvar_log_v         => log_v,&
    fvar_log10_v       => log10_v,&
    fvar_sin_v         => sin_v,&
    fvar_cos_v         => cos_v,&
    fvar_tan_v         => tan_v,&
    fvar_asin_v        => asin_v,&
    fvar_acos_v        => acos_v,&
    fvar_atan_v        => atan_v,&
    fvar_sqrt_v        => sqrt_v
    ! fvar_sinh_v        => sinh_v,&
    ! fvar_cosh_v        => cosh_v,&
    ! fvar_tanh_v        => tanh_v,&
    ! fvar_logit_d       => logit_d,&
    ! fvar_logit_v       => logit_v,&
    ! fvar_inv_logit_d   => inv_logit_d,&
    ! fvar_inv_logit_v   => inv_logit_v,&
    ! fvar_sum_v         => sum_v

  use fz_eval


  ! cvodes solve from sundials
#ifdef USE_SUNDIALS
  use fazang_cvodes_mod
  use fazang_cvodes_model_mod
  use fazang_cvodes_options_mod
#endif

  implicit none

  interface val
     module procedure var_val
     module procedure fvar_val
  end interface val

  interface adj
     module procedure var_adj
     module procedure fvar_adj
  end interface adj

  interface assignment(=)
     module procedure new_var_val
     module procedure new_var_real32
     module procedure set_var
     module procedure new_fvar_val
     module procedure new_fvar_real32
     module procedure set_fvar
  end interface assignment(=)

  interface operator(+)
     module procedure var_add_dv
     module procedure var_add_vd
     module procedure var_add_vv
     module procedure var_pos_v
     module procedure fvar_add_dv
     module procedure fvar_add_vd
     module procedure fvar_add_vv
     module procedure fvar_pos_v
  end interface operator(+)

  interface operator(-)
     module procedure var_sub_dv
     module procedure var_sub_vd
     module procedure var_sub_vv
     module procedure var_neg_v
     module procedure fvar_sub_dv
     module procedure fvar_sub_vd
     module procedure fvar_sub_vv
     module procedure fvar_neg_v
  end interface operator(-)

  interface operator(*)
     module procedure var_mul_dv
     module procedure var_mul_vd
     module procedure var_mul_vv
     module procedure fvar_mul_dv
     module procedure fvar_mul_vd
     module procedure fvar_mul_vv
  end interface operator(*)

  interface operator(**)
     module procedure var_pow_dv
     module procedure var_pow_vd
     module procedure var_pow_vv
  end interface operator(**)

  interface operator(/)
     module procedure var_div_dv
     module procedure var_div_vd
     module procedure var_div_vv
     module procedure fvar_div_dv
     module procedure fvar_div_vd
     module procedure fvar_div_vv
  end interface operator(/)

  interface grad
     module procedure var_grad_of
     module procedure fvar_grad_of
     module procedure var_grad_all
  end interface grad

  interface reset_adj
     module procedure reset_adj_from
     module procedure reset_all_adj
  end interface

  interface reset_deriv
     module procedure reset_from
     module procedure reset_all_deriv
  end interface

  interface exp   ; module procedure var_exp_v;   module procedure fvar_exp_v;   end interface
  interface log   ; module procedure var_log_v;   module procedure fvar_log_v;   end interface
  interface log10 ; module procedure var_log10_v; module procedure fvar_log10_v; end interface
  interface sin   ; module procedure var_sin_v;   module procedure fvar_sin_v;   end interface
  interface cos   ; module procedure var_cos_v;   module procedure fvar_cos_v;   end interface
  interface tan   ; module procedure var_tan_v;   module procedure fvar_tan_v;   end interface
  interface asin  ; module procedure var_asin_v;  module procedure fvar_asin_v;  end interface
  interface acos  ; module procedure var_acos_v;  module procedure fvar_acos_v;  end interface
  interface atan  ; module procedure var_atan_v;  module procedure fvar_atan_v;  end interface
  interface sqrt  ; module procedure var_sqrt_v;  module procedure fvar_sqrt_v;  end interface
  interface sinh; module procedure var_sinh_v; end interface
  interface cosh; module procedure var_cosh_v; end interface
  interface tanh; module procedure var_tanh_v; end interface
  interface sum; module procedure var_sum_v; end interface

  interface logit
     module procedure var_logit_d
     module procedure var_logit_v
  end interface

  interface inv_logit
     module procedure var_inv_logit_d
     module procedure var_inv_logit_v
  end interface

end module fazang
