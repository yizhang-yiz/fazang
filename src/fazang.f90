module fazang
  use fz_env, only : ik, rk, eps, log_eps, pi, reboot_chain
  use fz_var

  ! cvodes solve from sundials
#ifdef USE_SUNDIALS
  use fz_cvodes_mod
  use fz_cvodes_model_mod
  use fz_cvodes_options_mod
#endif

  implicit none

end module fazang
