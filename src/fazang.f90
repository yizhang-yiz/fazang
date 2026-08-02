module fazang
  use fz_env
  use fz_var
  use fz_eval


  ! cvodes solve from sundials
#ifdef USE_SUNDIALS
  use fazang_cvodes_mod
  use fazang_cvodes_model_mod
  use fazang_cvodes_options_mod
#endif

  implicit none

end module fazang
