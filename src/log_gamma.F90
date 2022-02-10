module fazang_log_gamma_op_mod
  use iso_fortran_env
  use fazang_vari_mod, only : vari
  use fazang_var_mod
  use fazang_env_mod
  use fazang_digamma_mod

  implicit none

contains
  
  subroutine chain_log_gamma(this)
    class(vari), intent(in) :: this
    real(rk) :: adj(1), val(1)
    val = this%operand_val()
    adj = this%operand_adj() + this%adj() * digamma(val)
    call this%set_operand_adj(adj)
  end subroutine chain_log_gamma

  impure elemental function log_gamma_v(v) result(s)
    type(var), intent(in) :: v
    type(var) :: s
    s = var(log_gamma(v%val()), [v])
    call s%set_chain(chain_log_gamma)
  end function log_gamma_v

end module fazang_log_gamma_op_mod

module fazang_log_gamma_mod
  use fazang_log_gamma_op_mod
  implicit none

  interface log_gamma
     module procedure log_gamma_v
  end interface log_gamma
end module fazang_log_gamma_mod
