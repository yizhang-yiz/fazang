module fazang_log_op_mod
  use iso_fortran_env
  use fazang_vari_mod, only : vari, adstack, callstack
  use fazang_var_mod
  use fazang_env_mod

  implicit none

contains
  
  subroutine chain_log(this)
    class(vari), intent(in) :: this
    real(rk) :: new_adj(1), val(1)
    new_adj = this%operand_adj()
    val = this%operand_val()
    new_adj(1) = new_adj(1) + this%adj() / val(1)
    call this%set_operand_adj(new_adj)
  end subroutine chain_log

  impure elemental function log_v(v) result(s)
    type(var), intent(in) :: v
    type(var) :: s
    s = var(log(v%val()), [v])
    call s%set_chain(chain_log)
  end function log_v

end module fazang_log_op_mod

module fazang_log_mod
  use fazang_log_op_mod
  implicit none

  interface log
     module procedure log_v
  end interface log
end module fazang_log_mod
