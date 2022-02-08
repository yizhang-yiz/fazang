module fazang_acosh_v_mod
  use iso_fortran_env
  use fazang_env_mod
  use fazang_sub_mod
  use fazang_arith_mod
  use fazang_rel_operator_mod
  use fazang_var_mod, only: var
  use fazang_vari_mod, only: vari, callstack

  implicit none

  private
  public :: acosh_v

contains
  subroutine chain_acosh(this)
    class(vari), intent(in) :: this
    real(rk) :: adj(1)
    real(rk) :: val(1)
    val = this%operand_val()
    adj = this%operand_adj()
    adj = adj + this%adj() / sqrt(val * val - 1.d0)
    call this%set_operand_adj(adj)
  end subroutine chain_acosh

  impure elemental function acosh_v(v) result(s)
    type(var), intent(in) :: v
    type(var) :: s
    if ( is_nan(v) ) then
       s = v
    elseif ( v <= 1.d0 ) then
       error stop
    else
       s = var(acosh(v%val()), [v])
       call s%set_chain(chain_acosh)
    endif
  end function acosh_v

end module fazang_acosh_v_mod

module fazang_acosh_mod
  use fazang_acosh_v_mod
  implicit none

  interface acosh
     module procedure acosh_v
  end interface acosh
end module fazang_acosh_mod
