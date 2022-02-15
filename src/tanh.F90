module fazang_tanh_v_mod
  use iso_fortran_env
  use fazang_env_mod
  use fazang_sub_mod
  use fazang_arith_mod
  use fazang_rel_operator_mod
  use fazang_var_mod, only: var
  use fazang_vari_mod, only: vari, callstack

  implicit none

  private
  public :: tanh_v

contains
  subroutine chain_tanh(this)
    class(vari), intent(in) :: this
    real(rk) :: adj(1)
    real(rk) :: val(1)
    val = this%operand_val()
    val = cosh(val)
    adj = this%adj() / (val * val)
    call this%set_operand_adj(adj)
  end subroutine chain_tanh

  impure elemental function tanh_v(v) result(s)
    type(var), intent(in) :: v
    type(var) :: s
    s = var(tanh(v%val()), [v])
    call s%set_chain(chain_tanh)
  end function tanh_v

end module fazang_tanh_v_mod

module fazang_tanh_mod
  use fazang_tanh_v_mod
  implicit none

  interface tanh
     module procedure tanh_v
  end interface tanh
end module fazang_tanh_mod
