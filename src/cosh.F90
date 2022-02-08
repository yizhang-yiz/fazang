module fazang_cosh_v_mod
  use iso_fortran_env
  use fazang_env_mod
  use fazang_sub_mod
  use fazang_arith_mod
  use fazang_rel_operator_mod
  use fazang_var_mod, only: var
  use fazang_vari_mod, only: vari, callstack

  implicit none

  private
  public :: cosh_v

contains
  subroutine chain_cosh(this)
    class(vari), intent(in) :: this
    real(rk) :: adj(1)
    real(rk) :: val(1)
    val = this%operand_val()
    adj = this%operand_adj()
    adj = adj + this%adj() * sinh(val)
    call this%set_operand_adj(adj)
  end subroutine chain_cosh

  impure elemental function cosh_v(v) result(s)
    type(var), intent(in) :: v
    type(var) :: s
    s = var(cosh(v%val()), [v])
    call s%set_chain(chain_cosh)
  end function cosh_v

end module fazang_cosh_v_mod

module fazang_cosh_mod
  use fazang_cosh_v_mod
  implicit none

  interface cosh
     module procedure cosh_v
  end interface cosh
end module fazang_cosh_mod
