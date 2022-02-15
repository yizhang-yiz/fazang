module fazang_sinh_v_mod
  use iso_fortran_env
  use fazang_env_mod
  use fazang_sub_mod
  use fazang_arith_mod
  use fazang_rel_operator_mod
  use fazang_var_mod, only: var
  use fazang_vari_mod, only: vari, callstack

  implicit none

  private
  public :: sinh_v

contains
  subroutine chain_sinh(this)
    class(vari), intent(in) :: this
    real(rk) :: adj(1)
    real(rk) :: val(1)
    val = this%operand_val()
    adj = this%adj() * cosh(val)
    call this%set_operand_adj(adj)
  end subroutine chain_sinh

  impure elemental function sinh_v(v) result(s)
    type(var), intent(in) :: v
    type(var) :: s
    s = var(sinh(v%val()), [v])
    call s%set_chain(chain_sinh)
  end function sinh_v

end module fazang_sinh_v_mod

module fazang_sinh_mod
  use fazang_sinh_v_mod
  implicit none

  interface sinh
     module procedure sinh_v
  end interface sinh
end module fazang_sinh_mod
