module fazang_asinh_v_mod
  use iso_fortran_env
  use fazang_env_mod
  use fazang_sub_mod
  use fazang_arith_mod
  use fazang_rel_operator_mod
  use fazang_var_mod, only: var
  use fazang_vari_mod, only: vari, callstack

  implicit none

  private
  public :: asinh_v

contains
  subroutine chain_asinh(this)
    class(vari), intent(in) :: this
    real(rk) :: adj(1)
    real(rk) :: val(1)
    val = this%operand_val()
    adj = this%adj() / sqrt(val * val + 1.d0)
    call this%set_operand_adj(adj)
  end subroutine chain_asinh

  impure elemental function asinh_v(v) result(s)
    type(var), intent(in) :: v
    type(var) :: s
    s = var(asinh(v%val()), [v])
    call s%set_chain(chain_asinh)
  end function asinh_v

end module fazang_asinh_v_mod

module fazang_asinh_mod
  use fazang_asinh_v_mod
  implicit none

  interface asinh
     module procedure asinh_v
  end interface asinh
end module fazang_asinh_mod
