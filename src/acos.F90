module fazang_acos_op_mod
  use iso_fortran_env
  use fazang_vari_mod, only : vari, adstack, callstack
  use fazang_var_mod, only : var
  use fazang_env_mod

  implicit none

contains
  
  subroutine chain_acos(this)
    class(vari), intent(in) :: this
    real(rk) :: new_adj(1), val(1)
    new_adj = this%operand_adj()
    val = this%operand_val()
    new_adj(1) = new_adj(1) - this%adj() / sqrt(1.d0 - val(1) * val(1))
    call this%set_operand_adj(new_adj)
  end subroutine chain_acos

  impure elemental function acos_v(v) result(s)
    type(var), intent(in) :: v
    type(var) :: s
    s = var(acos(v%val()), [v])
    call s%set_chain(chain_acos)
  end function acos_v

end module fazang_acos_op_mod

module fazang_acos_mod
  use fazang_acos_op_mod
  implicit none

  interface acos
     module procedure acos_v
  end interface acos
end module fazang_acos_mod
