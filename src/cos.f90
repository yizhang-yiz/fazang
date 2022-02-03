module fazang_cos_op_mod
  use iso_fortran_env
  use fazang_vari_mod, only: vari, adstack, callstack
  use fazang_var_mod
  use fazang_env_mod

  implicit none

contains
  
  subroutine chain_cos(this)
    class(vari), intent(in) :: this
    real(rk) :: new_adj(1), val(1)
    new_adj = this%operand_adj()
    val = this%operand_val()
    new_adj(1) = new_adj(1) - this%adj() * sin(val(1))
    call this%set_operand_adj(new_adj)
  end subroutine chain_cos

  impure elemental function cos_v(v) result(s)
    type(var), intent(in) :: v
    type(var) :: s
    s = var(cos(v%val()), [v])
    call s%set_chain(chain_cos)
  end function cos_v

end module fazang_cos_op_mod

module fazang_cos_mod
  use fazang_cos_op_mod
  implicit none

  interface cos
     module procedure cos_v
  end interface cos
end module fazang_cos_mod
