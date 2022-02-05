module fazang_erf_op_mod
  use iso_fortran_env
  use fazang_vari_mod, only : vari
  use fazang_var_mod
  use fazang_arith_mod
  use fazang_env_mod

  implicit none

contains
  
  subroutine chain_erf(this)
    class(vari), intent(in) :: this
    real(rk) :: new_adj(1), val(1)
    new_adj = this%operand_adj()
    val = this%operand_val()
    new_adj(1) = new_adj(1) + this%adj() * two_over_sqrt_pi * exp(-val(1)*val(1))
    call this%set_operand_adj(new_adj)
  end subroutine chain_erf

  impure elemental function erf_v(v) result(s)
    type(var), intent(in) :: v
    type(var) :: s
    s = var(erf(v%val()), [v])
    call s%set_chain(chain_erf)
  end function erf_v

end module fazang_erf_op_mod

module fazang_erf_mod
  use fazang_erf_op_mod
  implicit none

  interface erf
     module procedure erf_v
  end interface erf
end module fazang_erf_mod
