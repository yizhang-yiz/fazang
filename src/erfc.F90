module fazang_erfc_op_mod
  use iso_fortran_env
  use fazang_vari_mod, only : vari
  use fazang_var_mod
  use fazang_arith_mod
  use fazang_env_mod

  implicit none

contains
  
  subroutine chain_erfc(this)
    class(vari), intent(in) :: this
    real(rk) :: new_adj(1), val(1)
    new_adj = this%operand_adj()
    val = this%operand_val()
    new_adj(1) = new_adj(1) - this%adj() * two_over_sqrt_pi * exp(-val(1)*val(1))
    call this%set_operand_adj(new_adj)
  end subroutine chain_erfc

  impure elemental function erfc_v(v) result(s)
    type(var), intent(in) :: v
    type(var) :: s
    s = var(erfc(v%val()), [v])
    call s%set_chain(chain_erfc)
  end function erfc_v

end module fazang_erfc_op_mod

module fazang_erfc_mod
  use fazang_erfc_op_mod
  implicit none

  interface erfc
     module procedure erfc_v
  end interface erfc
end module fazang_erfc_mod
