module fazang_sqrt_op_mod
  use iso_fortran_env
  use fazang_vari_mod, only : vari, adstack, callstack
  use fazang_var_mod
  use fazang_env_mod

  implicit none

contains
  
  subroutine chain_sqrt(this)
    class(vari), intent(in) :: this
    real(rk) :: new_adj(1)
    new_adj = this%operand_adj()
    new_adj(1) = new_adj(1) + 0.5d0 * this%adj() / this%val()
    call this%set_operand_adj(new_adj)
  end subroutine chain_sqrt

  impure elemental function sqrt_v(v) result(s)
    type(var), intent(in) :: v
    type(var) :: s
    s = var(sqrt(v%val()), [v])
    call s%set_chain(chain_sqrt)
  end function sqrt_v

end module fazang_sqrt_op_mod

module fazang_sqrt_mod
  use fazang_sqrt_op_mod
  implicit none

  interface sqrt
     module procedure sqrt_v
  end interface sqrt
end module fazang_sqrt_mod
