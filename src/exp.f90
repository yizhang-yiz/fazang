module exp_op_mod
  use iso_fortran_env
  use vari_mod, only : vari, adstack, callstack
  use var_mod
  use env_mod

  implicit none

contains
  
  subroutine chain_exp(this)
    class(vari), intent(in) :: this
    real(rk) :: new_adj(1)
    new_adj = this%operand_adj()
    new_adj(1) = new_adj(1) + this%adj() * this%val()
    call this%set_operand_adj(new_adj)
  end subroutine chain_exp

  impure elemental function exp_v(v) result(s)
    type(var), intent(in) :: v
    type(var) :: s
    s = var(exp(v%val()), [v])
    call s%set_chain(chain_exp)
  end function exp_v

end module exp_op_mod

module exp_mod
  use exp_op_mod
  implicit none

  interface exp
     module procedure exp_v
  end interface exp
end module exp_mod
