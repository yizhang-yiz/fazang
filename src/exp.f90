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
    integer(ik) :: i(1)
    i = this%operand_index()
    new_adj = this%operand_adj()
    new_adj(1) = new_adj(1) + this%adj() * this%val()
    call callstack % stack % set_adj(i(1), new_adj(1))
  end subroutine chain_exp

  function exp_v(v) result(s)
    type(var), intent(in) :: v
    type(var) :: s
    s = var(exp(v%val()), [v])
    s%vi%chain => chain_exp
  end function exp_v

end module exp_op_mod

module exp_mod
  use exp_op_mod
  implicit none

  interface exp
     module procedure exp_v
  end interface exp
end module exp_mod
