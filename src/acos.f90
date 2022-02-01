module acos_op_mod
  use iso_fortran_env
  use vari_mod, only : vari, adstack, callstack
  use var_mod, only : var
  use env_mod

  implicit none

contains
  
  subroutine chain_acos(this)
    class(vari), intent(in) :: this
    real(rk) :: new_adj(1), val(1)
    integer(ik) :: i(1)
    i = this%operand_index()
    new_adj = this%operand_adj()
    val = this%operand_val()
    new_adj(1) = new_adj(1) - this%adj() / sqrt(1.d0 - val(1) * val(1))
    call callstack % stack % set_adj(i(1), new_adj(1))
  end subroutine chain_acos

  impure elemental function acos_v(v) result(s)
    type(var), intent(in) :: v
    type(var) :: s
    s = var(acos(v%val()), [v])
    s%vi%chain => chain_acos
  end function acos_v

end module acos_op_mod

module acos_mod
  use acos_op_mod
  implicit none

  interface acos
     module procedure acos_v
  end interface acos
end module acos_mod
