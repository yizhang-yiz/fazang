module tan_op_mod
  use iso_fortran_env
  use vari_mod, only : vari, adstack, callstack
  use var_mod
  use env_mod

  implicit none

contains
  
  subroutine chain_tan(this)
    class(vari), intent(in) :: this
    real(rk) :: new_adj(1)
    integer(ik) :: i(1)
    i = this%operand_index()
    new_adj = this%operand_adj()
    new_adj(1) = new_adj(1) + this%adj() * (1.d0 + this%val() * this%val())
    call callstack % stack % set_adj(i(1), new_adj(1))
  end subroutine chain_tan

  impure elemental function tan_v(v) result(s)
    type(var), intent(in) :: v
    type(var) :: s
    s = var(tan(v%val()), [v])
    s%vi%chain => chain_tan
  end function tan_v

end module tan_op_mod

module tan_mod
  use tan_op_mod
  implicit none

  interface tan
     module procedure tan_v
  end interface tan
end module tan_mod
