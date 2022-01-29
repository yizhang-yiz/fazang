module sin_op_mod
  use iso_fortran_env
  use vari_mod, only : vari, adstack, callstack
  use var_mod
  use env_mod

  implicit none

contains
  
  subroutine chain_sin(this)
    class(vari), intent(in) :: this
    real(rk) :: new_adj(1), val(1)
    integer(ik) :: i(1)
    i = this%operand_index()
    new_adj = this%operand_adj()
    val = this%operand_val()
    new_adj(1) = new_adj(1) + this%adj() * cos(val(1))
    call callstack % stack % set_adj(i(1), new_adj(1))
  end subroutine chain_sin

  function sin_v(v) result(s)
    type(var), intent(in) :: v
    type(var) :: s
    s = var(sin(v%val()), [v])
    s%vi%chain => chain_sin
  end function sin_v

end module sin_op_mod

module sin_mod
  use sin_op_mod
  implicit none

  interface sin
     module procedure sin_v
  end interface sin
end module sin_mod
