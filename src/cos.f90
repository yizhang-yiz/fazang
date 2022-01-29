module cos_op_mod
  use iso_fortran_env
  use vari_mod, only: vari, adstack, callstack
  use var_mod
  use env_mod

  implicit none

contains
  
  subroutine chain_cos(this)
    class(vari), intent(in) :: this
    real(rk) :: new_adj(1), val(1)
    integer(ik) :: i(1)
    i = this%operand_index()
    new_adj = this%operand_adj()
    val = this%operand_val()
    new_adj(1) = new_adj(1) - this%adj() * sin(val(1))
    call callstack % stack % set_adj(i(1), new_adj(1))
  end subroutine chain_cos

  function cos_v(v) result(s)
    type(var), intent(in) :: v
    type(var) :: s
    s = var(cos(v%val()), [v])
    s%vi%chain => chain_cos
  end function cos_v

end module cos_op_mod

module cos_mod
  use cos_op_mod
  implicit none

  interface cos
     module procedure cos_v
  end interface cos
end module cos_mod
