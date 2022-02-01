module atan_op_mod
  use iso_fortran_env
  use vari_mod, only : vari, adstack, callstack
  use var_mod
  use env_mod

  implicit none

contains
  
  subroutine chain_atan(this)
    class(vari), intent(in) :: this
    real(rk) :: new_adj(1), val(1)
    integer(ik) :: i(1)
    i = this%operand_index()
    new_adj = this%operand_adj()
    val = this%operand_val()
    new_adj(1) = new_adj(1) + this%adj() / (1.d0 + val(1) * val(1))
    call callstack % stack % set_adj(i(1), new_adj(1))
  end subroutine chain_atan
  
  impure elemental function atan_v(v) result(s)
    type(var), intent(in) :: v
    type(var) :: s
    s = var(atan(v%val()), [v])
    s%vi%chain => chain_atan
  end function atan_v

end module atan_op_mod

module atan_mod
  use atan_op_mod
  implicit none

  interface atan
     module procedure atan_v
  end interface atan
end module atan_mod
