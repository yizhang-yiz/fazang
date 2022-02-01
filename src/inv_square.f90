module inv_square_mod
  use iso_fortran_env
  use vari_mod, only : vari, adstack, callstack
  use var_mod
  use env_mod

  implicit none

  private
  public :: inv_square

contains
  
  subroutine chain_isq(this)
    class(vari), intent(in) :: this
    real(rk) :: new_adj(1), val(1)
    integer(ik) :: i(1)
    i = this%operand_index()
    new_adj = this%operand_adj()
    val = this%operand_val()
    new_adj(1) = new_adj(1) - this%adj() * 2.d0 / (val(1) * val(1) * val(1))
    call callstack % stack % set_adj(i(1), new_adj(1))
  end subroutine chain_isq

  impure elemental function inv_square(v) result(s)
    type(var), intent(in) :: v
    type(var) :: s
    s = var(1.d0 / v%val() * v%val(), [v])
    s%vi%chain => chain_isq
  end function inv_square

end module inv_square_mod
