module square_mod
  use iso_fortran_env
  use vari_mod, only : vari, adstack, callstack
  use var_mod
  use env_mod

  implicit none

  private
  public :: square

contains
  
  subroutine chain_sq(this)
    class(vari), intent(in) :: this
    real(rk) :: new_adj(1), val(1)
    integer(ik) :: i(1)
    i = this%operand_index()
    new_adj = this%operand_adj()
    val = this%operand_val()
    new_adj(1) = new_adj(1) + this%adj() * 2.d0 * val(1)
    call callstack % stack % set_adj(i(1), new_adj(1))
  end subroutine chain_sq

  impure elemental function square(v) result(s)
    type(var), intent(in) :: v
    type(var) :: s
    s = var(v%val() * v%val(), [v])
    s%vi%chain => chain_sq
  end function square

end module square_mod
