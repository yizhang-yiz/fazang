module inv_sqrt_mod
  use iso_fortran_env
  use vari_mod, only : vari, adstack, callstack
  use var_mod
  use env_mod

  implicit none

  private
  public :: inv_sqrt

contains
  
  subroutine chain_isqrt(this)
    class(vari), intent(in) :: this
    real(rk) :: new_adj(1), val(1)
    integer(ik) :: i(1)
    i = this%operand_index()
    new_adj = this%operand_adj()
    val = this%operand_val()
    new_adj(1) = new_adj(1) - 0.5d0 * this%adj() / (val(1) * sqrt(val(1)))
    call callstack % stack % set_adj(i(1), new_adj(1))
  end subroutine chain_isqrt

  impure elemental function inv_sqrt(v) result(s)
    type(var), intent(in) :: v
    type(var) :: s
    s = var(1.d0 / sqrt(v%val()), [v])
    s%vi%chain => chain_isqrt
  end function inv_sqrt

end module inv_sqrt_mod
