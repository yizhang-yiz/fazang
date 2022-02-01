module inv_mod
  use iso_fortran_env
  use vari_mod, only : vari, adstack, callstack
  use var_mod
  use env_mod

  implicit none

  private
  public :: inv

contains
  
  subroutine chain_inv(this)
    class(vari), intent(in) :: this
    real(rk) :: new_adj(1), val(1)
    integer(ik) :: i(1)
    i = this%operand_index()
    new_adj = this%operand_adj()
    val = this%operand_val()
    new_adj(1) = new_adj(1) - this%adj() / (val(1) * val(1))
    call callstack % stack % set_adj(i(1), new_adj(1))
  end subroutine chain_inv

  impure elemental function inv(v) result(s)
    type(var), intent(in) :: v
    type(var) :: s
    s = var(1.d0 / v%val(), [v])
    s%vi%chain => chain_inv
  end function inv

end module inv_mod
