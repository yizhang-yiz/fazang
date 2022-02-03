module fazang_square_mod
  use iso_fortran_env
  use fazang_vari_mod, only : vari, adstack, callstack
  use fazang_var_mod
  use fazang_env_mod

  implicit none

  private
  public :: square

contains
  
  subroutine chain_sq(this)
    class(vari), intent(in) :: this
    real(rk) :: new_adj(1), val(1)
    new_adj = this%operand_adj()
    val = this%operand_val()
    new_adj(1) = new_adj(1) + this%adj() * 2.d0 * val(1)
    call this%set_operand_adj(new_adj)
  end subroutine chain_sq

  impure elemental function square(v) result(s)
    type(var), intent(in) :: v
    type(var) :: s
    s = var(v%val() * v%val(), [v])
    call s%set_chain(chain_sq)
  end function square

end module fazang_square_mod
