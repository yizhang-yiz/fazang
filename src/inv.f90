module fazang_inv_mod
  use iso_fortran_env
  use fazang_vari_mod, only : vari, adstack, callstack
  use fazang_var_mod
  use fazang_env_mod

  implicit none

  private
  public :: inv

contains
  
  subroutine chain_inv(this)
    class(vari), intent(in) :: this
    real(rk) :: new_adj(1), val(1)
    new_adj = this%operand_adj()
    val = this%operand_val()
    new_adj(1) = new_adj(1) - this%adj() / (val(1) * val(1))
    call this%set_operand_adj(new_adj)
  end subroutine chain_inv

  impure elemental function inv(v) result(s)
    type(var), intent(in) :: v
    type(var) :: s
    s = var(1.d0 / v%val(), [v])
    call s%set_chain(chain_inv)
  end function inv

end module fazang_inv_mod
