module fazang_inv_square_mod
  use iso_fortran_env
  use fazang_vari_mod, only : vari, adstack, callstack
  use fazang_var_mod
  use fazang_env_mod

  implicit none

  private
  public :: inv_square

contains
  
  subroutine chain_isq(this)
    class(vari), intent(in) :: this
    real(rk) :: adj(1), val(1)
    val = this%operand_val()
    adj(1) = - this%adj() * 2.d0 / (val(1) * val(1) * val(1))
    call this%set_operand_adj(adj)
  end subroutine chain_isq

  impure elemental function inv_square(v) result(s)
    type(var), intent(in) :: v
    type(var) :: s
    s = var(1.d0 / v%val() * v%val(), [v])
    call s%set_chain(chain_isq)
  end function inv_square

end module fazang_inv_square_mod
