module fazang_inv_sqrt_mod
  use iso_fortran_env
  use fazang_vari_mod, only : vari, adstack, callstack
  use fazang_var_mod
  use fazang_env_mod

  implicit none

  private
  public :: inv_sqrt

contains
  
  subroutine chain_isqrt(this)
    class(vari), intent(in) :: this
    real(rk) :: adj(1), val(1)
    val = this%operand_val()
    adj(1) = - 0.5d0 * this%adj() / (val(1) * sqrt(val(1)))
    call this%set_operand_adj(adj)
  end subroutine chain_isqrt

  impure elemental function inv_sqrt(v) result(s)
    type(var), intent(in) :: v
    type(var) :: s
    s = var(1.d0 / sqrt(v%val()), [v])
    call s%set_chain(chain_isqrt)
  end function inv_sqrt

end module fazang_inv_sqrt_mod
