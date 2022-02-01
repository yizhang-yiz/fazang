module asin_op_mod
  use iso_fortran_env
  use vari_mod, only : vari, adstack, callstack
  use var_mod
  use env_mod

  implicit none

contains
  
  subroutine chain_asin(this)
    class(vari), intent(in) :: this
    real(rk) :: new_adj(1), val(1)
    integer(ik) :: i(1)
    i = this%operand_index()
    new_adj = this%operand_adj()
    val = this%operand_val()
    new_adj(1) = new_adj(1) + this%adj() / sqrt(1.d0 - val(1) * val(1))
    call callstack % stack % set_adj(i(1), new_adj(1))
  end subroutine chain_asin

  impure elemental function asin_v(v) result(s)
    type(var), intent(in) :: v
    type(var) :: s
    s = var(asin(v%val()), [v])
    s%vi%chain => chain_asin
  end function asin_v

end module asin_op_mod

module asin_mod
  use asin_op_mod
  implicit none

  interface asin
     module procedure asin_v
  end interface asin
end module asin_mod
