module asin_op_mod
  use iso_fortran_env
  use vari_mod
  use op_v_mod

  implicit none

contains
  
  subroutine chain_asin(this)
    class(vari), intent(in) :: this
    real(rk) new_adj, x
    integer(ik) :: i(1)
    i = callstack%get_operand_index(this)
    x = callstack%val(i(1))
    new_adj = callstack%adj(i(1)) + this%adj() / sqrt(1.d0 - x * x)
    call callstack%set_adj(i(1), new_adj)
  end subroutine chain_asin

  function asin_v(v) result(s)
    type(var), intent(in) :: v
    type(var) :: s
    s = var(asin(v%val()))
    call setup_callstack(s, v, chain_asin)
  end function asin_v

end module asin_op_mod

module asin_mod
  use asin_op_mod
  implicit none

  interface asin
     module procedure asin_v
  end interface asin
end module asin_mod
