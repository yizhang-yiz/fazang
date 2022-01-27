module acos_op_mod
  use iso_fortran_env
  use vari_mod
  use var_mod, only : var
  use op_v_mod

  implicit none

contains
  
  subroutine chain_acos(this)
    class(vari), intent(in) :: this
    real(rk) new_adj, x
    integer(ik) :: i(1)
    i = callstack%get_operand_index(this)
    x = callstack%val(i(1))
    new_adj = callstack%adj(i(1)) - this%adj() / sqrt(1.d0 - x * x)
    call callstack%set_adj(i(1), new_adj)
  end subroutine chain_acos

  function acos_v(v) result(s)
    type(var), intent(in) :: v
    type(var) :: s
    s = var(acos(v%val()))
    call setup_callstack(s, v, chain_acos)
  end function acos_v

end module acos_op_mod

module acos_mod
  use acos_op_mod
  implicit none

  interface acos
     module procedure acos_v
  end interface acos
end module acos_mod
