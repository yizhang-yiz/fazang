module sin_op_mod
  use iso_fortran_env
  use vari_mod
  use op_v_mod

  implicit none

contains
  
  subroutine chain_sin(this)
    class(vari), intent(in) :: this
    real(rk) new_adj
    integer(ik) :: i(1)
    i = callstack%get_operand_index(this)
    new_adj = callstack%adj(i(1)) + this%adj() * cos(callstack%val(i(1)))
    call callstack%set_adj(i(1), new_adj)
  end subroutine chain_sin

  function sin_v(v) result(s)
    type(vari), intent(in) :: v
    type(vari) :: s
    s = vari(sin(v%val()))
    call setup_callstack(s, v, chain_sin)
  end function sin_v

end module sin_op_mod

module sin_mod
  use sin_op_mod
  implicit none

  interface sin
     module procedure sin_v
  end interface sin
end module sin_mod
