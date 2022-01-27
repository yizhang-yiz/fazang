module cos_op_mod
  use iso_fortran_env
  use vari_mod
  use op_v_mod

  implicit none

contains
  
  subroutine chain_cos(this)
    class(vari), intent(in) :: this
    real(rk) new_adj
    integer(ik) :: i(1)
    i = callstack%get_operand_index(this)
    new_adj = callstack%adj(i(1)) - this%adj() * sin(callstack%val(i(1)))
    call callstack%set_adj(i(1), new_adj)
  end subroutine chain_cos

  function cos_v(v) result(s)
    type(var), intent(in) :: v
    type(var) :: s
    s = var(cos(v%val()))
    call setup_callstack(s, v, chain_cos)
  end function cos_v

end module cos_op_mod

module cos_mod
  use cos_op_mod
  implicit none

  interface cos
     module procedure cos_v
  end interface cos
end module cos_mod
