module sqrt_op_mod
  use iso_fortran_env
  use vari_mod
  use op_v_mod

  implicit none

contains
  
  subroutine chain_sqrt(this)
    class(vari), intent(in) :: this
    real(rk) new_adj
    integer(ik) :: i(1)
    i = callstack%get_operand_index(this)
    new_adj = callstack%adj(i(1)) + 0.5d0 * this%adj() / this%val()
    call callstack%set_adj(i(1), new_adj)
  end subroutine chain_sqrt

  function sqrt_v(v) result(s)
    type(var), intent(in) :: v
    type(var) :: s
    s = var(sqrt(v%val()))
    call setup_callstack(s, v, chain_sqrt)
  end function sqrt_v

end module sqrt_op_mod

module sqrt_mod
  use sqrt_op_mod
  implicit none

  interface sqrt
     module procedure sqrt_v
  end interface sqrt
end module sqrt_mod
