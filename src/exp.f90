module exp_op_mod
  use iso_fortran_env
  use vari_mod
  use op_v_mod

  implicit none

contains
  
  subroutine chain_exp(this)
    class(vari), intent(in) :: this
    real(rk) new_adj
    integer(ik) :: i(1)
    i = callstack%get_operand_index(this)
    new_adj = callstack%adj(i(1)) + this%adj() * this%val()
    call callstack%set_adj(i(1), new_adj)
  end subroutine chain_exp

  function exp_v(v) result(s)
    type(vari), intent(in) :: v
    type(vari) :: s
    s = vari(exp(v%val()))
    call setup_callstack(s, v, chain_exp)
  end function exp_v

end module exp_op_mod

module exp_mod
  use exp_op_mod
  implicit none

  interface exp
     module procedure exp_v
  end interface exp
end module exp_mod
