module log_op_mod
  use iso_fortran_env
  use vari_mod
  use op_v_mod

  implicit none

contains
  
  subroutine chain_log(this)
    class(vari), intent(in) :: this
    real(rk) new_adj, x
    integer(ik) :: i(1)
    i = callstack%get_operand_index(this)
    x = callstack%val(i(1))
    new_adj = callstack%adj(i(1)) + this%adj() / x
    call callstack%set_adj(i(1), new_adj)
  end subroutine chain_log

  function log_v(v) result(s)
    type(vari), intent(in) :: v
    type(vari) :: s
    s = vari(log(v%val()))
    call setup_callstack(s, v, chain_log)
  end function log_v

end module log_op_mod

module log_mod
  use log_op_mod
  implicit none

  interface log
     module procedure log_v
  end interface log
end module log_mod
