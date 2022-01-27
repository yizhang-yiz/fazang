module op_v_mod
  use iso_fortran_env
  use var_mod, only : var
  use vari_mod

  implicit none

contains
  
  subroutine setup_callstack(v, operand, proc_chain)
    type(var), intent(inout) :: v
    type(var), intent(in) :: operand
    procedure(chain_op) :: proc_chain

    call callstack%push_operand(v%vi, operand%vi)
    v % vi % n_operand = 1
    v % vi % chain => proc_chain
  end subroutine setup_callstack

end module op_v_mod
