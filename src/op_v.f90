module op_v_mod
  use iso_fortran_env
  use vari_mod

  implicit none

contains
  
  subroutine setup_callstack(v, operand, proc_chain)
    type(vari), intent(inout) :: v
    type(vari), intent(in) :: operand
    procedure(chain_op) :: proc_chain

    call callstack%push_operand(v, operand)
    v%n_operand = 1
    v % chain => proc_chain
  end subroutine setup_callstack

end module op_v_mod
