module op_vv_mod
  use iso_fortran_env
  use vari_mod

  implicit none

  interface setup_callstack
     module procedure setup_callstack_vv
     module procedure setup_callstack_vd
  end interface setup_callstack

contains
  
  subroutine setup_callstack_vv(v, operand1, operand2, proc_chain)
    type(vari), intent(inout) :: v
    type(vari), intent(in) :: operand1, operand2
    procedure(chain_op) :: proc_chain

    call callstack%push_operand(v, operand1)
    call callstack%push_operand(v, operand2)
    v%n_operand = 2
    v % chain => proc_chain
  end subroutine setup_callstack_vv

  subroutine setup_callstack_vd(v, operand1, proc_chain)
    type(vari), intent(inout) :: v
    type(vari), intent(in) :: operand1
    procedure(chain_op) :: proc_chain

    call callstack%push_operand(v, operand1)
    v%n_operand = 1
    v % chain => proc_chain
  end subroutine setup_callstack_vd

end module op_vv_mod
