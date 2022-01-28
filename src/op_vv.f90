module op_vv_mod
  use iso_fortran_env
  use var_mod, only : var
  use vari_mod

  implicit none

  interface setup_callstack
     module procedure setup_callstack_vv
     module procedure setup_callstack_v
     module procedure setup_callstack_vd
  end interface setup_callstack

contains
  
  subroutine setup_callstack_vv(v, operand1, operand2, proc_chain)
    type(var), intent(inout) :: v
    type(var), intent(in) :: operand1, operand2
    procedure(chain_op) :: proc_chain

    call callstack%push_operand(v%vi, operand1%vi)
    call callstack%push_operand(v%vi, operand2%vi)
    v%vi% n_operand = 2
    v%vi% chain => proc_chain
  end subroutine setup_callstack_vv

  subroutine setup_callstack_v(v, operand1, proc_chain)
    type(var), intent(inout) :: v
    type(var), intent(in) :: operand1
    procedure(chain_op) :: proc_chain

    call callstack%push_operand(v%vi, operand1%vi)
    v%vi % n_operand = 1
    v%vi % chain => proc_chain
  end subroutine setup_callstack_v

  subroutine setup_callstack_vd(v, operand1, operand2, proc_chain)
    type(var), intent(inout) :: v
    type(var), intent(in) :: operand1
    real(rk), intent(in) :: operand2
    procedure(chain_op) :: proc_chain

    call callstack%push_operand(v%vi, operand1%vi)
    call callstack%push_operand(v%vi, operand2)
    v%vi % n_operand = 1
    v%vi % n_operand_r = 1
    v%vi % chain => proc_chain
  end subroutine setup_callstack_vd

end module op_vv_mod
