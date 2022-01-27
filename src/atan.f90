module atan_op_mod
  use iso_fortran_env
  use vari_mod
  use op_v_mod

  implicit none

contains
  
  subroutine chain_atan(this)
    class(vari), intent(in) :: this
    real(rk) new_adj, x
    integer(ik) :: i(1)
    i = callstack%get_operand_index(this)
    x = callstack%val(i(1))
    new_adj = callstack%adj(i(1)) + this%adj() / (1.d0 + x * x)
    call callstack%set_adj(i(1), new_adj)
  end subroutine chain_atan

  function atan_v(v) result(s)
    type(var), intent(in) :: v
    type(var) :: s
    s = var(atan(v%val()))
    call setup_callstack(s, v, chain_atan)
  end function atan_v

end module atan_op_mod

module atan_mod
  use atan_op_mod
  implicit none

  interface atan
     module procedure atan_v
  end interface atan
end module atan_mod
