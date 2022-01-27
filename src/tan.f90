module tan_op_mod
  use iso_fortran_env
  use vari_mod
  use op_v_mod

  implicit none

contains
  
  subroutine chain_tan(this)
    class(vari), intent(in) :: this
    real(rk) new_adj
    integer(ik) :: i(1)
    i = callstack%get_operand_index(this)
    new_adj = callstack%adj(i(1)) + this%adj() * (1.d0 + this%val() * this%val())
    call callstack%set_adj(i(1), new_adj)
  end subroutine chain_tan

  function tan_v(v) result(s)
    type(vari), intent(in) :: v
    type(vari) :: s
    s = vari(tan(v%val()))
    call setup_callstack(s, v, chain_tan)
  end function tan_v

end module tan_op_mod

module tan_mod
  use tan_op_mod
  implicit none

  interface tan
     module procedure tan_v
  end interface tan
end module tan_mod
