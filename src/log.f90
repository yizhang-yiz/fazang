module log_op_mod
  use iso_fortran_env
  use vari_mod, only : vari, adstack, callstack
  use var_mod
  use env_mod

  implicit none

contains
  
  subroutine chain_log(this)
    class(vari), intent(in) :: this
    real(rk) :: new_adj(1), val(1)
    integer(ik) :: i(1)
    i = this%operand_index()
    new_adj = this%operand_adj()
    val = this%operand_val()
    new_adj(1) = new_adj(1) + this%adj() / val(1)
    call callstack % stack % set_adj(i(1), new_adj(1))
  end subroutine chain_log

  impure elemental function log_v(v) result(s)
    type(var), intent(in) :: v
    type(var) :: s
    s = var(log(v%val()), [v])
    s%vi%chain => chain_log
  end function log_v

end module log_op_mod

module log_mod
  use log_op_mod
  implicit none

  interface log
     module procedure log_v
  end interface log
end module log_mod
