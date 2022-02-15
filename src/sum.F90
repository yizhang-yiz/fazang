module fazang_sum_op_mod
  use iso_fortran_env
  use fazang_env_mod
  use fazang_var_mod
  use fazang_vari_mod, only: vari, callstack

  implicit none

contains
  
  subroutine chain_sum(this)
    class(vari), intent(in) :: this
    real(rk) :: adj(this%n_operand())
    adj = this%adj()
    call this%set_operand_adj(adj)
  end subroutine chain_sum

  function sum_v(v) result(s)
    type(var), intent(in) :: v(:)
    type(var) :: s
    s = var(sum(val(v)), v)
    call s%set_chain(chain_sum)
  end function sum_v

end module fazang_sum_op_mod

module fazang_sum_mod
  use fazang_sum_op_mod
  implicit none

  interface sum
     module procedure sum_v
  end interface sum
end module fazang_sum_mod
