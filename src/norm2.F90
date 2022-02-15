module fazang_norm2_op_mod
  use iso_fortran_env
  use fazang_env_mod
  use fazang_var_mod
  use fazang_vari_mod, only: vari, callstack

  implicit none

contains
  
  subroutine chain_norm2(this)
    class(vari), intent(in) :: this
    real(rk) :: adj(this%n_operand()), val(this%n_operand())
    val = this%operand_val()
    adj = this%adj() * val / this%val()
    call this%set_operand_adj(adj)
  end subroutine chain_norm2

  function norm2_v(v) result(s)
    type(var), intent(in) :: v(:)
    type(var) :: s
    s = var(norm2(val(v)), v)
    call s%set_chain(chain_norm2)
  end function norm2_v

end module fazang_norm2_op_mod

module fazang_norm2_mod
  use fazang_norm2_op_mod
  implicit none

  interface norm2
     module procedure norm2_v
  end interface norm2
end module fazang_norm2_mod
