module fazang_sin_op_mod
  use iso_fortran_env
  use fazang_vari_mod, only : vari
  use fazang_var_mod
  use fazang_env_mod

  implicit none

contains
  
  subroutine chain_sin(this)
    class(vari), intent(in) :: this
    real(rk) :: adj(1), val(1)
    val = this%operand_val()
    adj(1) = this%adj() * cos(val(1))
    call this%set_operand_adj(adj)
  end subroutine chain_sin

  impure elemental function sin_v(v) result(s)
    type(var), intent(in) :: v
    type(var) :: s
    s = var(sin(v%val()), [v])
    call s%set_chain(chain_sin)
  end function sin_v

end module fazang_sin_op_mod

module fazang_sin_mod
  use fazang_sin_op_mod
  implicit none

  interface sin
     module procedure sin_v
  end interface sin
end module fazang_sin_mod
