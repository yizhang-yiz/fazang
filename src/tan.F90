module fazang_tan_op_mod
  use iso_fortran_env
  use fazang_vari_mod, only : vari, adstack, callstack
  use fazang_var_mod
  use fazang_env_mod

  implicit none

contains
  
  subroutine chain_tan(this)
    class(vari), intent(in) :: this
    real(rk) :: adj(1)
    adj = this%adj() * (1.d0 + this%val() * this%val())
    call this%set_operand_adj(adj)
  end subroutine chain_tan

  impure elemental function tan_v(v) result(s)
    type(var), intent(in) :: v
    type(var) :: s
    s = var(tan(v%val()), [v])
    call s%set_chain(chain_tan)
  end function tan_v

end module fazang_tan_op_mod

module fazang_tan_mod
  use fazang_tan_op_mod
  implicit none

  interface tan
     module procedure tan_v
  end interface tan
end module fazang_tan_mod
