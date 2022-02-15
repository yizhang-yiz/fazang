module fazang_logit_mod
  use iso_fortran_env
  use fazang_vari_mod, only : vari, adstack, callstack
  use fazang_var_mod
  use fazang_env_mod

  implicit none

  private
  public :: logit

  interface logit
     module procedure :: logit_d
     module procedure :: logit_v
  end interface logit

contains
  
  subroutine chain_logit(this)
    class(vari), intent(in) :: this
    real(rk) :: adj(1), val(1)
    val = this%operand_val()
    adj(1) = this%adj() / (val(1) - val(1) * val(1))
    call this%set_operand_adj(adj)
  end subroutine chain_logit

  impure elemental function logit_v(v) result(s)
    type(var), intent(in) :: v
    type(var) :: s
    s = var(logit_d(v%val()), [v])
    call s%set_chain(chain_logit)
  end function logit_v

  elemental function logit_d(d) result(s)
    real(rk), intent(in) :: d
    real(rk) :: s
    s = log(d / (1.d0 - d))
  end function logit_d

end module fazang_logit_mod
