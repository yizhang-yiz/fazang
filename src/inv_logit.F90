module fazang_inv_logit_mod
  use iso_fortran_env
  use fazang_vari_mod, only : vari, adstack, callstack
  use fazang_inv_mod
  use fazang_arith_mod
  use fazang_var_mod
  use fazang_env_mod

  implicit none

  private
  public :: inv_logit

  interface inv_logit
     module procedure :: inv_logit_d
     module procedure :: inv_logit_v
  end interface inv_logit

contains
  
  subroutine chain_inv_logit(this)
    class(vari), intent(in) :: this
    real(rk) :: adj(1)
    adj(1) = this%adj() * this%val() * (1.d0 - this%val())
    call this%set_operand_adj(adj)
  end subroutine chain_inv_logit

  impure elemental function inv_logit_v(v) result(s)
    type(var), intent(in) :: v
    type(var) :: s
    s = var(inv_logit_d(v%val()), [v])
    call s%set_chain(chain_inv_logit)
  end function inv_logit_v

  elemental function inv_logit_d(d) result(s)
    real(rk), intent(in) :: d
    real(rk) :: s, exp_d
    if ( d < 0.d0 ) then
       exp_d = exp(d)
       if (d < log_epsilon) then
          s = exp_d
       else
          s = exp_d / (1.d0 + exp_d);
       endif
    else
       s = inv(1.d0 + exp(-d))
    endif
  end function inv_logit_d

end module fazang_inv_logit_mod
