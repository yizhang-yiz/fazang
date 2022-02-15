module fazang_log_sum_exp_mod
  use iso_fortran_env
  use fazang_env_mod
  use fazang_var_mod
  use fazang_arith_mod
  use fazang_vari_mod, only: vari, callstack

  implicit none

  private
  public :: log_sum_exp

  interface log_sum_exp
     module procedure :: log_sum_exp_v
     module procedure :: log_sum_exp_d
  end interface log_sum_exp

contains
  
  subroutine chain_log_sum_exp(this)
    class(vari), intent(in) :: this
    real(rk) :: adj(this%n_operand())
    real(rk) :: val(this%n_operand())
    val = this%operand_val()
    adj = this%adj() * exp(val - this%val())
    call this%set_operand_adj(adj)
  end subroutine chain_log_sum_exp

  pure function log_sum_exp_d(d) result(s)
    real(rk), intent(in) :: d(:)
    real(rk) :: s, max_d
    max_d = maxval(d)
    if ( is_inf(max_d) ) then
       s = max_d
    else
       s = max_d + log(sum(exp(d - max_d)))
    endif
  end function log_sum_exp_d

  function log_sum_exp_v(v) result(s)
    type(var), intent(in) :: v(:)
    type(var) :: s
    s = var(log_sum_exp_d(v%val()), v)
    call s%set_chain(chain_log_sum_exp)
  end function log_sum_exp_v

end module fazang_log_sum_exp_mod
