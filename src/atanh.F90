module fazang_atanh_v_mod
  use iso_fortran_env
  use fazang_env_mod
  use fazang_sub_mod
  use fazang_arith_mod
  use fazang_rel_operator_mod
  use fazang_var_mod, only: var
  use fazang_vari_mod, only: vari, callstack

  implicit none

  private
  public :: atanh_v

contains
  subroutine chain_atanh(this)
    class(vari), intent(in) :: this
    real(rk) :: adj(1)
    real(rk) :: val(1)
    val = this%operand_val()
    adj = this%operand_adj()
    adj = adj + this%adj() / (1.d0 - val * val)
    call this%set_operand_adj(adj)
  end subroutine chain_atanh

  impure elemental function atanh_v(v) result(s)
    type(var), intent(in) :: v
    type(var) :: s
    if ( is_nan(v) ) then
       s = v
    elseif ( v <= -1.d0 .or. v >= 1.d0 ) then
       error stop
    else
       s = var(atanh(v%val()), [v])
       call s%set_chain(chain_atanh)
    endif
  end function atanh_v

end module fazang_atanh_v_mod

module fazang_atanh_mod
  use fazang_atanh_v_mod
  implicit none

  interface atanh
     module procedure atanh_v
  end interface atanh
end module fazang_atanh_mod
