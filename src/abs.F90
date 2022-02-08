module fazang_abs_v_mod
  use iso_fortran_env
  use fazang_env_mod
  use fazang_sub_mod
  use fazang_arith_mod
  use fazang_var_mod, only: var
  use fazang_vari_mod, only: vari, callstack

  implicit none

  private
  public :: abs_v

contains
  subroutine chain_abs(this)
    class(vari), intent(in) :: this
    if ( is_nan(this%val()) ) then
       call this%set_operand_adj([nan64])
    endif
  end subroutine chain_abs

  impure elemental function abs_v(v) result(s)
    type(var), intent(in) :: v
    type(var) :: s
    if ( v%val() > 0.d0 ) then
       s = v
    elseif ( v%val() < 0.d0 ) then
       s = -v
    elseif ( v%val() == 0.d0 ) then
       s = var()
    else
       s = var(nan64, [v])
       call s%set_chain(chain_abs)
    endif
  end function abs_v

end module fazang_abs_v_mod

module fazang_abs_mod
  use fazang_abs_v_mod
  implicit none

  interface abs
     module procedure abs_v
  end interface abs
end module fazang_abs_mod
