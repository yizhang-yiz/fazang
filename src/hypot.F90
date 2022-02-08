module fazang_hypot_v_mod
  use iso_fortran_env
  use fazang_env_mod
  use fazang_var_mod, only: var
  use fazang_vari_mod, only: vari, callstack
  use fazang_abs_mod

  implicit none

  private
  public :: hypot_vv, hypot_vd, hypot_dv

contains
  subroutine chain_hypot(this)
    class(vari), intent(in) :: this
    real(rk) :: new_adj(this%n_operand()), val(this%n_operand())
    new_adj = this%operand_adj()
    val = this%operand_val()
    new_adj = new_adj + this%adj() * val / this%val()
    call this%set_operand_adj(new_adj)
  end subroutine chain_hypot

  impure elemental function hypot_vv(v1, v2) result(s)
    type(var), intent(in) :: v1, v2
    type(var) :: s
    s = var(hypot(v1%val(), v2%val()), [v1, v2])
    call s%set_chain(chain_hypot)
  end function hypot_vv

  impure elemental function hypot_vd(v1, v2) result(s)
    type(var), intent(in) :: v1
    real(rk), intent(in) :: v2
    type(var) :: s
    if ( v2 .ne. 0.d0 ) then
       s = var(hypot(v1%val(), v2), [v1])
    else
       s = abs(v1)
    end if
    call s%set_chain(chain_hypot)
  end function hypot_vd

  impure elemental function hypot_dv(v1, v2) result(s)
    real(rk), intent(in) :: v1
    type(var), intent(in) :: v2
    type(var) :: s
    if ( v1 .ne. 0.d0 ) then
       s = var(hypot(v2%val(), v1), [v2])
    else
       s = v2
    end if
    call s%set_chain(chain_hypot)
  end function hypot_dv

end module fazang_hypot_v_mod

module fazang_hypot_mod
  use fazang_hypot_v_mod
  implicit none

  interface hypot
     module procedure hypot_vv
     module procedure hypot_vd
     module procedure hypot_dv
  end interface hypot
end module fazang_hypot_mod
