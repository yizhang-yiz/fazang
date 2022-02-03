module fazang_div_mod
  use iso_fortran_env
  use fazang_env_mod
  use fazang_var_mod, only: var
  use fazang_vari_mod, only: vari, callstack

  implicit none

  private
  public :: operator (/)

  interface operator (/)
     module procedure :: div_vv
     module procedure :: div_vd
     module procedure :: div_dv
  end interface operator (/)

contains
  
  subroutine chain_div_vv(this)
    class(vari), intent(in) :: this
    real(rk) :: new_adj(2), val(2)
    new_adj = this%operand_adj()
    val = this%operand_val()
    new_adj(1) = new_adj(1) + this%adj()/val(2)
    new_adj(2) = new_adj(2) - this%val() * this%adj()/val(2)
    call this%set_operand_adj(new_adj)
  end subroutine chain_div_vv

  impure elemental function div_vv(v1, v2) result(s)
    type(var), intent(in) :: v1, v2
    type(var) :: s
    s = var(v1%val() / v2%val(), [v1, v2])
    call s%set_chain(chain_div_vv)
  end function div_vv

  subroutine chain_div_vd(this)
    class(vari), intent(in) :: this
    real(rk) d(1), new_adj(1)
    new_adj = this%operand_adj()
    d = this%data_operand()
    new_adj(1) = new_adj(1) + this%adj() / d(1)
    call this%set_operand_adj(new_adj)
  end subroutine chain_div_vd

  impure elemental function div_vd(v, d) result(s)
    type(var), intent(in) :: v
    real(rk), intent(in) :: d
    type(var) :: s
    if ( d .ne. 1.d0 ) then
       s = var(v%val() / d, [v], [d])
    else
       s = v
    end if
    call s%set_chain(chain_div_vd)
  end function div_vd

  subroutine chain_div_dv(this)
    class(vari), intent(in) :: this
    real(rk) val(1), new_adj(1)
    new_adj = this%operand_adj()
    val = this%operand_val()
    new_adj(1) = new_adj(1) - this%val() * this%adj() / val(1)
    call this%set_operand_adj(new_adj)
  end subroutine chain_div_dv

  impure elemental function div_dv(d, v) result(s)
    real(rk), intent(in) :: d
    type(var), intent(in) :: v
    type(var) :: s
    s = var(d / v%val(), [v], [d])
    call s%set_chain(chain_div_dv)
  end function div_dv

end module fazang_div_mod
