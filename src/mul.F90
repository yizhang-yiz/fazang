module fazang_mul_mod
  use iso_fortran_env
  use fazang_env_mod
  use fazang_var_mod, only: var
  use fazang_vari_mod, only: vari

  implicit none

  private
  public :: operator (*)

  interface operator (*)
     module procedure :: mul_vv
     module procedure :: mul_vd
     module procedure :: mul_dv
  end interface operator (*)

contains
  
  subroutine chain_mul_vv(this)
    class(vari), intent(in) :: this
    real(rk) :: adj(2), val(2)
    val = this%operand_val()
    adj(1) = val(2) * this%adj()
    adj(2) = val(1) * this%adj()
    call this%set_operand_adj(adj)
  end subroutine chain_mul_vv

  impure elemental function mul_vv(v1, v2) result(s)
    type(var), intent(in) :: v1, v2
    type(var) :: s
    s = var(v1%val() * v2%val(), [v1, v2])
    call s%set_chain(chain_mul_vv)
  end function mul_vv

  subroutine chain_mul_vd(this)
    class(vari), intent(in) :: this
    real(rk) adj(1)
    adj = this%adj() * this%data_operand()
    call this%set_operand_adj(adj)
  end subroutine chain_mul_vd

  impure elemental function mul_vd(v, d) result(s)
    type(var), intent(in) :: v
    real(rk), intent(in) :: d
    type(var) :: s
    if ( d .ne. 1.d0 ) then
       s = var(v%val() * d, [v], [d])
    else
       s = v
    end if
    call s%set_chain(chain_mul_vd)
  end function mul_vd

  impure elemental function mul_dv(d, v) result(s)
    real(rk), intent(in) :: d
    type(var), intent(in) :: v
    type(var) :: s
    if ( d .ne. 1.d0 ) then
       s = var(v%val() * d, [v], [d])
    else
       s = v
    end if
    call s%set_chain(chain_mul_vd)
  end function mul_dv

end module fazang_mul_mod
