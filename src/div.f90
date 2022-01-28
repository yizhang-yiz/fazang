module div_mod
  use iso_fortran_env
  use var_mod, only: var
  use vari_mod

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
    integer(ik) :: i(2)
    real(rk) x1, x2
    i = callstack%get_operand_index(this)
    x1 = callstack%val(i(1))
    x2 = callstack%val(i(2))
    call callstack%set_adj(i(1), callstack%adj(i(1)) + this%adj()/x2)
    call callstack%set_adj(i(2), callstack%adj(i(2)) - this%val() * this%adj()/x2)
  end subroutine chain_div_vv

  function div_vv(v1, v2) result(s)
    use op_vv_mod
    type(var), intent(in) :: v1, v2
    type(var) :: s
    s = var(v1%val() / v2%val())
    call setup_callstack(s, v1, v2, chain_div_vv)
  end function div_vv

  subroutine chain_div_vd(this)
    class(vari), intent(in) :: this
    integer(ik) :: i(1)
    real(rk) d(1)
    i = callstack%get_operand_index(this)
    d = callstack%get_operand_r(this)
    call callstack%set_adj(i(1), callstack%adj(i(1)) + this%adj() / d(1))
  end subroutine chain_div_vd

  function div_vd(v, d) result(s)
    use op_vv_mod
    type(var), intent(in) :: v
    real(rk), intent(in) :: d
    type(var) :: s
    if ( d .ne. 1.d0 ) then
       s = var(v%val() / d)
    else
       s = v
    end if
    call setup_callstack(s, v, d, chain_div_vd)
  end function div_vd

  subroutine chain_div_dv(this)
    class(vari), intent(in) :: this
    integer(ik) :: i(1)
    real(rk) x2
    i = callstack%get_operand_index(this)
    x2 = callstack%val(i(1))
    call callstack%set_adj(i(1), callstack%adj(i(1)) - this%val() * this%adj() / x2)
  end subroutine chain_div_dv

  function div_dv(d, v) result(s)
    use op_vv_mod
    real(rk), intent(in) :: d
    type(var), intent(in) :: v
    type(var) :: s
    s = var(d / v%val())
    call setup_callstack(s, v, chain_div_dv)
  end function div_dv

end module div_mod
