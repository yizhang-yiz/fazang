module sub_mod
  use iso_fortran_env
  use var_mod, only: var
  use vari_mod

  implicit none

  private
  public :: operator (-)

  interface operator (-)
     module procedure :: sub_vv
     module procedure :: sub_vd
     module procedure :: sub_dv
     module procedure :: neg
  end interface operator (-)

contains
  subroutine chain_sub_vv(this)
    class(vari), intent(in) :: this
    integer(ik) :: i(2)
    i = callstack%get_operand_index(this)
    call callstack%set_adj(i(1), callstack%adj(i(1)) + this%adj())
    call callstack%set_adj(i(2), callstack%adj(i(2)) - this%adj())
  end subroutine chain_sub_vv

  function sub_vv(v1, v2) result(s)
    use op_vv_mod
    type(var), intent(in) :: v1, v2
    type(var) :: s
    s = var(v1%val() - v2%val())
    call setup_callstack(s, v1, v2, chain_sub_vv)
  end function sub_vv

  subroutine chain_sub_vd(this)
    use op_vv_mod
    class(vari), intent(in) :: this
    integer(ik) :: i(1)
    i = callstack%get_operand_index(this)
    call callstack%set_adj(i(1), callstack%adj(i(1)) + this%adj())
  end subroutine chain_sub_vd

  function sub_vd(v1, v2) result(s)
    use op_vv_mod
    type(var), intent(in) :: v1
    real(rk), intent(in) :: v2
    type(var) :: s
    if ( v2 .ne. 0.d0 ) then
       s = var(v1%val() - v2)
    else
       s = v1
    end if
    call setup_callstack(s, v1, chain_sub_vd)
  end function sub_vd

  subroutine chain_sub_dv(this)
    class(vari), intent(in) :: this
    integer(ik) :: i(1)
    i = callstack%get_operand_index(this)
    call callstack%set_adj(i(1), callstack%adj(i(1)) - this%adj())
  end subroutine chain_sub_dv

  function sub_dv(v1, v2) result(s)
    use op_vv_mod
    type(var), intent(in) :: v2
    real(rk), intent(in) :: v1
    type(var) :: s
    s = var(v1 - v2%val())
    call setup_callstack(s, v2, chain_sub_dv)
  end function sub_dv

  function neg(v) result(s)
    use op_vv_mod
    type(var), intent(in) :: v
    type(var) :: s
    s = var(-v%val())
    call setup_callstack(s, v, chain_sub_dv)
  end function neg

end module sub_mod
