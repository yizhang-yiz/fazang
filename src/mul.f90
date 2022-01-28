module mul_mod
  use iso_fortran_env
  use var_mod, only: var
  use vari_mod

  implicit none

  interface operator (*)
     module procedure :: mul_vv
     module procedure :: mul_vd
     module procedure :: mul_dv
  end interface operator (*)

contains
  
  subroutine chain_mul_vv(this)
    class(vari), intent(in) :: this
    integer(ik) :: i(2)
    real(rk) x1, x2
    i = callstack%get_operand_index(this)
    x1 = callstack%val(i(1))
    x2 = callstack%val(i(2))
    call callstack%set_adj(i(1), callstack%adj(i(1)) + x2 * this%adj())
    call callstack%set_adj(i(2), callstack%adj(i(2)) + x1 * this%adj())
  end subroutine chain_mul_vv

  function mul_vv(v1, v2) result(s)
    use op_vv_mod
    type(var), intent(in) :: v1, v2
    type(var) :: s
    s = var(v1%val() * v2%val())
    call setup_callstack(s, v1, v2, chain_mul_vv)
  end function mul_vv

  subroutine chain_mul_vd(this)
    class(vari), intent(in) :: this
    integer(ik) :: i(1)
    real(rk) d(1)
    i = callstack%get_operand_index(this)
    d = callstack%get_operand_r(this)
    call callstack%set_adj(i(1), callstack%adj(i(1)) + this%adj() * d(1))
  end subroutine chain_mul_vd

  function mul_vd(v, d) result(s)
    use op_vv_mod
    type(var), intent(in) :: v
    real(rk), intent(in) :: d
    type(var) :: s
    if ( d .ne. 1.d0 ) then
       s = var(v%val() * d)
    else
       s = v
    end if
    call setup_callstack(s, v, d, chain_mul_vd)
  end function mul_vd

  function mul_dv(d, v) result(s)
    use op_vv_mod
    real(rk), intent(in) :: d
    type(var), intent(in) :: v
    type(var) :: s
    if ( d .ne. 1.d0 ) then
       s = var(v%val() * d)
    else
       s = v
    end if
    call setup_callstack(s, v, d, chain_mul_vd)
  end function mul_dv

end module mul_mod
