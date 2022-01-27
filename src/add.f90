module add_op_mod
  use iso_fortran_env
  use var_mod, only: var
  use vari_mod
  use op_vv_mod

  implicit none

  interface operator (+)
     module procedure :: add_vv
     module procedure :: add_vd
     module procedure :: add_dv
  end interface operator (+)

contains
  
  subroutine chain_add_vv(this)
    class(vari), intent(in) :: this
    integer(ik) :: i(2)
    i = callstack%get_operand_index(this)
    call callstack%set_adj(i(1), callstack%adj(i(1)) + this%adj())
    call callstack%set_adj(i(2), callstack%adj(i(2)) + this%adj())
  end subroutine chain_add_vv

  function add_vv(v1, v2) result(s)
    type(var), intent(in) :: v1, v2
    type(var) :: s
    s = var(v1%val() + v2%val())
    call setup_callstack(s, v1, v2, chain_add_vv)
  end function add_vv

  subroutine chain_add_vd(this)
    class(vari), intent(in) :: this
    integer(ik) :: i(1)
    i = callstack%get_operand_index(this)
    call callstack%set_adj(i(1), callstack%adj(i(1)) + this%adj())
  end subroutine chain_add_vd

  function add_vd(v1, v2) result(s)
    type(var), intent(in) :: v1
    real(rk), intent(in) :: v2
    type(var) :: s
    if ( v2 .ne. 0.d0 ) then
       s = var(v1%val() + v2)
    else
       s = v1
    end if
    call setup_callstack(s, v1, chain_add_vd)
  end function add_vd

  function add_dv(v1, v2) result(s)
    type(var), intent(in) :: v2
    real(rk), intent(in) :: v1
    type(var) :: s
    if ( v1 .ne. 0.d0 ) then
       s = var(v1 + v2%val())
    else
       s = v2
    end if
    call setup_callstack(s, v2, chain_add_vd)
  end function add_dv

end module add_op_mod

module add_mod
  use add_op_mod
  implicit none

  interface add
     module procedure add_vv
     module procedure add_vd
     module procedure add_dv
  end interface add
end module add_mod
