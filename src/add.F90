module fazang_add_mod
  use iso_fortran_env
  use fazang_env_mod
  use fazang_var_mod, only: var
  use fazang_vari_mod, only: vari, callstack

  implicit none

  private
  public :: operator (+)

  interface operator (+)
     module procedure :: add_vv
     module procedure :: add_vd
     module procedure :: add_dv
     module procedure :: pos
  end interface operator (+)

contains
  subroutine chain_add_vv(this)
    class(vari), intent(in) :: this
    real(rk) :: adj(2)
    adj = this%adj()
    call this%set_operand_adj(adj)
  end subroutine chain_add_vv

  impure elemental function add_vv(v1, v2) result(s)
    implicit none
    type(var), intent(in) :: v1, v2
    type(var) :: s
    s = var(v1%val() + v2%val(), [v1, v2])
    call s%set_chain(chain_add_vv)
  end function add_vv

  subroutine chain_add_vd(this)
    implicit none
    class(vari), intent(in) :: this
    real(rk) :: adj(1)
    adj(1) = this%adj()
    call this%set_operand_adj(adj)
  end subroutine chain_add_vd

  impure elemental function add_vd(v1, v2) result(s)
    type(var), intent(in) :: v1
    real(rk), intent(in) :: v2
    type(var) :: s
    if ( v2 .ne. 0.d0 ) then
       s = var(v1%val() + v2, [v1])
    else
       s = v1
    end if
    call s%set_chain(chain_add_vd)
  end function add_vd

  impure elemental function add_dv(v1, v2) result(s)
    type(var), intent(in) :: v2
    real(rk), intent(in) :: v1
    type(var) :: s
    if ( v1 .ne. 0.d0 ) then
       s = var(v1 + v2%val(), [v2])
    else
       s = v2
    end if
    call s%set_chain(chain_add_vd)
  end function add_dv

  impure elemental function pos(v) result(s)
    type(var), intent(in) :: v
    type(var) :: s
    s = v
  end function pos

end module fazang_add_mod
