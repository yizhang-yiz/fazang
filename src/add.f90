module add_mod
  use iso_fortran_env
  use env_mod
  use var_mod, only: var
  use vari_mod, only: vari, callstack

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
    real(rk) :: new_adj(2)
    integer(ik) :: i(2)
    i = this%operand_index()
    new_adj = this%operand_adj()
    new_adj = new_adj + this%adj()
    call callstack % stack % set_adj(i(1), new_adj(1))
    call callstack % stack % set_adj(i(2), new_adj(2))
  end subroutine chain_add_vv

  function add_vv(v1, v2) result(s)
    type(var), intent(in) :: v1, v2
    type(var) :: s
    s = var(v1%val() + v2%val(), [v1, v2])
    s%vi%chain => chain_add_vv
  end function add_vv

  subroutine chain_add_vd(this)
    class(vari), intent(in) :: this
    real(rk) :: new_adj(1)
    integer(ik) :: i(1)
    i = this%operand_index()
    new_adj = this%operand_adj()
    new_adj(1) = new_adj(1) + this%adj()
    call callstack % stack % set_adj(i(1), new_adj(1))
  end subroutine chain_add_vd

  function add_vd(v1, v2) result(s)
    type(var), intent(in) :: v1
    real(rk), intent(in) :: v2
    type(var) :: s
    if ( v2 .ne. 0.d0 ) then
       s = var(v1%val() + v2, [v1])
    else
       s = v1
    end if
    s%vi%chain => chain_add_vd
  end function add_vd

  function add_dv(v1, v2) result(s)
    type(var), intent(in) :: v2
    real(rk), intent(in) :: v1
    type(var) :: s
    if ( v1 .ne. 0.d0 ) then
       s = var(v1 + v2%val(), [v2])
    else
       s = v2
    end if
    s%vi%chain => chain_add_vd
  end function add_dv

  function pos(v) result(s)
    type(var), intent(in) :: v
    type(var) :: s
    s = v
  end function pos

end module add_mod
