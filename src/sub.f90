module sub_mod
  use iso_fortran_env
  use env_mod
  use var_mod, only: var
  use vari_mod, only: vari, callstack

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
    real(rk) :: new_adj(2)
    integer(ik) :: i(2)
    i = this%operand_index()
    new_adj = this%operand_adj()
    new_adj(1) = new_adj(1) + this%adj()
    new_adj(2) = new_adj(2) - this%adj()
    call callstack % stack % set_adj(i(1), new_adj(1))
    call callstack % stack % set_adj(i(2), new_adj(2))
  end subroutine chain_sub_vv

  impure elemental function sub_vv(v1, v2) result(s)
    type(var), intent(in) :: v1, v2
    type(var) :: s
    s = var(v1%val() - v2%val(), [v1, v2])
    s%vi%chain => chain_sub_vv
  end function sub_vv

  subroutine chain_sub_vd(this)
    class(vari), intent(in) :: this
    real(rk) :: new_adj(1)
    integer(ik) :: i(1)
    i = this%operand_index()
    new_adj = this%operand_adj()
    new_adj(1) = new_adj(1) + this%adj()
    call callstack % stack % set_adj(i(1), new_adj(1))
  end subroutine chain_sub_vd

  impure elemental function sub_vd(v1, v2) result(s)
    type(var), intent(in) :: v1
    real(rk), intent(in) :: v2
    type(var) :: s
    if ( v2 .ne. 0.d0 ) then
       s = var(v1%val() - v2, [v1])
    else
       s = v1
    end if
    s%vi%chain => chain_sub_vd
  end function sub_vd

  subroutine chain_sub_dv(this)
    class(vari), intent(in) :: this
    real(rk) :: new_adj(1)
    integer(ik) :: i(1)
    i = this%operand_index()
    new_adj = this%operand_adj()
    new_adj(1) = new_adj(1) - this%adj()
    call callstack % stack % set_adj(i(1), new_adj(1))
  end subroutine chain_sub_dv

  impure elemental function sub_dv(v1, v2) result(s)
    type(var), intent(in) :: v2
    real(rk), intent(in) :: v1
    type(var) :: s
    s = var(v1 - v2%val(), [v2])
    s%vi%chain => chain_sub_dv
  end function sub_dv

  function neg(v) result(s)
    type(var), intent(in) :: v
    type(var) :: s
    s = var(-v%val(), [v])
    s%vi%chain => chain_sub_dv
  end function neg

end module sub_mod
