module mul_mod
  use iso_fortran_env
  use env_mod
  use var_mod, only: var
  use vari_mod, only: vari, callstack

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
    real(rk) :: new_adj(2), val(2)
    integer(ik) :: i(2)
    i = this%operand_index()
    new_adj = this%operand_adj()
    val = this%operand_val()
    new_adj(1) = new_adj(1) + val(2) * this%adj()
    new_adj(2) = new_adj(2) + val(1) * this%adj()
    call callstack % stack % set_adj(i(1), new_adj(1))
    call callstack % stack % set_adj(i(2), new_adj(2))
  end subroutine chain_mul_vv

  impure elemental function mul_vv(v1, v2) result(s)
    type(var), intent(in) :: v1, v2
    type(var) :: s
    s = var(v1%val() * v2%val(), [v1, v2])
    s%vi%chain => chain_mul_vv
  end function mul_vv

  subroutine chain_mul_vd(this)
    class(vari), intent(in) :: this
    integer(ik) :: i(1)
    real(rk) d(1), new_adj(1)
    i = this%operand_index()
    new_adj = this%operand_adj()
    d = this%data_operand()
    new_adj(1) = new_adj(1) + this%adj() * d(1)
    call callstack % stack % set_adj(i(1), new_adj(1))
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
    s%vi%chain => chain_mul_vd
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
    s%vi%chain => chain_mul_vd
  end function mul_dv

end module mul_mod
