module sum_op_mod
  use iso_fortran_env
  use env_mod
  use var_mod
  use vari_mod, only: vari, callstack

  implicit none

contains
  
  subroutine chain_sum(this)
    class(vari), intent(in) :: this
    real(rk) :: new_adj(this%n_operand())
    integer(ik) :: i(this%n_operand()), n, j
    n = this%n_operand()
    i = this%operand_index()
    new_adj = this%operand_adj()
    new_adj = new_adj + this%adj()
    do j = 1, n
       call callstack % stack % set_adj(i(j), new_adj(j))
    end do
  end subroutine chain_sum

  function sum_v(v) result(s)
    type(var), intent(in) :: v(:)
    type(var) :: s
    s = var(sum(val(v)), v)
    s%vi%chain => chain_sum
  end function sum_v

end module sum_op_mod

module sum_mod
  use sum_op_mod
  implicit none

  interface sum
     module procedure sum_v
  end interface sum
end module sum_mod
