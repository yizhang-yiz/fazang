module fazang_eager_adj_mod
  use fazang_var_mod
  use fazang_env_mod
  use fazang_vari_mod, only : vari
  implicit none

  private
  public :: var_with_partials

contains  

  subroutine chain_eager_adj(this)
    implicit none
    class(vari), intent(in) :: this
    real(rk) :: adj(this%n_operand())
    adj = this%adj() * this%data_operand()
    call this%set_operand_adj(adj)
  end subroutine chain_eager_adj

  function var_with_partials(val, operands, partial) result(v)
    implicit none
    type(var), intent(in) :: operands(:)
    real(rk), intent(in) :: val, partial(size(operands))
    type(var) :: v
    v = var(val, operands, partial)
    call v%set_chain(chain_eager_adj)
  end function var_with_partials

end module fazang_eager_adj_mod
