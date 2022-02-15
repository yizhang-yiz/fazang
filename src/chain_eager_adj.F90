module fazang_eager_adj_mod
  use fazang_var_mod
  use fazang_env_mod
  use fazang_vari_mod, only : vari
  implicit none

contains  

  subroutine chain_eager_adj(this)
    class(vari), intent(in) :: this
    real(rk) :: adj(this%n_operand())
    adj = this%adj() * this%data_operand()
    call this%set_operand_adj(adj)
  end subroutine chain_eager_adj

end module fazang_eager_adj_mod
