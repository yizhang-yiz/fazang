module grad_mod
  use vari_mod, only : adstack, callstack
  implicit none

contains

  subroutine set_zero_all_adj ()
    call callstack%set_zero_all_adj()
  end subroutine set_zero_all_adj

end module grad_mod
