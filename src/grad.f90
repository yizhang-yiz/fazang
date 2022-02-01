module grad_mod
  use vari_mod, only : adstack, callstack
  use env_mod
  use var_mod
  use nested_tape_mod
  implicit none

  abstract interface
     function dependent_function (x) result (fx)
       import :: var
       type(var), intent(in) :: x(:)
       type(var) :: fx
     end function dependent_function
  end interface

contains

  subroutine set_zero_all_adj ()
    call callstack%set_zero_all_adj()
  end subroutine set_zero_all_adj

  function gradient(f, x) result (f_df)
    procedure(dependent_function) :: f
    real(rk), intent(in) :: x(:)
    real(rk) :: f_df(1 + size(x))
    type(var) :: x_var(size(x)), f_var
    call begin_nested()

    x_var = var(x)
    f_var = f(x_var)
    f_df(1) = f_var%val()
    call f_var%grad()
    f_df(2:(1+size(x))) = x_var%adj()

    call end_nested()
  end function gradient

end module grad_mod
