module fazang_grad_mod
  use fazang_vari_mod, only : adstack, callstack
  use fazang_env_mod
  use fazang_var_mod
  use fazang_nested_tape_mod
  implicit none

  abstract interface
     function dependent_function (x) result (fx)
       import :: var
       type(var), intent(in) :: x(:)
       type(var) :: fx
     end function dependent_function
  end interface

  abstract interface
     function jac_dependent_function (x, n) result (fx)
       import :: var
       integer, intent(in) :: n
       type(var), intent(in) :: x(:)
       type(var) :: fx(n)
     end function jac_dependent_function
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

  function jacobian(f, n, x) result (f_df)
    procedure(jac_dependent_function) :: f
    real(rk), intent(in) :: x(:)
    integer(ik), intent(in) :: n
    integer(ik) :: i
    real(rk) :: f_df(n, 1 + size(x))
    type(var) :: x_var(size(x)), f_var(n)
    call begin_nested()

    x_var = var(x)
    f_var = f(x_var, n)
    f_df(:, 1) = f_var%val()

    do i = 1, n
       call set_zero_nested_adj()
       call f_var(i)%grad()
       f_df(i, 2:(1 + size(x))) = x_var%adj()
    end do

    call end_nested()
  end function jacobian


end module fazang_grad_mod
