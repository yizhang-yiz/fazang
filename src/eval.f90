module fz_eval
  use, intrinsic :: iso_fortran_env
  use fz_env
  use fz_var
  implicit none

  private
  public :: ad_op, fd_op, eval

  abstract interface
     type(var) function ad_op(x)
       use fz_var
       use fz_env
       type(var), intent(in) :: x(:)
     end function ad_op

     real(rk) function fd_op(x)
       use fz_env
       real(rk), intent(in) :: x(:)
     end function fd_op
  end interface

  interface eval
     module procedure eval_func_ad
     module procedure eval_func_fd
     module procedure eval_func_fd2
  end interface eval
contains
  function eval_func_ad(f, x) result(res)
    implicit none
    procedure(ad_op) :: f
    real(rk), intent(in) :: x(:)
    real(rk) :: res(size(x) + 1)

    type(var) :: v
    integer(ik) :: i, n
    type(var) :: vx(size(x))

    n = size(x)
    do i = 1, n
       vx(i) = x(i)
    end do

    v = f(vx)
    call grad(v)
    res(1) = val(v)
    res(2:n) = adj(vx)
  end function eval_func_ad

  function eval_func_fd(f, x, delta) result(res)
    implicit none
    procedure(fd_op) :: f
    real(rk), intent(in) :: x(:), delta
    real(rk) :: res(size(x) + 1)

    integer(ik) :: i, n
    real(rk) :: x1(size(x)), x2(size(x))

    n = size(x)
    res = 0.d0
    res(1) = f(x)
    x1 = x
    x2 = x
    do i = 1, n
       x1(i) = x1(i) - 0.5d0*delta
       x2(i) = x2(i) + 0.5d0*delta
       res(1 + i) = (f(x2) - f(x1))/delta
       x1(i) = x(i)
       x2(i) = x(i)
    end do
  end function eval_func_fd

  function eval_func_fd2(f, x, delta) result(res)
    implicit none
    procedure(fd_op) :: f
    real(rk), intent(in) :: x(:), delta(size(x))
    real(rk) :: res(size(x) + 1)

    integer(ik) :: i, n
    real(rk) :: x1(size(x)), x2(size(x))

    n = size(x)
    res = 0.d0
    res(1) = f(x)
    x1 = x
    x2 = x
    do i = 1, n
       x1(i) = x1(i) - 0.5d0*delta(i)
       x2(i) = x2(i) + 0.5d0*delta(i)
       res(1 + i) = (f(x2) - f(x1))/delta(i)
       x1(i) = x(i)
       x2(i) = x(i)
    end do
  end function eval_func_fd2

end module fz_eval
