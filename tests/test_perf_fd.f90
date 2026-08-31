#include "assert_inc.f90"

module eval_finite_diff
  use, intrinsic :: iso_fortran_env
  use fz_env
  implicit none

  abstract interface
     real(rk) function fd_op(x)
       import :: rk
       real(rk), intent(in) :: x(:)
     end function fd_op
  end interface

  interface fd_val_jac
     module procedure eval_func_fd
     module procedure eval_func_fd2
  end interface fd_val_jac
contains
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

end module eval_finite_diff

program finite_diff_perf_test
  use fz_var
  use eval_finite_diff

  implicit none
  real(rk), parameter :: tol = 1.d-15

  integer(ik), parameter :: nmax = 1028
  real(rk) :: x(nmax)
  integer(ik) :: i, j, narg

  integer(ik) :: clock_rate, clock_start, clock_stop
  real(rk) :: elapsed_time
  real(rk) :: res_ad(nmax+1), res_fd(nmax+1)

  call random_number(x)

  do i = 4, 10
     narg = 2**i

     ! ad
     call system_clock(count_rate=clock_rate)
     call system_clock(count=clock_start)

     call reboot_chain()
     res_ad(1:(1+narg)) = jac(logit_inv_logit_var, x(1:narg))

     call system_clock(count=clock_stop)
     elapsed_time = real(clock_stop - clock_start) / real(clock_rate)
     write(*, *) "time ad: ", narg, elapsed_time

     ! fd
     call system_clock(count=clock_start)

     res_fd(1:(1+narg)) = fd_val_jac(logit_inv_logit, x(1:narg), 1.d-5)

     call system_clock(count=clock_stop)
     elapsed_time = real(clock_stop - clock_start) / real(clock_rate)
     write(*, *) "time fd: ", narg, elapsed_time

  end do

contains
  type(var) function logit_inv_logit_var(x)
    implicit none
    type(var), intent(in) :: x(:)
    integer(ik) :: i, j
    type(var) :: v(size(x))
    do i = 1, size(x)
       v(i) = sin(x(i))
       do j = 1, 20
          v(i) = sin(v(i))
       enddo
    enddo
    logit_inv_logit_var = sum(v)
  end function logit_inv_logit_var

  real(rk) function logit_inv_logit(x)
    implicit none
    real(rk), intent(in) :: x(:)
    integer(ik) :: i, j
    real(rk) :: v(size(x))

    do i = 1, size(x)
       v(i) = sin(x(i))
       do j = 1, 20
          v(i) = sin(v(i))
       enddo
    enddo
    logit_inv_logit = sum(v)
  end function logit_inv_logit

end program finite_diff_perf_test
