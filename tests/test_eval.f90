#include "assert_inc.f90"

program fz_eval_test
  use fz_var
  use fz_eval

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
     res_ad(1:(1+narg)) = eval(logit_inv_logit_var, x(1:narg))

     call system_clock(count=clock_stop)
     elapsed_time = real(clock_stop - clock_start) / real(clock_rate)
     write(*, *) "time ad: ", narg, elapsed_time

     ! fd
     call system_clock(count=clock_start)

     res_fd(1:(1+narg)) = eval(logit_inv_logit, x(1:narg), 1.d-5)

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

end program fz_eval_test
