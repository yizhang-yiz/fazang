module benchmark_rosenbrock2
  use fazang
  implicit none

contains

    ! N-dimensional Rosenbrock function
    function rosenbrock(x) result(f)
      implicit none
        type(var), intent(in) :: x(:)
        type(var) :: f
        integer :: i, n
        n  = size(x)

        f = 0.d0
        do i = 1, n-1
           f = f + 100.d0*(x(i+1)-x(i)**2.d0)**2.d0 + (1.d0-x(i))**2.d0
        enddo

    end function rosenbrock

end module benchmark_rosenbrock2

program autodiff_benchmark
  use fazang
  use benchmark_rosenbrock2
  implicit none

  integer, parameter:: n_dims=1000, iterations=1000
  integer :: i, iter

  ! Timing variables
  integer(int64) :: count_start, count_end, count_rate
  real(rk) :: elapsed, total_time

  type(var) :: x(n_dims), f

  total_time = 0.0_rk

  ! Get the clock resolution for high-precision timing
  call system_clock(count_rate=count_rate)

  do iter = 1, iterations
     ! 1. Initialize independent variables
     do i = 1, n_dims
        x(i) = 1.0_rk + 0.1_rk * real(i - 1, rk)
     end do

     call system_clock(count=count_start)

     ! 2. Forward pass: Build the expression graph
     f = rosenbrock(x)

     ! 3. Reverse pass: Propagate the adjoints (chain rule)
     call grad(f)

     call system_clock(count=count_end)

     elapsed = real(count_end - count_start, rk) / real(count_rate, rk)
     total_time = total_time + elapsed

     ! 4. (Optional) Extract gradients
     if (.false.) write(*, *) "d(f)/dx(1): ", adj(x(1))

     ! 5. clean the AD tape
     call reboot_chain()
  end do

  print "(A, F10.6, A)", "Total Time: ", total_time, " s"
  print "(A, F10.6, A)", "Average Time per Iteration: ", (total_time / real(iterations, rk)) * 1000.0_rk, " ms"

end program autodiff_benchmark
