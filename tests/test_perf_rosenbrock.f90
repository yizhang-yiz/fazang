module benchmark_rosenbrock
    ! Note: Replace 'ad_core' with the actual name of your Fortran AD module.
    ! We assume it exports 'type(var)' and overloads operators (+, -, *, **, =).
  use fz_var
  use fz_env
  implicit none

  real(rk) :: s(100000)
contains

    ! N-dimensional Rosenbrock function
    ! A standard non-linear test function for optimization and autodiff benchmarking
    function rosenbrock(x) result(f)
      use fz_var
      implicit none
        type(var), intent(in) :: x(:)
        type(var) :: f
        integer :: n
        n  = size(x)

        f = 100.d0*sum(square(x(2:n)-square(x(1:(n-1))))) + sum(square(1.d0-x))

    end function rosenbrock

end module benchmark_rosenbrock

program autodiff_benchmark
  use, intrinsic :: iso_fortran_env, only: dp => real64, int64
  ! Replace with your actual module and memory management routine
  use fz_var
  use benchmark_rosenbrock
  implicit none

  integer, parameter:: n_dims=1000, iterations=1000
  integer :: i, iter

  ! Timing variables
  integer(int64) :: count_start, count_end, count_rate
  real(dp) :: elapsed, total_time

  type(var) :: x(n_dims)
  type(var) :: f

  total_time = 0.0_dp

  ! Get the clock resolution for high-precision timing
  call system_clock(count_rate=count_rate)

  do iter = 1, iterations
     ! 1. Initialize independent variables
     do i = 1, n_dims
        x(i) = 1.0_dp + 0.1_dp * real(i - 1, dp)
     end do

     call system_clock(count=count_start)

     ! 2. Forward pass: Build the expression graph on the tape
     f = rosenbrock(x)

     ! 3. Reverse pass: Propagate the adjoints (chain rule)
     ! Assuming type-bound procedure for reverse pass (e.g. f%grad())
     ! If your library uses a standalone subroutine, change to: call grad(f)
     call grad(f)

     call system_clock(count=count_end)

     elapsed = real(count_end - count_start, dp) / real(count_rate, dp)
     total_time = total_time + elapsed

     ! 4. (Optional) Extract gradients
     ! real_grad_0 = x(1)%adj

     ! 5. Memory Management: Clear the AD tape!
     ! Critical step when looping in reverse-mode AD to prevent memory leaks
     call reboot_chain()
  end do

  print "(A, F10.6, A)", "Total Time: ", total_time, " s"
  print "(A, F10.6, A)", "Average Time per Iteration: ", (total_time / real(iterations, dp)) * 1000.0_dp, " ms"

end program autodiff_benchmark
