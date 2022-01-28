module test_mod

  use, intrinsic :: iso_fortran_env
  implicit none

  real(REAL32), parameter :: ulp_f = 1.19209290e-7
  real(REAL64), parameter :: ulp_d = 2.2204460492503131e-16

contains

  subroutine expect_near_impl(a, b, tol, file, line)
    real(REAL64), intent(in) :: a, b, tol
    character (*) :: file
    integer(int32) :: line
    if ( abs(a - b) >= tol ) then
       write(*, *) file, "   line:", line
       write(*, *)
       write(*, *) "|", a, "-", b, "| >", tol
       stop 1
    end if
  end subroutine expect_near_impl


  subroutine expect_float_eq_impl (a, b, file, line)
    real(REAL64), intent(in) :: a, b
    character (*) :: file
    integer(int32) :: line
    if ( abs(a - b) >= 4.d0 * ulp_f ) then
       write(*, *) file, "   line:", line
       write(*, *)
       write(*, *) "|", a, "-", b, "| >", 4.d0 * ulp_f
       stop 2
    end if
  end subroutine expect_float_eq_impl

  subroutine expect_eq_impl (a, b, file, line)
    integer(int32), intent(in) :: a, b
    character (*) :: file
    integer(int32) :: line
    if ( .not.a == b ) then
       write(*, *) file
       write(*, *)
       write(*, *) a, "!=", b
       stop 3
    end if
  end subroutine expect_eq_impl

  subroutine expect_true_impl (a, file, line)
    logical, intent(in) :: a
    character (*) :: file
    integer(int32) :: line
    if ( .not. a ) then
       write(*, *) file, "   line:", line
       write(*, *)
       stop 4
    end if
  end subroutine expect_true_impl

end module test_mod
