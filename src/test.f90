module test_mod

  use, intrinsic :: iso_fortran_env
  implicit none

  real(REAL32), parameter :: ulp_f = 1.19209290e-7
  real(REAL64), parameter :: ulp_d = 2.2204460492503131e-16

contains

  subroutine expect_near (a, b, tol)
    real(REAL64), intent(in) :: a, b, tol
    if ( abs(a - b) >= tol ) then
       write(*, *) "|", a, "-", b, "| >", tol
       error stop
    end if
  end subroutine expect_near

  subroutine expect_float_eq (a, b)
    real(REAL64), intent(in) :: a, b
    if ( abs(a - b) >= 4.d0 * ulp_f ) then
       write(*, *) "|", a, "-", b, "| >", 4.d0 * ulp_f
       error stop
    end if
  end subroutine expect_float_eq

  subroutine expect_eq (a, b)
    integer(int32), intent(in) :: a, b
    if ( .not.a == b ) then
       write(*, *) a, "!=", b
       error stop
    end if
  end subroutine expect_eq

  elemental subroutine expect_true (a)
    logical, intent(in) :: a
    if ( .not. a ) then
       error stop
    end if
  end subroutine expect_true

end module test_mod
