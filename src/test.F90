module fazang_test_mod

  use, intrinsic :: iso_fortran_env
  implicit none

  real(REAL32), parameter :: ulp_f = 1.19209290e-7
  real(REAL64), parameter :: ulp_d = 2.2204460492503131e-16

  interface expect_dbl_eq_impl
     module procedure :: expect_dbl_eq_impl_0
     module procedure :: expect_dbl_eq_impl_1
     module procedure :: expect_dbl_eq_impl_2
  end interface expect_dbl_eq_impl

contains

  subroutine expect_near_impl(a, b, tol, file, line)
    real(REAL64), intent(in) :: a, b, tol
    character (*) :: file
    integer(int32) :: line
    if ( abs(a - b) >= tol ) then
       write(*, *) file, "   line:", line
       write(*, *)
       write(*, *) "|", a, "-", b, "| =", abs(a-b), " >", tol
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
       write(*, *) file, "   line:", line
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

  subroutine expect_dbl_eq_impl_0 (a, b, file, line)
    real(REAL64), intent(in) :: a, b
    character (*) :: file
    integer(int32) :: line
    if ( abs(a - b) >= 4.d0 * ulp_d ) then
       write(*, *) file, "   line:", line
       write(*, *)
       write(*, *) "|", a, "-", b, "| >", 4.d0 * ulp_d
       stop 5
    end if
  end subroutine expect_dbl_eq_impl_0

  subroutine expect_dbl_eq_impl_1 (a, b, file, line)
    real(REAL64), intent(in) :: a(:), b(:)
    character (*) :: file
    integer(int32) :: line, i
    do i = 1, size(a)
       if ( abs(a(i) - b(i)) >= 4.d0 * ulp_d ) then
          write(*, *) file, "   line:", line, "   i=", i
          write(*, *)
          write(*, *) "|", a(i), "-", b(i), "| >", 4.d0 * ulp_d
          stop 5
       end if
    end do
  end subroutine expect_dbl_eq_impl_1

  subroutine expect_dbl_eq_impl_2 (a, b, file, line)
    real(REAL64), intent(in) :: a(:, :), b(:, :)
    character (*) :: file
    integer(int32) :: line, i, j
    do i = 1, size(a, 2)
       do j = 1, size(a, 1)
          if ( abs(a(j, i) - b(j, i)) >= 4.d0 * ulp_d ) then
             write(*, *) file, "   line:", line, "   row=", j, " col=", i
             write(*, *)
             write(*, *) "|", a(j, i), "-", b(j, i), "| >", 4.d0 * ulp_d
             stop 5
          end if
       end do
    end do
  end subroutine expect_dbl_eq_impl_2

end module fazang_test_mod
