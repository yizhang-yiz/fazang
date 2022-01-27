program var_test
  use, intrinsic :: iso_fortran_env
  use adstack_mod
  use test_mod
  use var_mod
  implicit none

  type(adstack), target :: t
  type(var) :: x1, x2, x3
  type(var) :: y(3), a(2, 3)
  real(real64) :: z(3) = (/0.2d0, 0.5d0, 0.3d0/)
  real(real64) :: b(2, 3) = reshape((/1.2d0, 2.2d0, 5.2d0, 1.3d0,&
       & 2.3d0, 5.3d0/), shape(a))
  integer i, j

  current_adstack => t

  x1 = var(0.1d0)
  x2 = var(0.2d0)
  x3 = x2
  call expect_float_eq(x1%val(), 0.1d0)
  call expect_float_eq(x2%val(), 0.2d0)
  call expect_float_eq(x3%val(), 0.2d0)
  x1 = x2
  call expect_float_eq(x1%val(), 0.2d0)
  call expect_eq(t%head, 3)

  do i = 1, 3
     y(i) = var()
  end do
  y = z
  do i = 1, 3
     call expect_float_eq(y(i)%val(), z(i))
     call expect_eq(y(i)%vi%i, 3 + i - 1)
  end do
  call expect_eq(t%head, 6)

  do j = 1, 3
     do i = 1, 2
        a(i, j) = var()
     end do
  end do
  a = b
  do j = 1, 3
     do i = 1, 2
        call expect_float_eq(a(i, j)%val(), b(i, j))
        call expect_eq(a(i, j)%vi%i, 6 + i + (j-1) * 2 - 1)
     end do
  end do

  y = a(1, :)
  do i = 1, 3
     call expect_float_eq(y(i)%val(), b(1, i))
     call expect_eq(y(i)%vi%i, a(1, i)%vi%i)
     call expect_true(associated(y(i)%vi%adstack, current_adstack))
  end do
  call expect_eq(t%head, 12)

end program var_test
