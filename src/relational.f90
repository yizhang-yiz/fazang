module rel_operator_mod
  use iso_fortran_env
  use env_mod
  use var_mod, only : var

  implicit none

  private
  public :: operator(==), operator(/=), operator(>), operator(<)
  public :: operator(>=), operator(<=)

  interface operator(==)
     module procedure var_eq
     module procedure vd_eq
     module procedure dv_eq
  end interface operator(==)

  interface operator(/=)
     module procedure var_ne
     module procedure dv_ne
     module procedure vd_ne
  end interface operator(/=)

  interface operator(>)
     module procedure var_gt
     module procedure dv_gt
     module procedure vd_gt
  end interface operator(>)

  interface operator(<)
     module procedure var_lt
     module procedure vd_lt
     module procedure dv_lt
  end interface operator(<)

  interface operator(>=)
     module procedure var_ge
     module procedure vd_ge
     module procedure dv_ge
  end interface operator(>=)

  interface operator(<=)
     module procedure var_le
     module procedure vd_le
     module procedure dv_le
  end interface operator(<=)

contains

  elemental function var_eq(a, b) result(r)
    type(var), intent(in) :: a, b
    logical :: r
    r = a%val() == b%val()
  end function var_eq

  elemental function vd_eq(a, b) result(r)
    type(var), intent(in) :: a
    real(rk), intent(in) :: b
    logical :: r
    r = a%val() == b
  end function vd_eq

  elemental function dv_eq(a, b) result(r)
    real(rk), intent(in) :: a
    type(var), intent(in) :: b
    logical :: r
    r = b%val() == a
  end function dv_eq

  elemental function var_ne(a, b) result(r)
    type(var), intent(in) :: a, b
    logical :: r
    r = a%val() /= b%val()
  end function var_ne

  elemental function vd_ne(a, b) result(r)
    type(var), intent(in) :: a
    real(rk), intent(in) :: b
    logical :: r
    r = a%val() /= b
  end function vd_ne

  elemental function dv_ne(a, b) result(r)
    real(rk), intent(in) :: a
    type(var), intent(in) :: b
    logical :: r
    r = a /= b%val()
  end function dv_ne

  elemental function var_gt(a, b) result(r)
    type(var), intent(in) :: a, b
    logical :: r
    r = a%val() > b%val()
  end function var_gt

  elemental function vd_gt(a, b) result(r)
    type(var), intent(in) :: a
    real(rk), intent(in) :: b
    logical :: r
    r = a%val() > b
  end function vd_gt

  elemental function dv_gt(a, b) result(r)
    real(rk), intent(in) :: a
    type(var), intent(in) :: b
    logical :: r
    r = a > b%val()
  end function dv_gt

  elemental function var_lt(a, b) result(r)
    type(var), intent(in) :: a, b
    logical :: r
    r = a%val() < b%val()
  end function var_lt

  elemental function vd_lt(a, b) result(r)
    type(var), intent(in) :: a
    real(rk), intent(in) :: b
    logical :: r
    r = a%val() < b
  end function vd_lt

  elemental function dv_lt(a, b) result(r)
    real(rk), intent(in) :: a
    type(var), intent(in) :: b
    logical :: r
    r = a < b%val()
  end function dv_lt

  elemental function var_ge(a, b) result(r)
    type(var), intent(in) :: a, b
    logical :: r
    r = a%val() >= b%val()
  end function var_ge

  elemental function vd_ge(a, b) result(r)
    type(var), intent(in) :: a
    real(rk), intent(in) :: b
    logical :: r
    r = a%val() >= b
  end function vd_ge

  elemental function dv_ge(a, b) result(r)
    real(rk), intent(in) :: a
    type(var), intent(in) :: b
    logical :: r
    r = a >= b%val()
  end function dv_ge

  elemental function var_le(a, b) result(r)
    type(var), intent(in) :: a, b
    logical :: r
    r = a%val() <= b%val()
  end function var_le

  elemental function vd_le(a, b) result(r)
    type(var), intent(in) :: a
    real(rk), intent(in) :: b
    logical :: r
    r = a%val() <= b
  end function vd_le

  elemental function dv_le(a, b) result(r)
    real(rk), intent(in) :: a
    type(var), intent(in) :: b
    logical :: r
    r = a <= b%val()
  end function dv_le

end module rel_operator_mod
