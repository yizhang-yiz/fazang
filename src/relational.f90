module rel_operator_mod
  use iso_fortran_env
  use var_mod, only : var

  implicit none

  private
  public :: operator(==), operator(/=), operator(>), operator(<)
  public :: operator(>=), operator(<=)

  interface operator(==)
     module procedure var_eq
  end interface operator(==)

  interface operator(/=)
     module procedure var_ne
  end interface operator(/=)

  interface operator(>)
     module procedure var_gt
  end interface operator(>)

  interface operator(<)
     module procedure var_lt
  end interface operator(<)

  interface operator(>=)
     module procedure var_ge
  end interface operator(>=)

  interface operator(<=)
     module procedure var_le
  end interface operator(<=)

contains

  pure function var_eq(a, b) result(r)
    type(var), intent(in) :: a, b
    logical :: r
    r = a%val() == b%val()
  end function var_eq

  pure function var_ne(a, b) result(r)
    type(var), intent(in) :: a, b
    logical :: r
    r = a%val() /= b%val()
  end function var_ne

  pure function var_gt(a, b) result(r)
    type(var), intent(in) :: a, b
    logical :: r
    r = a%val() > b%val()
  end function var_gt

  pure function var_lt(a, b) result(r)
    type(var), intent(in) :: a, b
    logical :: r
    r = a%val() < b%val()
  end function var_lt

  pure function var_ge(a, b) result(r)
    type(var), intent(in) :: a, b
    logical :: r
    r = a%val() >= b%val()
  end function var_ge

  pure function var_le(a, b) result(r)
    type(var), intent(in) :: a, b
    logical :: r
    r = a%val() <= b%val()
  end function var_le

end module rel_operator_mod
