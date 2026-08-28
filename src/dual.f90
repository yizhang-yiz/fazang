module fz_dual
  use, intrinsic :: iso_fortran_env
  use, intrinsic :: iso_c_binding
  use fz_env

  type, bind(c) :: dual
    real(rk) :: v
    real(rk) :: dv = 0.0d0
  end type dual

  interface operator(+)
    module procedure add_dd, add_dr, add_rd
  end interface
  interface operator(-)
    module procedure sub_dd, sub_dr, sub_rd, neg_d
  end interface
  interface operator(*)
    module procedure mul_dd, mul_dr, mul_rd
  end interface
  interface operator(/)
    module procedure div_dd, div_dr, div_rd
  end interface
  interface operator(**)
    module procedure pow_dd, pow_dr, pow_rd
  end interface
  interface assignment(=)
    module procedure new_dual_val, new_dual
  end interface

  interface sin; module procedure sin_d; end interface
  interface cos; module procedure cos_d; end interface
  interface tan; module procedure tan_d; end interface
  interface exp; module procedure exp_d; end interface
  interface log; module procedure log_d; end interface
  interface log10; module procedure log10_d; end interface
  interface sqrt; module procedure sqrt_d; end interface
  interface abs; module procedure abs_d; end interface
  interface atan; module procedure atan_d; end interface
  interface asin; module procedure asin_d; end interface
  interface acos; module procedure acos_d; end interface

contains

  elemental subroutine new_dual_val(d, v)
    implicit none
    type(dual), intent(out) :: d
    real(rk), intent(in) :: v
    d%v = v
    d%dv = 1.d0
  end subroutine new_dual_val

  elemental subroutine new_dual(d, d1)
    implicit none
    type(dual), intent(out) :: d
    type(dual), intent(in) :: d1
    d%v = d1%v
    d%dv = d1%dv
  end subroutine new_dual

  pure function add_dd(a, b) result(c)
    implicit none
    type(dual), intent(in) :: a, b
    type(dual) :: c
    c%v = a%v + b%v
    c%dv = a%dv + b%dv
  end function
  pure function add_dr(a, b) result(c)
    implicit none
    type(dual), intent(in) :: a
    real(rk), intent(in) :: b
    type(dual) :: c
    c%v = a%v + b
    c%dv = a%dv
  end function
  pure function add_rd(a, b) result(c)
    implicit none
    real(rk), intent(in) :: a
    type(dual), intent(in) :: b
    type(dual) :: c
    c = b + a
  end function

  pure function neg_d(a) result(c)
    implicit none
    type(dual), intent(in) :: a
    type(dual) :: c
    c%v = -a%v
    c%dv = -a%dv
  end function
  pure function sub_dd(a, b) result(c)
    implicit none
    type(dual), intent(in) :: a, b
    type(dual) :: c
    c%v = a%v - b%v
    c%dv = a%dv - b%dv
  end function
  pure function sub_dr(a, b) result(c)
    implicit none
    type(dual), intent(in) :: a
    real(rk), intent(in) :: b
    type(dual) :: c
    c%v = a%v - b
    c%dv = a%dv
  end function
  pure function sub_rd(a, b) result(c)
    implicit none
    real(rk), intent(in) :: a
    type(dual), intent(in) :: b
    type(dual) :: c
    c%v = a - b%v
    c%dv = -b%dv
  end function

  function mul_dd(a, b) result(c)
    implicit none
    type(dual), intent(in) :: a, b
    type(dual) :: c
    c%v = a%v * b%v
    c%dv = a%dv*b%v + a%v*b%dv
  end function
  pure function mul_dr(a, b) result(c)
    implicit none
    type(dual), intent(in) :: a
    real(rk), intent(in) :: b
    type(dual) :: c
    c%v = a%v * b
    c%dv = a%dv * b
  end function
  pure function mul_rd(a, b) result(c)
    implicit none
    real(rk), intent(in) :: a
    type(dual), intent(in) :: b
    type(dual) :: c
    c = mul_dr(b, a)
  end function

  pure function div_dd(a, b) result(c)
    implicit none
    type(dual), intent(in) :: a, b
    type(dual) :: c
    c%v = a%v / b%v
    c%dv = (a%dv*b%v - a%v*b%dv) / (b%v*b%v)
  end function
  pure function div_dr(a, b) result(c)
    implicit none
    type(dual), intent(in) :: a
    real(rk), intent(in) :: b
    type(dual) :: c
    c%v = a%v / b
    c%dv = a%dv / b
  end function
  pure function div_rd(a, b) result(c)
    implicit none
    real(rk), intent(in) :: a
    type(dual), intent(in) :: b
    type(dual) :: c
    c%v = a / b%v
    c%dv = (-a*b%dv) / (b%v*b%v)
  end function

  pure function pow_dd(a, b) result(c)
    implicit none
    type(dual), intent(in) :: a, b
    type(dual) :: c
    c%v = a%v ** b%v
    c%dv = c%v * (b%dv*log(a%v) + b%v*a%dv/a%v)
  end function
  pure function pow_dr(a, b) result(c)
    implicit none
    type(dual), intent(in) :: a
    real(rk), intent(in) :: b
    type(dual) :: c
    c%v = a%v ** b
    c%dv = b * (a%v ** (b - 1.0)) * a%dv
  end function
  pure function pow_rd(a, b) result(c)
    implicit none
    real(rk), intent(in) :: a
    type(dual), intent(in) :: b
    type(dual) :: c
    c%v = a ** b%v
    c%dv = c%v * log(a) * b%dv
  end function

  pure function sin_d(x) result(y)
    implicit none
    type(dual), intent(in) :: x
    type(dual) :: y
    y%v = sin(x%v)
    y%dv = cos(x%v) * x%dv
  end function
  pure function cos_d(x) result(y)
    implicit none
    type(dual), intent(in) :: x
    type(dual) :: y
    y%v = cos(x%v)
    y%dv = -sin(x%v) * x%dv
  end function
  pure function tan_d(x) result(y)
    implicit none
    type(dual), intent(in) :: x
    type(dual) :: y
    y%v = tan(x%v)
    y%dv = x%dv / cos(x%v)**2
  end function
  pure function exp_d(x) result(y)
    implicit none
    type(dual), intent(in) :: x
    type(dual) :: y
    y%v = exp(x%v)
    y%dv = y%v * x%dv
  end function
  pure function log_d(x) result(y)
    implicit none
    type(dual), intent(in) :: x
    type(dual) :: y
    y%v = log(x%v)
    y%dv = x%dv / x%v
  end function log_d
  pure function log10_d(x) result(y)
    implicit none
    type(dual), intent(in) :: x
    type(dual) :: y
    y%v = log10(x%v)
    y%dv = x%dv/(x%v*dlog(10.d0))
  end function log10_d
  pure function sqrt_d(x) result(y)
    implicit none
    type(dual), intent(in) :: x
    type(dual) :: y
    y%v = sqrt(x%v)
    y%dv = x%dv / (2.0*y%v)
  end function
  pure function abs_d(x) result(y)
    implicit none
    type(dual), intent(in) :: x
    type(dual) :: y
    y%v = abs(x%v)
    y%dv = merge(x%dv, -x%dv, x%v >= 0.0)
  end function
  pure function atan_d(x) result(y)
    implicit none
    type(dual), intent(in) :: x
    type(dual) :: y
    y%v = atan(x%v)
    y%dv = x%dv / (1.0 + x%v*x%v)
  end function
  pure function asin_d(x) result(y)
    implicit none
    type(dual), intent(in) :: x
    type(dual) :: y
    y%v = asin(x%v)
    y%dv = x%dv / sqrt(1.0 - x%v*x%v)
  end function
  pure function acos_d(x) result(y)
    implicit none
    type(dual), intent(in) :: x
    type(dual) :: y
    y%v = acos(x%v)
    y%dv = -x%dv / sqrt(1.0 - x%v*x%v)
  end function acos_d

end module fz_dual
