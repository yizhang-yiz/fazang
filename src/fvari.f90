module fz_fvari
  use, intrinsic :: iso_fortran_env
  use, intrinsic :: iso_c_binding
  use fz_env

  type, bind(c) :: dual
    real(rk) :: v = 0.0d0
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

  type, bind(c) :: fvari
     ! real(rk) :: val_
     type(dual) :: val_
     type(dual) :: adj_ = dual(0d0, 0d0)
     integer(ik) :: i = 0 ! this vari location in storage
     integer(ik) :: j = 0 ! vari before i in storage (for rev pass)
     type(c_funptr) :: chain = c_null_funptr
  end type fvari

  integer(ik), parameter :: visize = storage_size(fvari(dual(0.d0,0.d0)))/8

  interface assignment(=)
     module procedure new_fvari_val
     module procedure new_fvari_real32
     module procedure new_fvari_dual
     module procedure new_dual_val
     module procedure new_dual
  end interface assignment(=)

  abstract interface
     subroutine chain_op(p)
       import
       implicit none
       type(fvari), pointer, intent(in) :: p
     end subroutine chain_op

     real(rk) function op1(x)
       use fz_env
       real(rk), intent(in) :: x
     end function op1

     real(rk) function op2(a, b)
       use fz_env
       real(rk), intent(in) :: a, b
     end function op2
  end interface

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

  subroutine recover(p, i)
    implicit none
    type(fvari), pointer, intent(out) :: p
    integer(ik), intent(in) :: i
    type(c_ptr) :: cp
    p => null()
    if (i > 0) then
       cp = c_loc(core_adstack%s_(i))
       call c_f_pointer(cp, p)
    end if
  end subroutine recover

  subroutine recover_parent(p, p1)
    implicit none
    type(fvari), pointer, intent(in) :: p
    type(fvari), pointer, intent(out) :: p1
    integer(ik), pointer :: i
    type(c_ptr) :: cp
    cp = c_loc(core_adstack%s_(p%i+visize))
    call c_f_pointer(cp, i)
    if (i > 0) then
       cp = c_loc(core_adstack%s_(i))
       call c_f_pointer(cp, p1)
    end if
  end subroutine recover_parent

  subroutine recover_parent2(p, p1, p2)
    implicit none
    type(fvari), pointer, intent(in) :: p
    type(fvari), pointer, intent(out) :: p1, p2
    integer(ik), pointer :: i
    type(c_ptr) :: cp
    cp = c_loc(core_adstack%s_(p%i+visize))
    call c_f_pointer(cp, i)
    if (i > 0) then
       cp = c_loc(core_adstack%s_(i))
       call c_f_pointer(cp, p1)
    end if
    cp = c_loc(core_adstack%s_(p%i+visize+iksize))
    call c_f_pointer(cp, i)
    if (i > 0) then
       cp = c_loc(core_adstack%s_(i))
       call c_f_pointer(cp, p2)
    end if
  end subroutine recover_parent2

  subroutine recover_parent_real(p, p1, b)
    implicit none
    type(fvari), pointer, intent(in) :: p
    type(fvari), pointer, intent(out) :: p1
    real(rk), intent(out) :: b
    integer(ik), pointer :: i
    real(rk), pointer :: pb
    type(c_ptr) :: cp
    cp = c_loc(core_adstack%s_(p%i+visize))
    call c_f_pointer(cp, i)
    if (i > 0) then
       cp = c_loc(core_adstack%s_(i))
       call c_f_pointer(cp, p1)
    end if
    cp = c_loc(core_adstack%s_(p%i+visize+iksize))
    call c_f_pointer(cp, pb)
    b = pb
  end subroutine recover_parent_real

  elemental function vi_val_v(this) result(v)
    implicit none
    type(fvari), intent(in) :: this
    real(rk) :: v
    v = this%val_%v
  end function vi_val_v

  elemental function vi_val_dv(this) result(v)
    implicit none
    type(fvari), intent(in) :: this
    real(rk) :: v
    v = this%val_%dv
  end function vi_val_dv

  elemental function vi_adj_v(this) result(v)
    implicit none
    type(fvari), intent(in) :: this
    real(rk) :: v
    v = this%adj_%v
  end function vi_adj_v

  elemental function vi_adj_dv(this) result(v)
    implicit none
    type(fvari), intent(in) :: this
    real(rk) :: v
    v = this%adj_%dv
  end function vi_adj_dv

  subroutine new_fvari_val(this, val)
    implicit none
    type(fvari), pointer, intent(out) :: this
    real(rk), intent(in) :: val
    call recover(this, core_adstack%i_)
    this%val_ = dual(val, 0.d0)
    this%adj_ = dual(0.d0, 0.d0)
    this%i = core_adstack%i_
    this%j = core_adstack%j_
    this%chain = c_funloc(chain_dummy)
    call core_adstack%incr(visize, .true.)
  end subroutine new_fvari_val

  subroutine new_fvari_real32(this, val)
    implicit none
    type(fvari), pointer, intent(out) :: this
    real(c_float), intent(in) :: val
    call recover(this, core_adstack%i_)
    this%val_ = dual(val, 0.d0)
    this%adj_ = dual(0.d0, 0.d0)
    this%chain = c_funloc(chain_dummy)
    this%i = core_adstack%i_
    this%j = core_adstack%j_
    call core_adstack%incr(visize, .true.)
  end subroutine new_fvari_real32

  subroutine new_fvari_dual(this, d)
    implicit none
    type(fvari), pointer, intent(out) :: this
    type(dual), intent(in) :: d
    call recover(this, core_adstack%i_)
    this%val_ = d
    this%adj_ = dual(0.d0, 0.d0)
    this%i = core_adstack%i_
    this%j = core_adstack%j_
    this%chain = c_funloc(chain_dummy)
    call core_adstack%incr(visize, .true.)
  end subroutine new_fvari_dual

  ! skip chain and return previous vi in AD stack
  subroutine chain_dummy(p)
    implicit none
    type(fvari), pointer :: p
  end subroutine

  subroutine reset_chain(p)
    implicit none
    type(fvari), pointer, intent(in) :: p
    type(fvari), pointer :: p1
    p1 => p
    do while (associated(p1))
       p1%val_%dv = 0.d0
       p1%adj_ = dual(0.d0, 0.d0)
       call recover(p1, p1%j)
    enddo
  end subroutine reset_chain

  subroutine chain(p)
    implicit none
    type(fvari), pointer, intent(in) :: p
    type(fvari), pointer :: p1
    procedure(chain_op), pointer :: p1_chain
    p%adj_ = dual(1.0d0, 0.0d0)
    p1 => p
    do while (associated(p1))
       call c_f_procpointer(p1%chain, p1_chain)
       call p1_chain(p1)
       call recover(p1, p1%j)
    enddo
  end subroutine chain

end module fz_fvari
