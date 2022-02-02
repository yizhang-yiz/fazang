module rel_operator_mod
  use iso_fortran_env
  use env_mod
  use var_mod, only : var

  implicit none

  private
  public :: operator(==), operator(/=), operator(>), operator(<)
  public :: operator(>=), operator(<=)

#define INTERFACE_REL_OP(S, F1, F2, F3) interface operator(S); module procedure F1; module procedure F2; module procedure F3; end interface operator(S)

  INTERFACE_REL_OP(==, var_eq, dv_eq, vd_eq)
  INTERFACE_REL_OP(/=, var_ne, dv_ne, vd_ne)
  INTERFACE_REL_OP(>, var_gt, dv_gt, vd_gt)
  INTERFACE_REL_OP(<, var_lt, dv_lt, vd_lt)
  INTERFACE_REL_OP(>=, var_ge, dv_ge, vd_ge)
  INTERFACE_REL_OP(<=, var_le, dv_le, vd_le)

#undef INTERFACE_REL_OP

contains

#define FUNC_REL_OP_VV(S, F) elemental function F (a, b) result(r); type(var), intent(in) :: a, b; logical :: r; r = a%val() S b%val(); end function F
#define FUNC_REL_OP_VD(S, F) elemental function F (a, b) result(r); type(var), intent(in) :: a; real(rk), intent(in) :: b; logical :: r; r = a%val() S b; end function F
#define FUNC_REL_OP_DV(S, F) elemental function F (a, b) result(r); real(rk), intent(in) :: a; type(var), intent(in) :: b; logical :: r; r = b%val() S a; end function F

  FUNC_REL_OP_VV(==, var_eq)
  FUNC_REL_OP_VD(==, vd_eq)
  FUNC_REL_OP_DV(==, dv_eq)

  FUNC_REL_OP_VV(/=, var_ne)
  FUNC_REL_OP_VD(/=, vd_ne)
  FUNC_REL_OP_DV(/=, dv_ne)

  FUNC_REL_OP_VV(>, var_gt)
  FUNC_REL_OP_VD(>, vd_gt)
  FUNC_REL_OP_DV(>, dv_gt)

  FUNC_REL_OP_VV(<, var_lt)
  FUNC_REL_OP_VD(<, vd_lt)
  FUNC_REL_OP_DV(<, dv_lt)

  FUNC_REL_OP_VV(>=, var_ge)
  FUNC_REL_OP_VD(>=, vd_ge)
  FUNC_REL_OP_DV(>=, dv_ge)

  FUNC_REL_OP_VV(<=, var_le)
  FUNC_REL_OP_VD(<=, vd_le)
  FUNC_REL_OP_DV(<=, dv_le)

end module rel_operator_mod
