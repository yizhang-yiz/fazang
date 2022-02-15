module fazang_pow_mod
  use fazang_vari_mod, only : vari
  use fazang_env_mod
  use fazang_var_mod
  use fazang_inv_mod
  use fazang_square_mod
  use fazang_inv_square_mod
  use fazang_sqrt_mod
  use fazang_inv_sqrt_mod
  use fazang_rel_operator_mod
  implicit none

  private
  public :: operator (**)

  interface operator (**)
     module procedure :: pow_vv
     module procedure :: pow_vd
     module procedure :: pow_dv
  end interface operator (**)

contains

  subroutine chain_pow_vv(this)
    class(vari), intent(in) :: this
    real(rk) :: adj(2), val(2)
    val = this%operand_val()
    if ( val(1) .ne. 0.d0 ) then
       adj(1) = this%adj() * val(2) * this%val() / val(1)
       adj(2) = this%adj() * this%val() * log(val(1))
       call this%set_operand_adj(adj)
    end if
  end subroutine chain_pow_vv

  subroutine chain_pow_vd(this)
    class(vari), intent(in) :: this
    real(rk) :: adj(1), val(1), d(1)
    val = this%operand_val()
    d = this%data_operand()
    if ( val(1) .ne. 0.d0 ) then
       adj(1) = this%adj() * d(1) * this%val() / val(1)
       call this%set_operand_adj(adj)
    end if
  end subroutine chain_pow_vd

  subroutine chain_pow_dv(this)
    class(vari), intent(in) :: this
    real(rk) :: adj(1), d(1)
    d = this%data_operand()
    if ( d(1) .ne. 0.d0 ) then
       adj(1) = this%adj() * this%val() * log(d(1))
       call this%set_operand_adj(adj)
    end if
  end subroutine chain_pow_dv

  impure elemental function pow_vv(v1, v2) result(s)
    type(var), intent(in) :: v1, v2
    type(var) :: s
    s = var(v1%val() ** v2%val(), [v1, v2])
    call s%set_chain(chain_pow_vv)
  end function pow_vv

  impure elemental function pow_vd(v1, v2) result(s)
    type(var), intent(in) :: v1
    real(rk), intent(in) :: v2
    type(var) :: s
    if ( v2 == 0.5d0 ) then
       s = sqrt(v1)
    else if ( v2 == 1.0d0 ) then
       s = v1
    else if ( v2 == 2.0d0 ) then
       s = square(v1)
    else if ( v2 == -2.0d0 ) then
       s = inv_square(v1)
    else if ( v2 == -1.0d0 ) then
       s = inv(v1)
    else if ( v2 == -0.5d0 ) then
       s = inv_sqrt(v1)
    else
       s = var(v1%val() ** v2, [v1], [v2])
       call s%set_chain(chain_pow_vd)
    endif
  end function pow_vd

  impure elemental function pow_dv(v1, v2) result(s)
    type(var), intent(in) :: v2
    real(rk), intent(in) :: v1
    type(var) :: s
    s = var(v1 ** v2%val(), [v2], [v1])
    call s%set_chain(chain_pow_dv)
  end function pow_dv

end module fazang_pow_mod
