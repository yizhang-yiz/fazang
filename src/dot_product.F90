module fazang_dot_product_mod
  use iso_fortran_env
  use fazang_env_mod
  use fazang_var_mod, only: var, val, adj
  use fazang_vari_mod, only: vari, callstack

  implicit none

  private
  public :: dot_product
  public :: chain_dot_product

  interface dot_product
     module procedure dot_prod_vv
     module procedure dot_prod_vd
     module procedure dot_prod_dv
  end interface dot_product

  interface chain_dot_product
     module procedure :: chain_dot_product_vv
     module procedure :: chain_dot_product_vd
     module procedure :: chain_dot_product_dv
  end interface chain_dot_product

contains

  function dot_prod_vv(a, b) result(s)
    type(var), intent(in) :: a(:), b(:)
    type(var) :: s
    s = var(dot_product(val(a), val(b)), [a, b])
    call s%set_chain(chain_dot_vv)
  end function dot_prod_vv

  ! For matrix mul C=AB, c_ij = sum(a_ik * b_kj)
  ! thus d(c_ij)/d(a_ik) = b_kj
  ! 
  subroutine chain_dot_vv(this)
    class(vari), intent(in) :: this
    integer(ik) :: i(this%n_operand()), n

    ! operand index is the two arrays' ids
    n = this%n_operand() / 2
    i = this%operand_index()
    call chain_dot_product_vv(this, i(1:n), i(n+1:2*n))
  end subroutine chain_dot_vv

  subroutine chain_dot_product_vv(this, row_id, col_id)
    class(vari), intent(in) :: this
    integer(ik), intent(in) :: row_id(:), col_id(:)
    real(rk) :: adj(size(row_id))
    adj = this%adj() * callstack % stack % val(col_id)
    call this%set_operand_adj(row_id, adj)
    adj = this%adj() * callstack % stack % val(row_id)
    call this%set_operand_adj(col_id, adj)
  end subroutine chain_dot_product_vv

  function dot_prod_vd(a, b) result(s)
    type(var), intent(in) :: a(:)
    real(rk), intent(in) :: b(:)
    type(var) :: s
    s = var(dot_product(val(a), b), a, b)
    call s%set_chain(chain_dot_vd)
  end function dot_prod_vd

  function dot_prod_dv(a, b) result(s)
    real(rk), intent(in) :: a(:)
    type(var), intent(in) :: b(:)
    type(var) :: s
    s = var(dot_product(a, val(b)), b, a)
    call s%set_chain(chain_dot_vd)
  end function dot_prod_dv

  subroutine chain_dot_vd(this)
    class(vari), intent(in) :: this
    integer(ik) :: i(this%n_operand())
    real(rk) :: d(size(this%data_operand()))
    i = this%operand_index()
    d = this%data_operand()
    call chain_dot_product_vd(this, i, d)
  end subroutine chain_dot_vd

  subroutine chain_dot_product_vd(this, row_id, col)
    class(vari), intent(in) :: this
    integer(ik), intent(in) :: row_id(:)
    real(rk), intent(in) :: col(:)
    call this%set_operand_adj(row_id, this%adj() * col)
  end subroutine chain_dot_product_vd

  subroutine chain_dot_product_dv(this, row, col_id)
    class(vari), intent(in) :: this
    real(rk), intent(in) :: row(:)
    integer(ik), intent(in) :: col_id(:)
    call chain_dot_product_vd(this, col_id, row)
  end subroutine chain_dot_product_dv

end module fazang_dot_product_mod
