module matmul_mod
  use iso_fortran_env
  use env_mod
  use var_mod, only: var, val, adj, vi_index
  use vari_mod, only: vari, callstack
  use dot_product_mod

  implicit none

  private
  public :: matmul

  interface matmul
     module procedure mat_mul_vv
     module procedure mat_mul_vd
     module procedure mat_mul_dv
  end interface matmul

contains

  ! For matrix mul C=AB, c_ij = sum(a_ik * b_kj)
  ! thus d(c_ij)/d(a_ik) = b_kj
  ! 
  subroutine chain_matmul_vv(this)
    class(vari), intent(in) :: this
    integer(ik) :: info(4)
    integer(ik) :: row_id(callstack%stack%matrix_ncol(this%matrix_operand_1_index()))
    integer(ik) :: col_id(callstack%stack%matrix_nrow(this%matrix_operand_2_index()))
    info = this%operand_index()
    row_id = callstack % stack % matrix_row_index(info(3), info(1))
    col_id = callstack % stack % matrix_col_index(info(4), info(2))
    call chain_dot_product(this, row_id, col_id)
  end subroutine chain_matmul_vv

  function mat_mul_vv(a, b) result(s)
    type(var), intent(in) :: a(:, :), b(:, :)
    type(var) :: s(size(a, 1), size(b, 2))
    real(rk) :: a_val(size(a,1), size(a,2)), b_val(size(b, 1), size(b, 2))
    real(rk) :: s_val(size(a,1), size(b,2))
    integer(ik) :: i, j, a_id, b_id
    a_id = callstack % stack % push([shape(a), vi_index(a)])
    b_id = callstack % stack % push([shape(b), vi_index(b)])
    a_val = val(a)
    b_val = val(b)
    s_val = matmul(a_val, b_val)
    do j = 1, size(b, 2)
       do i = 1, size(a, 1)
          s(i, j) = var(s_val(i, j), [i, j, a_id, b_id])
          s(i, j)%vi%chain => chain_matmul_vv
       end do
    end do
  end function mat_mul_vv

  subroutine chain_matmul_vd(this)
    class(vari), intent(in) :: this
    integer(ik) :: info(4)
    integer(ik) :: row_id(callstack%stack%matrix_ncol(this%matrix_operand_1_index()))
    real(rk) :: col(callstack%stack%matrix_nrow(this%matrix_operand_2_index()))
    info = this%operand_index()
    row_id = callstack % stack % matrix_row_index(info(3), info(1))
    col = callstack % stack % matrix_col_data(info(4), info(2))
    call chain_dot_product(this, row_id, col)
  end subroutine chain_matmul_vd

  function mat_mul_vd(a, b) result(s)
    type(var), intent(in) :: a(:, :) 
    real(rk), intent(in) :: b(:, :)
    type(var) :: s(size(a, 1), size(b, 2))
    real(rk) :: a_val(size(a,1), size(a,2))
    real(rk) :: s_val(size(a,1), size(b,2))
    integer(ik) :: i, j, a_id, b_id
    a_id = callstack % stack % push([shape(a), vi_index(a)])
    b_id = callstack % stack % push(shape(b))
    i = callstack % stack % push(b)
    a_val = val(a)
    s_val = matmul(a_val, b)
    do j = 1, size(b, 2)
       do i = 1, size(a, 1)
          s(i, j) = var(s_val(i, j), [i, j, a_id, b_id])
          s(i, j)%vi%chain => chain_matmul_vd
       end do
    end do
  end function mat_mul_vd

  subroutine chain_matmul_dv(this)
    class(vari), intent(in) :: this
    integer(ik) :: info(4)
    real(rk) :: row(callstack%stack%matrix_ncol(this%matrix_operand_1_index()))
    integer(ik) :: col_id(callstack%stack%matrix_nrow(this%matrix_operand_2_index()))
    info = this%operand_index()
    row = callstack % stack % matrix_row_data(info(3), info(1))
    col_id = callstack % stack % matrix_col_index(info(4), info(2))
    call chain_dot_product(this, row, col_id)
  end subroutine chain_matmul_dv

  function mat_mul_dv(a, b) result(s)
    real(rk), intent(in) :: a(:, :)
    type(var), intent(in) :: b(:, :) 
    type(var) :: s(size(a, 1), size(b, 2))
    real(rk) :: b_val(size(b,1), size(b,2))
    real(rk) :: s_val(size(a,1), size(b,2))
    integer(ik) :: i, j, a_id, b_id
    a_id = callstack % stack % push(shape(a))
    i = callstack % stack % push(a)
    b_id = callstack % stack % push([shape(b), vi_index(b)])
    b_val = val(b)
    s_val = matmul(a, b_val)
    do j = 1, size(b, 2)
       do i = 1, size(a, 1)
          s(i, j) = var(s_val(i, j), [i, j, a_id, b_id])
          s(i, j)%vi%chain => chain_matmul_dv
       end do
    end do
  end function mat_mul_dv
  
end module matmul_mod
