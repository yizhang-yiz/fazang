module fazang_matmul_mod
  use iso_fortran_env
  use fazang_env_mod
  use fazang_var_mod, only: var, val, adj, vi_index
  use fazang_vari_mod, only: vari, callstack, vari_index
  use fazang_array_in_tape_mod
  use fazang_dot_product_mod

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
    type(array_in_tape) :: mat1, mat2
    info = this%operand_index()
    mat1 = array_in_tape(callstack % stack, info(3), 2)
    mat2 = array_in_tape(callstack % stack, info(4), 2)
    call chain_dot_product(this, mat1%row(info(1)), mat2%col(info(2)))
  end subroutine chain_matmul_vv

  function mat_mul_vv(a, b) result(s)
    type(var), intent(in) :: a(:, :), b(:, :)
    type(var) :: s(size(a, 1), size(b, 2))
    real(rk) :: a_val(size(a,1), size(a,2)), b_val(size(b, 1), size(b, 2))
    real(rk) :: s_val(size(a,1), size(b,2))
    integer(ik) :: i, j
    type(array_in_tape) :: arr_a, arr_b
    arr_a = array_in_tape(callstack % stack, a)
    arr_b = array_in_tape(callstack % stack, b)
    a_val = val(a)
    b_val = val(b)
    s_val = matmul(a_val, b_val)
    do j = 1, size(b, 2)
       do i = 1, size(a, 1)
          s(i, j) = var(s_val(i, j), [i, j, arr_a%loc, arr_b%loc])
          call s(i, j)%set_chain(chain_matmul_vv)
       end do
    end do
  end function mat_mul_vv

  subroutine chain_matmul_vd(this)
    class(vari), intent(in) :: this
    integer(ik) :: info(4)
    type(array_in_tape) :: mat1, mat2
    info = this%operand_index()
    mat1 = array_in_tape(callstack % stack, info(3), 2)
    mat2 = array_in_tape(callstack % stack, info(4), 2)
    call chain_dot_product(this, mat1%row(info(1)), mat2%data_col(info(2)))
  end subroutine chain_matmul_vd

  function mat_mul_vd(a, b) result(s)
    type(var), intent(in) :: a(:, :) 
    real(rk), intent(in) :: b(:, :)
    type(var) :: s(size(a, 1), size(b, 2))
    real(rk) :: a_val(size(a,1), size(a,2))
    real(rk) :: s_val(size(a,1), size(b,2))
    integer(ik) :: i, j
    type(array_in_tape) :: arr_a, arr_b
    arr_a = array_in_tape(callstack % stack, a)
    arr_b = array_in_tape(callstack % stack, b)
    a_val = val(a)
    s_val = matmul(a_val, b)
    do j = 1, size(b, 2)
       do i = 1, size(a, 1)
          s(i, j) = var(s_val(i, j), [i, j, arr_a%loc, arr_b%loc])
          call s(i, j)%set_chain(chain_matmul_vd)
       end do
    end do
  end function mat_mul_vd

  subroutine chain_matmul_dv(this)
    class(vari), intent(in) :: this
    integer(ik) :: info(4)
    type(array_in_tape) :: mat1, mat2
    info = this%operand_index()
    mat1 = array_in_tape(callstack % stack, info(3), 2)
    mat2 = array_in_tape(callstack % stack, info(4), 2)
    call chain_dot_product(this, mat1%data_row(info(1)), mat2%col(info(2)))
  end subroutine chain_matmul_dv

  function mat_mul_dv(a, b) result(s)
    real(rk), intent(in) :: a(:, :)
    type(var), intent(in) :: b(:, :) 
    type(var) :: s(size(a, 1), size(b, 2))
    real(rk) :: b_val(size(b,1), size(b,2))
    real(rk) :: s_val(size(a,1), size(b,2))
    integer(ik) :: i, j
    type(array_in_tape) :: arr_a, arr_b
    arr_a = array_in_tape(callstack % stack, a)
    arr_b = array_in_tape(callstack % stack, b)
    b_val = val(b)
    s_val = matmul(a, b_val)
    do j = 1, size(b, 2)
       do i = 1, size(a, 1)
          s(i, j) = var(s_val(i, j), [i, j, arr_a%loc, arr_b%loc])
          call s(i, j)%set_chain(chain_matmul_dv)
       end do
    end do
  end function mat_mul_dv
  
end module fazang_matmul_mod
