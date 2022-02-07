module fazang_array_in_tape_mod
  use fazang_tape_mod
  use fazang_vari_mod, only : vari, vari_index
  use fazang_var_mod, only : var, vi_index
  use fazang_env_mod
  implicit none

  type array_in_tape
     type(tape), pointer :: tape_ptr
     integer(ik) :: loc = 0         ! starting index in tape
     integer(ik) :: rank
   contains
     procedure :: nrow, ncol, row, col, data_row, data_col
     ! generic :: push => push_1d, push_2d
  end type array_in_tape

  interface array_in_tape
     module procedure :: var_1d_array_in_tape
     module procedure :: var_2d_array_in_tape
     module procedure :: vari_array_in_tape
     module procedure :: data_1d_array_in_tape
     module procedure :: data_2d_array_in_tape
  end interface array_in_tape

contains

  ! we can't use assumed-rank to simplify impl due to gfortran bug
  ! push to tape through sideffect
  function var_1d_array_in_tape(t, a) result(s)
    type(var), intent(in) :: a(:)
    type(tape), target, intent(in) :: t
    type(array_in_tape) :: s
    s%tape_ptr => t
    s%rank = 1
    s%loc = s%tape_ptr%push([size(a), vi_index(a)])
  end function var_1d_array_in_tape

  ! push to tape through sideffect
  function var_2d_array_in_tape(t, a) result(s)
    type(var), intent(in) :: a(:, :)
    type(tape), target, intent(in) :: t
    type(array_in_tape) :: s
    s%tape_ptr => t
    s%rank = 2
    s%loc = s%tape_ptr%push([shape(a), vi_index(a)])
  end function var_2d_array_in_tape

  ! push to tape through sideffect
  function data_1d_array_in_tape(t, a) result(s)
    real(rk), intent(in) :: a(:)
    type(tape), target, intent(in) :: t
    type(array_in_tape) :: s
    integer(ik) :: i
    s%tape_ptr => t
    s%rank = 1
    s%loc = s%tape_ptr%push(size(a))
    i = s%tape_ptr%push(a)
  end function data_1d_array_in_tape

  ! push to tape through sideffect
  function data_2d_array_in_tape(t, a) result(s)
    real(rk), intent(in) :: a(:, :)
    type(tape), target, intent(in) :: t
    type(array_in_tape) :: s
    integer(ik) :: i
    s%tape_ptr => t
    s%rank = 2
    s%loc = s%tape_ptr%push(shape(a))
    i = s%tape_ptr%push(a)
  end function data_2d_array_in_tape

  function vari_array_in_tape(t, i, irank) result(s)
    integer(ik), intent(in) :: i, irank
    type(tape), target, intent(in) :: t
    type(array_in_tape) :: s
    s%tape_ptr => t
    s%rank = irank
    s%loc = i
  end function vari_array_in_tape

  pure integer(ik) function nrow(this)
    class(array_in_tape), intent(in) :: this    
    nrow = this%tape_ptr%storage(this%loc)
  end function nrow

  pure integer(ik) function ncol(this)
    class(array_in_tape), intent(in) :: this    
    ncol = this%tape_ptr%storage(this%loc + 1)
  end function ncol

  pure function row(this, irow) result(id)
    class(array_in_tape), intent(in) :: this
    integer(ik), intent(in) :: irow
    integer(ik) :: id(this%ncol())
    id = this % tape_ptr % storage((this%loc + 1 + irow) : &
         & (this%loc + 1 + this%nrow() * this%ncol()) : this%nrow())
  end function row

  pure function data_row(this, irow) result(d)
    class(array_in_tape), intent(in) :: this
    integer(ik), intent(in) :: irow
    real(rk) :: d(this%ncol())
    integer(ik) :: j
    do j = 1, this % ncol()
       d(j) = this % tape_ptr % get_real_at(this%loc + 2 + &
            & 2*(j-1)* this%nrow() + 2 * (irow-1))
    end do
  end function data_row

  pure function col(this, icol) result(id)
    class(array_in_tape), intent(in) :: this
    integer(ik), intent(in) :: icol
    integer(ik) :: id(this%nrow())
    id = this % tape_ptr % storage((this%loc + 1 + (icol-1) * &
         & this%nrow() + 1) : (this%loc + 1 + icol * this%nrow()))
  end function col

  pure function data_col(this, icol) result(d)
    class(array_in_tape), intent(in) :: this
    integer(ik), intent(in) :: icol
    real(rk) :: d(this%nrow())
    d = this%tape_ptr%get_real_array_at(this%loc + 2 &
         & + (icol-1)*this%nrow()*2, this%nrow())
  end function data_col

end module fazang_array_in_tape_mod
