module tape_mod
  use iso_fortran_env
  use iso_c_binding
  use env_mod  
  implicit none
  
  private
  public :: tape, min_rec_size

  ! (val, adj, n_operands=0, n_data_operands=0)
  integer(ik), parameter :: min_rec_size = 6

  ! essential a csr val_adj for sparse arrays
  ! each record for vari is laid out as
  ! (val, adj, n_operands, operands..., n_data_operandsdata_operands...)
  type :: tape
     integer(ik) :: head = 1
     integer(ik) :: storage(min_rec_size * adstack_len) = 0 !crude size est
     ! type(c_ptr) :: cp
     ! real(rk), pointer :: fp
   contains
     generic :: push => push_1, push_n, push_val, push_val_op,&
          & push_val_op_dop, push_val_dop
     generic :: set_val => set_val_curr, set_val_at
     generic :: set_adj => set_adj_curr, set_adj_at
     generic :: val => val_curr, val_at
     generic :: adj => adj_curr, adj_at
     procedure :: n_operand, operand_val, operand_adj, operand_index, data_operand
     procedure :: n_data_operand
     procedure :: record_len
     generic :: cast_from_real => set_real_at, set_real_array_at
     generic :: cast_to_real => get_real_at, get_real_array_at
     procedure, private :: set_val_curr, set_val_at
     procedure, private :: set_adj_curr, set_adj_at
     procedure, private :: val_curr, val_at
     procedure, private :: adj_curr, adj_at
     procedure, private :: push_1, push_n, push_val
     procedure, private :: push_val_op, push_val_op_dop, push_val_dop
     procedure :: set_real_at, set_real_array_at
     procedure :: get_real_at, get_real_array_at
  end type tape
  
contains

  function push_1(this) result(id)
    class(tape), intent(inout) :: this
    integer(ik) :: id
    id = this%head
    call incr(this%head, min_rec_size)
  end function push_1

  function push_n(this, n) result(id)
    integer(ik), intent(in) :: n
    class(tape), intent(inout) :: this
    integer(ik) :: id
    id = this%head
    call incr(this%head, min_rec_size * n)
  end function push_n

  subroutine set_val_at(this, i, d)
    real(rk), intent(in) :: d
    class(tape), target, intent(inout) :: this
    integer(ik), intent(in) :: i
    call this%set_real_at(i, d)
  end subroutine set_val_at

  subroutine set_val_curr(this, d)
    real(rk), intent(in) :: d
    class(tape), intent(inout) :: this
    call this%set_val_at(this%head, d)
  end subroutine set_val_curr

  function push_val(this, d) result(id)
    real(rk), intent(in) :: d
    class(tape), intent(inout) :: this
    integer(ik) :: id
    id = this%head
    call this%set_val_curr(d)
    call incr(this%head, min_rec_size)
  end function push_val

  subroutine set_adj_at(this, i, d)
    real(rk), intent(in) :: d
    class(tape), target, intent(inout) :: this
    integer(ik), intent(in) :: i
    call this%set_real_at(i + 2, d)
  end subroutine set_adj_at

  subroutine set_adj_curr(this, d)
    real(rk), intent(in) :: d
    class(tape), intent(inout) :: this
    call this%set_adj_at(this%head, d)
  end subroutine set_adj_curr

  pure function val_at(this, i) result(d)
    class(tape), target, intent(in) :: this
    integer(ik), intent(in) :: i
    real(rk) :: d
    d = this%get_real_at(i)
  end function val_at

  pure function adj_at(this, i) result(d)
    integer(ik), intent(in) :: i
    real(rk) :: d
    class(tape), target, intent(in) :: this
    d = this%get_real_at(i + 2)
  end function adj_at
  
  pure function val_curr (this) result(d)
    real(rk) :: d
    class(tape), target, intent(in) :: this
    d = this%val_at(this%head)
  end function val_curr

  pure function adj_curr (this) result(d)
    real(rk) :: d
    class(tape), target, intent(in) :: this
    d = this%val_at(this%head)
  end function adj_curr

  ! op: operands index
  function push_val_op(this, val, op) result(id)
    real(rk), intent(in) :: val
    integer(ik), intent(in) :: op(:)
    class(tape), intent(inout) :: this
    integer(ik) :: id
    id = this%head
    call this%set_val_curr(val)
    call incr4(this%head)
    this%storage(this%head) = size(op)
    call incr1(this%head)
    this%storage(this%head : (this%head + size(op) -1)) = op
    call incr(this%head, size(op))
    call incr1(this%head)       ! dop = 0
  end function push_val_op

  function push_val_dop(this, val, dop) result(id)
    real(rk), intent(in) :: val
    real(rk), intent(in) :: dop(:)
    class(tape), target, intent(inout) :: this
    integer(ik) :: id
    id = this%head
    call this%set_val_curr(val)
    call incr4(this%head)
    call incr1(this%head)
    this%storage(this%head) = size(dop)
    call incr1(this%head)
    call this%set_real_array_at(this%head, dop)
    call incr(this%head, 2 * size(dop)) ! real64 to int32
  end function push_val_dop

  ! op: operands index
  ! dop: data operands value
  function push_val_op_dop(this, val, op, dop) result(id)
    real(rk), intent(in) :: val
    real(rk), intent(in) :: dop(:)
    integer(ik), intent(in) :: op(:)
    class(tape), target, intent(inout) :: this
    integer(ik) :: id
    id = this%head
    call this%set_val_curr(val)
    call incr4(this%head)
    this%storage(this%head) = size(op)
    call incr1(this%head)
    this%storage(this%head : (this%head + size(op) -1)) = op
    call incr(this%head, size(op))
    this%storage(this%head) = size(dop)
    call incr1(this%head)
    call this%set_real_array_at(this%head, dop)
    call incr(this%head, 2 * size(dop)) ! real64 to int32
  end function push_val_op_dop

  pure function n_operand(this, i) result(n)
    class(tape), target, intent(in) :: this
    integer(ik), intent(in) :: i
    integer(ik) :: n
    n = this%storage(i + 4)
  end function n_operand

  pure function operand_val(this, i) result(d)
    class(tape), target, intent(in) :: this
    integer(ik), intent(in) :: i
    integer(ik) :: op(this%storage(i + 4)), nop, j
    real(rk) :: d(this%storage(i + 4))
    nop = this%storage(i + 4)
    op = this%storage((i + 5) : (i + 4 + nop))
    do j = 1, nop
       d(j) = this%val_at(op(j))
    end do
  end function operand_val

  pure function operand_adj(this, i) result(d)
    class(tape), target, intent(in) :: this
    integer(ik), intent(in) :: i
    integer(ik) :: op(this%storage(i + 4)), nop, j
    real(rk) :: d(this%storage(i + 4))
    nop = this%storage(i + 4)
    op = this%storage((i + 5) : (i + 4 + nop))
    do j = 1, nop
       d(j) = this%adj_at(op(j))
    end do
  end function operand_adj

  pure function operand_index(this, i) result(op)
    class(tape), target, intent(in) :: this
    integer(ik), intent(in) :: i
    integer(ik) :: op(this%storage(i + 4)), nop
    nop = this%storage(i + 4)
    op = this%storage((i + 5) : (i + 4 + nop))
  end function operand_index

  pure function record_len(this, i) result(j)
    class(tape), target, intent(in) :: this
    integer(ik), intent(in) :: i
    integer(ik) :: j
    j = min_rec_size + this%storage(i + 4) + 2 * this%storage(i + 5)
  end function record_len

  pure function n_data_operand(this, i) result(d)
    class(tape), target, intent(in) :: this
    integer(ik), intent(in) :: i
    integer(ik) :: d
    d = this%storage(i + 5 + this%storage(i + 4))
  end function n_data_operand

  pure function data_operand(this, i) result(d)
    class(tape), target, intent(in) :: this
    integer(ik), intent(in) :: i
    integer(ik) :: nop
    real(rk) :: d(this%storage(i + 5 + this%storage(i + 4)))
    nop = this%storage(i + 5 + this%storage(i + 4))
    d = this%get_real_array_at(i + min_rec_size + this%storage(i + 4), nop)
  end function data_operand

  subroutine set_real_at(this, i, d)
    class(tape), target, intent(inout) :: this
    integer(ik), intent(in) :: i
    real(rk), intent(in) :: d
    this%storage(i:(i+1)) = transfer(d, this%storage)

    ! an alternative is to use c pointer
    ! this%cp = c_loc(this%storage(i))
    ! call c_f_pointer(this%cp, this%fp)
    ! this%fp = d
  end subroutine set_real_at

  subroutine set_real_array_at(this, i, d)
    class(tape), target, intent(inout) :: this
    integer(ik), intent(in) :: i
    real(rk), intent(in) :: d(:)
    ! real(rk), pointer :: fp(:)
    integer(ik) :: n
    n = size(d)
    this%storage(i:(i+2*n-1)) = transfer(d, this%storage)

    ! an alternative is to use c pointer
    ! this%cp = c_loc(this%storage(i))
    ! call c_f_pointer(this%cp, fp, [1])
    ! fp(1:size(d)) = d
  end subroutine set_real_array_at

  pure function get_real_at(this, i) result(d)
    class(tape), target, intent(in) :: this
    integer(ik), intent(in) :: i
    real(rk) :: d
    d = transfer(this%storage(i:(i+1)), d)
  end function get_real_at

  pure function get_real_array_at(this, i, n) result(d)
    class(tape), target, intent(in) :: this
    integer(ik), intent(in) :: i, n
    real(rk) :: d(n)
    d = transfer(this%storage(i:(i+2*n-1)), d)
  end function get_real_array_at

  ! subroutine check(this)
  !   class(tape), intent(inout) :: this
  !   if ( this%head >= size(this%storage) ) then
  !      write(*, *) "adstack overflow"
  !      stop 99
  !   end if
  ! end subroutine check

end module tape_mod
