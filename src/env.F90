module fazang_env_mod
  use, intrinsic :: iso_fortran_env

  implicit none

#ifdef FZ_STACK_LEN
  integer, parameter :: adstack_len = FZ_STACK_LEN
#elif defined FZ_USE_STACK
  integer, parameter :: adstack_len = 1024
#elif defined FZ_INIT_STACK_LEN
  integer, parameter :: init_vari_stack_size = FZ_INIT_STACK_LEN
#else
  integer, parameter :: init_vari_stack_size = 1024
#endif

  ! each tape record = (val, adj, n_operands=0, n_data_operands=0)
  integer, parameter :: min_rec_size = 6

  integer, parameter :: init_tape_size = 2 * min_rec_size * init_vari_stack_size

  ! real KIND
  integer, parameter :: rk = real64

  ! int KIND
  integer, parameter :: ik = int32

contains
  elemental subroutine incr(var, inc)
    integer,intent(inout) :: var
    integer,intent(in)    :: inc
    var = var + inc
  end subroutine incr

  elemental subroutine incr1(var)
    integer,intent(inout) :: var
    var = var + 1
  end subroutine incr1

  elemental subroutine incr2(var)
    integer,intent(inout) :: var
    var = var + 2
  end subroutine incr2

  elemental subroutine incr4(var)
    integer,intent(inout) :: var
    var = var + 4
  end subroutine incr4


end module fazang_env_mod
