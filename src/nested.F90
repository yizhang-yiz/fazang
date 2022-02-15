module fazang_nested_tape_mod
  use fazang_tape_mod
  use fazang_env_mod
  use fazang_vari_mod, only : callstack
  implicit none

  private
  public :: set_zero_nested_adj, begin_nested, end_nested
  
  integer, parameter :: max_nested_stack = 10
  integer :: nested_tape_head(max_nested_stack) = 0
  integer :: nested_vari_head(max_nested_stack) = 0
  integer :: nest_level = 0

contains
  subroutine set_zero_nested_adj ()
    integer i
    do i = callstack % head - 1, nested_vari_head(nest_level), -1
       call callstack % stack % set_adj(callstack%varis(i)%i, 0.0d0)
    end do
  end subroutine set_zero_nested_adj

  ! we can't use OOP like FINAL because gfortran has implementation
  ! gap of standard 2018
  subroutine begin_nested()
    nest_level = nest_level + 1
    nested_vari_head(nest_level) = callstack % head
    nested_tape_head(nest_level) = callstack % stack % head
  end subroutine begin_nested

  subroutine end_nested()
    integer :: i
    if ( nest_level > 0 ) then
       do i = callstack % stack % head, nested_tape_head(nest_level), -1
          callstack % stack % storage(i) = 0
       end do
       callstack % stack % head = nested_tape_head(nest_level)
       callstack % head = nested_vari_head(nest_level)
       nested_tape_head(nest_level) = 0
       nested_vari_head(nest_level) = 0
       nest_level = nest_level - 1
    end if
  end subroutine end_nested

end module fazang_nested_tape_mod
