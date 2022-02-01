module nested_tape_mod
  use tape_mod
  use env_mod
  use vari_mod, only : callstack
  implicit none

  private
  public :: begin_nested, end_nested
  
  integer, parameter :: max_nested_stack = 10
  integer :: nested_head(max_nested_stack) = 0
  integer :: nest_level = 0

contains
  ! we can't use OOP like FINAL because gfortran has implementation
  ! gap of standard 2018
  subroutine begin_nested()
    nest_level = nest_level + 1
    nested_head(nest_level) = callstack % stack % head
  end subroutine begin_nested

  subroutine end_nested()
    integer :: i
    if ( nest_level > 0 ) then
       do i = callstack % stack % head, nested_head(nest_level), -1
          callstack % stack % storage(i) = 0
       end do
       callstack % stack % head = nested_head(nest_level)
       nested_head(nest_level) = 0
       nest_level = nest_level - 1
    end if
  end subroutine end_nested

end module nested_tape_mod
