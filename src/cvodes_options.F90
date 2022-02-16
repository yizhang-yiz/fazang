module fazang_cvodes_options_mod
  use, intrinsic :: iso_c_binding

  use fcvodes_mod                   ! Fortran interface to CVODES
  use fsundials_context_mod         ! Fortran interface to SUNContext
  use fsundials_nvector_mod         ! Fortran interface to generic N_Vector
  use fnvector_serial_mod           ! Fortran interface to serial N_Vector
  use fsundials_nonlinearsolver_mod ! Fortran interface to generic SUNNonlinearSolver
  use fsundials_linearsolver_mod ! Fortran interface to generic SUNLinearSolver
  use fsunnonlinsol_newton_mod      ! Fortran interface to Newton SUNNonlinearSolver
  use fsunmatrix_dense_mod       ! Fortran interface to dense SUNMatrix
  use fsunlinsol_dense_mod       ! Fortran interface to dense SUNLinearSolver
  use fsundials_matrix_mod       ! Fortran interface to generic SUNMatrix

  implicit none

  type, abstract :: cvodes_options
     integer :: cv_method = -1
   contains
     procedure(set_cvodes), deferred :: set
  end type cvodes_options

  abstract interface
     subroutine set_cvodes(this, mem)
       import c_ptr, cvodes_options
       class(cvodes_options), intent(in) :: this
       type(c_ptr), intent(inout) :: mem ! CVODES memory
     end subroutine set_cvodes
  end interface

  type, extends(cvodes_options) :: cvodes_tol
     real(c_double) :: rtol, atol
     integer(c_long) :: max_nstep
  contains
    procedure :: set
 end type cvodes_tol

contains

  subroutine set(this, mem)
    class(cvodes_tol), intent(in) :: this
    type(c_ptr), intent(inout) :: mem ! CVODES memory
    integer :: ierr
    ierr = FCVodeSStolerances(mem, this % rtol, this % atol)
    ierr = FCVodeSetMaxNumSteps(mem, this % max_nstep)
  end subroutine set

end module fazang_cvodes_options_mod
