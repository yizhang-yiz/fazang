module fazang_cvodes_mod
  use, intrinsic :: iso_c_binding

  use fazang_cvodes_model_mod
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

  type :: cvodes_service
     ! local variables
     integer(c_long) :: neq         
     type(N_Vector), pointer  :: sunvec_y  ! solution N_Vector
     integer(c_int) :: ierr        ! error flag from C functions
     type(c_ptr)    :: mem      ! CVODES memory
     type(c_ptr)    :: ctx         ! SUNDIALS simulation context

     type(SUNMatrix),       pointer :: sunmat_A      ! sundials matrix
     type(SUNLinearSolver), pointer :: sunlinsol_LS  ! sundials linear solver

   contains
     final :: free_cvodes_service
  end type cvodes_service

  interface cvodes_service
     module procedure :: new_cvodes_service
  end interface cvodes_service

contains
  function new_cvodes_service(t0, y, ode) result(serv)
    type(cvodes_service) :: serv
    integer :: ierr
    real(c_double) :: t0
    real(c_double), intent(inout) :: y(:)
    type(cvs_rhs), target, intent(in) :: ode

    ierr = FSUNContext_Create(c_null_ptr, serv%ctx)

    ! create SUNDIALS N_Vector
    serv % neq = size(y)
    serv % sunvec_y => FN_VMake_Serial(serv % neq, y, serv % ctx)
    if (.not. associated(serv % sunvec_y)) then
       print *, 'ERROR: sunvec = NULL'
       stop 1
    end if

    serv % sunmat_A => FSUNDenseMatrix(serv % neq, &
         serv % neq, serv % ctx)

    serv % sunlinsol_LS => FSUNLinSol_Dense(serv % sunvec_y,&
         serv % sunmat_A, serv % ctx)

    ! create CVode memory
    serv % mem = FCVodeCreate(CV_BDF, serv % ctx)
    if (.not. c_associated(serv % mem)) then
       print *, 'ERROR: mem = NULL'
       stop 1
    end if

    ierr = FCVodeInit(serv % mem, c_funloc(ode % cvs_f), t0, serv % sunvec_y)
    if (ierr /= 0) then
       print *, 'Error in FCVodeInit, ierr = ', ierr, '; halting'
       stop 1
    endif

    ierr = FCVodeSetLinearSolver(serv % mem, serv % sunlinsol_LS, &
         serv % sunmat_A)

    ierr = FCVodeSetUserData(serv % mem, c_loc(ode))
  end function new_cvodes_service

  subroutine free_cvodes_service(this)
    type(cvodes_service) :: this
    integer :: ierr
    call FCVodeFree(this % mem)
    ! ierr = FSUNNonLinSolFree(this % sunnls)
    ierr = FSUNLinSolFree(this % sunlinsol_LS)
    call FSUNMatDestroy(this % sunmat_A)
    call FN_VDestroy(this % sunvec_y)
    ierr = FSUNContext_Free(this % ctx)
  end subroutine free_cvodes_service

  function cvodes_bdf_data(t, y, ts, rhs, rtol, atol, max_nstep) result(yt)
    procedure(cvs_rhs_func) :: rhs
    real(c_double), intent(in) :: ts(:), rtol, atol, t
    real(c_double), intent(inout) :: y(:)
    real(c_double) :: yt(size(y), size(ts))
    integer(c_long), intent(in) :: max_nstep
    real(c_double) :: tcur(1)
    type(cvs_rhs), target :: ode
    type(cvodes_service) :: serv
    integer :: ierr, outstep, nout

    ode = cvs_rhs(rhs)
    serv = new_cvodes_service(t, y, ode)

    ierr = FCVodeSStolerances(serv % mem, rtol, atol)
    ierr = FCVodeSetMaxNumSteps(serv % mem, max_nstep)

    nout = size(ts)
    tcur = t
    do outstep = 1, nout
       ierr = FCVode(serv % mem, ts(outstep), serv % sunvec_y, tcur, CV_NORMAL)
       if (ierr /= 0) then
          print *, 'Error in FCVODE, ierr = ', ierr, '; halting'
          stop 1
       endif
       yt(:, outstep) = y
    enddo
  end function cvodes_bdf_data

end module fazang_cvodes_mod
