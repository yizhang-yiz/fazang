module fazang_cvodes_mod
  use, intrinsic :: iso_c_binding

  use fazang_cvodes_model_mod
  use fazang_cvodes_options_mod
  use fazang_eager_adj_mod

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

     ! sens
     integer(ik) :: sens = 0
     type(c_ptr) :: yS

   contains
     final :: free_cvodes_service
  end type cvodes_service

  interface cvodes_service
     module procedure :: new_cvodes_service
  end interface cvodes_service

contains
  function new_cvodes_service(t0, y, ode) result(serv)
    implicit none
    
    type(cvodes_service) :: serv
    integer :: ierr, is
    real(c_double) :: t0
    real(c_double), intent(inout) :: y(:)
    type(cvs_rhs), target, intent(in) :: ode
    type(N_Vector), pointer :: yiS
    real(c_double), pointer :: yiSvec(:)

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

    ierr = FCVodeSetUserData(serv % mem, c_loc(ode))

    ierr = FCVodeSetLinearSolver(serv % mem, serv % sunlinsol_LS, &
         serv % sunmat_A)

    ! need sensitivity ?
    if ( ode % ns > 0 ) then
       serv % sens = ode % ns
       serv % yS = FN_VCloneVectorArray(ode % ns, serv % sunvec_y) 
       do is = 0, ode % ns - 1
          yiS => FN_VGetVecAtIndexVectorArray(serv % yS, is)
          yiSvec => FN_VGetArrayPointer(yiS)
          call FN_VConst(0.d0, yiS)
          if (ode % ys .and. is < serv % neq) then
             yiSvec(is + 1) = 1.0;
          endif
       end do
       ierr = FCVodeSensInit(serv % mem, ode % ns, CV_STAGGERED, &
            c_funloc(ode % cvs_sens), serv % yS)
    end if

  end function new_cvodes_service

  subroutine free_cvodes_service(this)
    implicit none

    type(cvodes_service) :: this
    integer :: ierr
    call FCVodeFree(this % mem)
    ! ierr = FSUNNonLinSolFree(this % sunnls)
    ierr = FSUNLinSolFree(this % sunlinsol_LS)
    call FSUNMatDestroy(this % sunmat_A)
    call FN_VDestroy(this % sunvec_y)
    if (this % sens > 0) then
       call FN_VDestroyVectorArray(this % yS, this % sens)
    endif
    ierr = FSUNContext_Free(this % ctx)
  end subroutine free_cvodes_service

  function cvodes_bdf_data(t, y, ts, rhs, cvs_options) result(yt)
    implicit none

    procedure(cvs_rhs_func) :: rhs
    real(c_double), intent(in) :: ts(:), t
    class(cvodes_options), intent(in) :: cvs_options
    real(c_double), intent(inout) :: y(:)
    real(c_double) :: yt(size(y), size(ts))
    real(c_double) :: tcur(1)
    type(cvs_rhs), target :: ode
    type(cvodes_service) :: serv
    integer :: ierr, outstep, nout

    ode = cvs_rhs(rhs)
    serv = new_cvodes_service(t, y, ode)
    call cvs_options % set(serv % mem)

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

  function cvodes_bdf_y0_sens(t, y, ts, rhs, rhs_yvar, cvs_options) result(yt)
    implicit none

    procedure(cvs_rhs_func) :: rhs
    procedure(cvs_rhs_func_yvar) :: rhs_yvar
    real(c_double), intent(in) :: ts(:), t
    class(cvodes_options), intent(in) :: cvs_options
    type(var), intent(in) :: y(:)
    type(var) :: yt(size(y), size(ts))

    ! local variables
    real(c_double) :: tcur(1)
    type(cvs_rhs), target :: ode
    type(cvodes_service) :: serv
    integer :: ierr, outstep, nout, is
    integer(c_long) :: i
    real(rk) :: yval(size(y)), g(size(y))
    type(N_Vector), pointer :: yiS
    real(c_double), pointer :: yiSvec(:)

    integer(c_int), parameter :: err_con = 1

    ode = cvs_rhs(.true., size(y), rhs, rhs_yvar)
    yval = y%val()
    serv = new_cvodes_service(t, yval, ode)

    ierr = FCVodeSensEEtolerances(serv % mem)       

    call cvs_options % set(serv % mem)

    nout = size(ts)
    tcur = t
    do outstep = 1, nout
       ierr = FCVode(serv % mem, ts(outstep), serv % sunvec_y, tcur, CV_NORMAL)
       if (ierr /= 0) then
          print *, 'Error in FCVODE, ierr = ', ierr, '; halting'
          stop 1
       endif
       ierr = FCVodeGetSens(serv % mem, tcur, serv % yS)
       do i = 1, serv % neq
          g = 0.d0
          do is = 0, ode % ns - 1
             yiS => FN_VGetVecAtIndexVectorArray(serv % yS, is)
             yiSvec => FN_VGetArrayPointer(yiS)
             g(is + 1) = yiSvec(i)
          end do
          yiSvec => FN_VGetArrayPointer(serv % sunvec_y)
          yt(i, outstep) = var(yiSvec(i), y, g)
          call yt(i, outstep)%set_chain(chain_eager_adj)
       enddo
    enddo
  end function cvodes_bdf_y0_sens

end module fazang_cvodes_mod
