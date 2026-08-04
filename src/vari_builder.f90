module fz_vari_builder
  use, intrinsic :: iso_fortran_env
  use, intrinsic :: iso_c_binding
  use fz_env, only : ik, rk, iksize, rksize, adstack, core_adstack, pop_array
  use fz_vari, only: vari, assignment(=), vi_val_at
  use fz_var, only : var, visize, val, adj
  implicit none

  abstract interface
     real(rk) function val_op_no_args(x)
       use fz_env
       implicit none
       real(rk), intent(in) :: x(:)
     end function val_op_no_args

     real(rk) function val_op_r_args(x, arg)
       use fz_env
       implicit none
       real(rk), intent(in) :: x(:), arg(:)
     end function val_op_r_args

     real(rk) function val_op_r_i_args(x, arg, iarg)
       use fz_env
       implicit none
       real(rk), intent(in) :: x(:), arg(:)
       integer(ik), intent(in) :: iarg(:)
     end function val_op_r_i_args

     real(rk) function val_op_i_args(x, iarg)
       use fz_env
       implicit none
       real(rk), intent(in) :: x(:)
       integer(ik), intent(in) :: iarg(:)
     end function val_op_i_args

     function jac_op_no_args(x) result(res)
       use fz_env
       implicit none
       real(rk), intent(in) :: x(:)
       real(rk) :: res(size(x))
     end function jac_op_no_args

     function jac_op_r_args(x, arg) result(res)
       use fz_env
       implicit none
       real(rk), intent(in) :: x(:), arg(:)
       real(rk) :: res(size(x))
     end function jac_op_r_args

     function jac_op_r_i_args(x, arg, iarg) result(res)
       use fz_env
       implicit none
       real(rk), intent(in) :: x(:), arg(:)
       integer(ik), intent(in) :: iarg(:)
       real(rk) :: res(size(x))
     end function jac_op_r_i_args

     function jac_op_i_args(x, iarg) result(res)
       use fz_env
       implicit none
       real(rk), intent(in) :: x(:)
       integer(ik), intent(in) :: iarg(:)
       real(rk) :: res(size(x))
     end function jac_op_i_args

  end interface

  type :: vari_builder
     integer(ik) :: nv = 0          ! nb. of depedent vari's
     integer(ik) :: nr = 0          ! nb. of real args
     integer(ik) :: ni = 0          ! nb. of int args
     integer(ik), pointer :: pv(:) => null() ! array for vari id's
     real(rk), pointer :: pr(:) => null()    ! array for real args
     integer(ik), pointer :: pj(:) => null() ! array for int args
   contains
     procedure, private, nopass :: attach_vari_no_args
     procedure, private, nopass :: attach_vari_r_args
     procedure, private, nopass :: attach_vari_i_args
     procedure, private, nopass :: attach_vari_r_i_args
     generic :: attach => attach_vari_no_args, attach_vari_r_args, attach_vari_i_args, attach_vari_r_i_args
  end type vari_builder

  interface vari_builder
     module procedure recover_builder_from_vari
     module procedure recover_builder_from_i
  end interface vari_builder

contains

  subroutine attach_vari_no_args(v)
    implicit none
    type(var), intent(in) :: v(:)
    integer(ik) :: i
    call core_adstack%push(size(v))
    call core_adstack%push(0_ik)
    call core_adstack%push(0_ik)
    do i = 1, size(v)
       call core_adstack%push(v(i)%p%i)
    end do
  end subroutine attach_vari_no_args

  subroutine attach_vari_r_args(v, r)
    implicit none
    type(var), intent(in) :: v(:)
    real(rk), intent(in) :: r(:)
    integer(ik) :: i
    call core_adstack%push(size(v))
    call core_adstack%push(size(r))
    call core_adstack%push(0_ik)
    do i = 1, size(v)
       call core_adstack%push(v(i)%p%i)
    end do
    do i = 1, size(r)
       call core_adstack%push(r(i))
    end do
  end subroutine attach_vari_r_args

  subroutine attach_vari_i_args(v, ia)
    implicit none
    type(var), intent(in) :: v(:)
    integer(ik), intent(in) :: ia(:)
    integer(ik) :: i
    call core_adstack%push(size(v))
    call core_adstack%push(0_ik)
    call core_adstack%push(size(ia))
    do i = 1, size(v)
       call core_adstack%push(v(i)%p%i)
    end do
    do i = 1, size(ia)
       call core_adstack%push(ia(i))
    end do
  end subroutine attach_vari_i_args

  subroutine attach_vari_r_i_args(v, r, ia)
    implicit none
    type(var), intent(in) :: v(:)
    real(rk), intent(in) :: r(:)
    integer(ik), intent(in) :: ia(:)
    integer(ik) :: i
    call core_adstack%push(size(v))
    call core_adstack%push(size(r))
    call core_adstack%push(size(ia))
    do i = 1, size(v)
       call core_adstack%push(v(i)%p%i)
    end do
    do i = 1, size(r)
       call core_adstack%push(r(i))
    end do
    do i = 1, size(ia)
       call core_adstack%push(ia(i))
    end do
  end subroutine attach_vari_r_i_args

  function recover_builder_from_i(i) result(this)
    implicit none
    type(vari_builder) :: this
    integer(kind=ik), intent(in) :: i
    integer(kind=ik) :: j
    j = i + visize; this%nv = core_adstack%pop_int(j)
    j = j + iksize; this%nr = core_adstack%pop_int(j)
    j = j + iksize; this%ni = core_adstack%pop_int(j)
    j = j + iksize
    call pop_array(this%pv, this%nv, j)
    j = j + this%nv * iksize
    if (this%nr > 0) then
       call pop_array(this%pr, this%nr, j)
       j = j + this%nr * rksize
    endif
    if (this%ni > 0) then
       call pop_array(this%pj, this%ni, j)
       j = j + this%ni * iksize
    endif
  end function recover_builder_from_i

  function recover_builder_from_vari(vi) result(this)
    implicit none
    type(vari_builder) :: this
    type(vari), intent(in) :: vi
    this = recover_builder_from_i(vi%i)
  end function recover_builder_from_vari

  subroutine set_val_no_args(val_proc, x, v)
    implicit none
    type(var), intent(out) :: v
    type(var), intent(in) :: x(:)
    procedure(val_op_no_args), pointer, intent(in) :: val_proc
    type(vari_builder) :: builder
    v%p = val_proc(val(x))
    call builder%attach(x)
  end subroutine set_val_no_args

subroutine set_adj_jac_no_args(jac, this_adj, vi_id, nv)
  use fz_vari, only: recover_vari => recover
  implicit none
  procedure(jac_op_no_args), pointer, intent(in) :: jac
  type(vari), pointer :: va
  integer(ik), intent(in) :: nv
  integer(ik), intent(in) :: vi_id(nv)
  real(rk), intent(in) :: this_adj
  integer(ik) :: i
  real(rk) :: dydx(nv)
  dydx = jac(vi_val_at(vi_id))
  do i = 1, nv
     call recover_vari(va, vi_id(i))
     va%adj_ = va%adj_ + this_adj * dydx(i)
  end do
end subroutine set_adj_jac_no_args

end module fz_vari_builder
