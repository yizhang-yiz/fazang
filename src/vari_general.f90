! new vari based on pre-computed value and jacobian
! one use case is ODE solution with forward sensi
! technically most vari can be impl this way, tho more storage required.
module fz_vari_precomputed_adj
  use fz_env
  use fz_vari
  implicit none
  type, extends(chain_base) :: vi_chain
   contains
     procedure, nopass :: chain => chain_impl
  end type vi_chain
  type( vi_chain ), target :: vi_chain_instance
contains
  subroutine chain_impl(ip)
    implicit none
    integer(ik), intent(in) :: ip
    type(vari), pointer :: this, a
    real(rk), pointer :: q, data(:)
    integer(ik), pointer :: vec(:), idata(:)
    integer(ik) :: i
    type(c_ptr) :: cp
    call recover_general_vari(ip, this, vec, data, idata)
    do i = 1, size(vec)
       cp = c_loc(core_adstack%s_(vec(i))); call c_f_pointer(cp, a)
       a%adj_ = a%adj_ + this%adj_ * data(i)
    end do
  end subroutine chain_impl

  function new_vi (val, vec, adj) result(iout)
    implicit none
    integer(ik) :: iout
    real(rk), target :: val
    integer(ik), target, intent(in) :: vec(:)
    real(rk), target, intent(in) :: adj(:)
    integer(ik) :: idata(0)
    call new_general_vari(iout, val, vec, adj(1:size(vec)), idata)
    chains(iout)%c => vi_chain_instance
  end function new_vi

end module fz_vari_precomputed_adj
