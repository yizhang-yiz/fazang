module fazang_value_of_mod
  use fazang_env_mod
  use fazang_var_mod
  implicit none

  private
  public :: value_of

  ! interface value_of
  !    module procedure :: value_of_v
  !    module procedure :: value_of_d
  ! end interface value_of

contains
  elemental real(rk) function value_of(v)
    class(*), intent(in) :: v
    select type (v)
    type is (var)
       value_of = v%val()
    type is (real(rk))
       value_of = v
    end select
  end function value_of

  ! elemental real(rk) function value_of_v(v)
  !   type(var), intent(in) :: v
  !   value_of_v = v%val()
  ! end function value_of_v

  ! elemental real(rk) function value_of_d(d)
  !   real(rk), intent(in) :: d
  !   value_of_d = d
  ! end function value_of_d

end module fazang_value_of_mod
  
