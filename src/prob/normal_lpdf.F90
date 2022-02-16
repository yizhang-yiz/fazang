module fazang_normal_lpdf_mod
  use fazang_var_mod
  use fazang_env_mod
  use fazang_arith_mod
  use fazang_value_of_mod
  use fazang_eager_adj_mod
  implicit none

  private
  public :: normal_lpdf

  interface normal_lpdf
     module procedure :: normal_lpdf_vv
     module procedure :: normal_lpdf_vd
     module procedure :: normal_lpdf_dv
     module procedure :: normal_lpdf_dd
  end interface normal_lpdf

contains

  function normal_lpdf_vv(y, mu, sigma) result(v)
    real(rk), intent(in) :: y(:)
    type(var), intent(in) :: mu
    type(var), intent(in) :: sigma
    type(var) :: v
    v = var_with_partials(sum(logp(y, mu, sigma)), [mu, sigma], &
         [sum(grad_mu(y, mu, sigma)), sum(grad_sigma(y, mu, sigma))])
  end function normal_lpdf_vv
  
  function normal_lpdf_vd(y, mu, sigma) result(v)
    real(rk), intent(in) :: y(:)
    type(var), intent(in) :: mu
    real(rk), intent(in) :: sigma
    type(var) :: v
    v = var_with_partials(sum(logp(y, mu, sigma)), [mu], [sum(grad_mu(y, mu, sigma))])
  end function normal_lpdf_vd

  function normal_lpdf_dv(y, mu, sigma) result(v)
    real(rk), intent(in) :: y(:)
    real(rk), intent(in) :: mu
    type(var), intent(in) :: sigma
    type(var) :: v
    v = var_with_partials(sum(logp(y, mu, sigma)), [sigma], [sum(grad_sigma(y, mu, sigma))])
  end function normal_lpdf_dv

  function normal_lpdf_dd(y, mu, sigma) result(v)
    real(rk), intent(in) :: y(:)
    real(rk), intent(in) :: mu
    real(rk), intent(in) :: sigma
    real(rk) :: v
    v = sum(logp(y, mu, sigma))
  end function normal_lpdf_dd

  elemental function logp(y, mu, sigma) result(lp)
    real(rk), intent(in) :: y
    class(*), intent(in) :: mu
    class(*), intent(in) :: sigma
    real(rk) :: y_scaled, inv_sigma, lp
    select type (mu)
    class default
       select type (sigma)
       class default
          inv_sigma = 1.d0/value_of(sigma)
          y_scaled = (y - value_of(mu)) * inv_sigma
          lp =  - 0.5d0 * y_scaled * y_scaled
          lp = lp - log(value_of(sigma))
          lp = lp - log_sqrt_two_pi
       end select
    end select
  end function logp

  elemental function grad_mu(y, mu, sigma) result(dmu)
    real(rk), intent(in) :: y
    class(*), intent(in) :: mu
    class(*), intent(in) :: sigma
    real(rk) :: y_scaled, inv_sigma, dmu
    select type (mu)
    class default
       select type (sigma)
       class default
          inv_sigma = 1.d0/value_of(sigma)
          y_scaled = (y - value_of(mu)) * inv_sigma
          dmu = y_scaled * inv_sigma
       end select
    end select
  end function grad_mu

  ! elemental function grad_y(y, mu, sigma) result(dy)
  !   real(rk), intent(in) :: y
  !   class(*), intent(in) :: mu
  !   class(*), intent(in) :: sigma
  !   real(rk) :: y_scaled, inv_sigma, dy
  !   select type (mu)
  !   class default
  !      select type (sigma)
  !      class default
  !         inv_sigma = 1.d0/value_of(sigma)
  !         y_scaled = (y - value_of(mu)) * inv_sigma
  !         dy = -y_scaled * inv_sigma
  !      end select
  !   end select
  ! end function grad_y

  elemental function grad_sigma(y, mu, sigma) result(ds)
    real(rk), intent(in) :: y
    class(*), intent(in) :: mu
    class(*), intent(in) :: sigma
    real(rk) :: y_scaled, inv_sigma, ds
    select type (mu)
    class default
       select type (sigma)
       class default
          inv_sigma = 1.d0/value_of(sigma)
          y_scaled = (y - value_of(mu)) * inv_sigma
          ds = inv_sigma * y_scaled * y_scaled - inv_sigma       
       end select
    end select
  end function grad_sigma

end module fazang_normal_lpdf_mod
