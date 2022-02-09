module fazang_lognormal_lpdf_mod
  use fazang_var_mod
  use fazang_env_mod
  use fazang_arith_mod
  use fazang_value_of_mod
  use fazang_eager_adj_mod
  implicit none

  private
  public :: lognormal_lpdf

  interface lognormal_lpdf
     module procedure :: lognormal_lpdf_vv
     module procedure :: lognormal_lpdf_vd
     module procedure :: lognormal_lpdf_dv
     module procedure :: lognormal_lpdf_dd
  end interface lognormal_lpdf

contains

  function lognormal_lpdf_vv(y, mu, sigma) result(v)
    real(rk), intent(in) :: y(:)
    type(var), intent(in) :: mu
    type(var), intent(in) :: sigma
    type(var) :: v
    v = var(sum(logp(y, mu, sigma)), [mu, sigma], &
         [sum(grad_mu(y, mu, sigma)), sum(grad_sigma(y, mu, sigma))])
    call v%set_chain(chain_eager_adj)
  end function lognormal_lpdf_vv
  
  function lognormal_lpdf_vd(y, mu, sigma) result(v)
    real(rk), intent(in) :: y(:)
    type(var), intent(in) :: mu
    real(rk), intent(in) :: sigma
    type(var) :: v
    v = var(sum(logp(y, mu, sigma)), [mu], [sum(grad_mu(y, mu, sigma))])
    call v%set_chain(chain_eager_adj)
  end function lognormal_lpdf_vd

  function lognormal_lpdf_dv(y, mu, sigma) result(v)
    real(rk), intent(in) :: y(:)
    real(rk), intent(in) :: mu
    type(var), intent(in) :: sigma
    type(var) :: v
    v = var(sum(logp(y, mu, sigma)), [sigma], [sum(grad_sigma(y, mu, sigma))])
    call v%set_chain(chain_eager_adj)
  end function lognormal_lpdf_dv

  function lognormal_lpdf_dd(y, mu, sigma) result(v)
    real(rk), intent(in) :: y(:)
    real(rk), intent(in) :: mu
    real(rk), intent(in) :: sigma
    real(rk) :: v
    v = sum(logp(y, mu, sigma))
  end function lognormal_lpdf_dd

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
          y_scaled = (log(y) - value_of(mu)) * inv_sigma
          lp =  - 0.5d0 * y_scaled * y_scaled
          lp = lp - log(value_of(sigma))
          lp = lp - log(y)
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
          y_scaled = (log(y) - value_of(mu)) * inv_sigma
          dmu = y_scaled * inv_sigma
       end select
    end select
  end function grad_mu

  elemental function grad_y(y, mu, sigma) result(dy)
    class(*), intent(in) :: y
    class(*), intent(in) :: mu
    class(*), intent(in) :: sigma
    real(rk) :: y_scaled, inv_sigma, dy
    select type (y)
    class default
    select type (mu)
    class default
       select type (sigma)
       class default
          inv_sigma = 1.d0/value_of(sigma)
          y_scaled = (value_of(y) - value_of(mu)) * inv_sigma
          dy = (-1.d0 -y_scaled * inv_sigma) / value_of(y)
       end select
    end select
    end select
  end function grad_y

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
          y_scaled = (log(y) - value_of(mu)) * inv_sigma
          ds = inv_sigma * y_scaled * y_scaled - inv_sigma       
       end select
    end select
  end function grad_sigma

end module fazang_lognormal_lpdf_mod
