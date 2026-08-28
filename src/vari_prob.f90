#include "vari_op_inc.f90"

DEF_VARI1_INT_MOD(bernoulli_lpmf_vari_mod, fz_vari, n*log(vi%val_) + (1_ik-n)*log(1.0d0-vi%val_), (this%val_ * (1.d0 - this%val_)) )

DEF_VARI2_REAL_MOD(normal_lpdf_vari_mod, fz_vari, normal_lpdf_d_d_d(vi_val(a), vi_val(b), c), ((c - vi_val(a)) / (vi_val(b) * vi_val(b))), normal_dsigma(vi_val(a), vi_val(b), c))

DEF_VARI2_REAL_MOD(lognormal_lpdf_vari_mod, fz_vari, lognormal_lpdf_d_d_d(vi_val(a), vi_val(b), c), (log(c) - vi_val(a)) / (vi_val(b) * vi_val(b)), lognormal_dsigma(vi_val(a), vi_val(b), c))

DEF_VARI2_REAL_MOD(weibull_lpdf_vari_mod, fz_vari, weibull_lpdf_d_d_d(vi_val(a), vi_val(b), c), weibull_dshape(vi_val(a), vi_val(b), c), weibull_dscale(vi_val(a), vi_val(b), c))

DEF_VARI2_REAL_MOD(cauchy_lpdf_vari_mod, fz_vari, cauchy_lpdf_d_d_d(vi_val(a), vi_val(b), c), cauchy_dloc(vi_val(a), vi_val(b), c), cauchy_dscale(vi_val(a), vi_val(b), c))

DEF_VARI2_REAL_MOD(gumbel_lpdf_vari_mod, fz_vari, gumbel_lpdf_d_d_d(vi_val(a), vi_val(b), c), gumbel_dloc(vi_val(a), vi_val(b), c), gumbel_dscale(vi_val(a), vi_val(b), c))

DEF_VARI2_REAL_MOD(logistic_lpdf_vari_mod, fz_vari, logistic_lpdf_d_d_d(vi_val(a), vi_val(b), c), logistic_dloc(vi_val(a), vi_val(b), c), logistic_dscale(vi_val(a), vi_val(b), c))

DEF_VARI2_REAL_MOD(laplace_lpdf_vari_mod, fz_vari, laplace_lpdf_d_d_d(vi_val(a), vi_val(b), c), laplace_dloc(vi_val(a), vi_val(b), c), laplace_dscale(vi_val(a), vi_val(b), c))

DEF_VARI1_REAL_MOD(chi_square_lpdf_vari_mod, fz_vari, chi_square_lpdf_d_d(vi%val_, b), chi_square_dnu(a%val_, b))

DEF_VARI1_REAL_MOD(inv_chi_square_lpdf_vari_mod, fz_vari, inv_chi_square_lpdf_d_d(vi%val_, b), inv_chi_square_dnu(a%val_, b))
