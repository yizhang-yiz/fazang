#include "vari_op_inc.f90"

DEF_VARI1_INT_MOD(vari, bernoulli_lpmf, n*log(vi%val_) + (1_ik-n)*log(1.0d0-vi%val_), (this%val_ * (1.d0 - this%val_)) )

DEF_VARI2_REAL_MOD(vari, normal_lpdf, normal_lpdf_d_d_d(vi_val(a), vi_val(b), c), ((c - vi_val(a)) / (vi_val(b) * vi_val(b))), normal_dsigma(vi_val(a), vi_val(b), c))

DEF_VARI2_REAL_MOD(vari, lognormal_lpdf, lognormal_lpdf_d_d_d(vi_val(a), vi_val(b), c), (log(c) - vi_val(a)) / (vi_val(b) * vi_val(b)), lognormal_dsigma(vi_val(a), vi_val(b), c))

DEF_VARI2_REAL_MOD(vari, weibull_lpdf, weibull_lpdf_d_d_d(vi_val(a), vi_val(b), c), weibull_dshape(vi_val(a), vi_val(b), c), weibull_dscale(vi_val(a), vi_val(b), c))

DEF_VARI2_REAL_MOD(vari, cauchy_lpdf, cauchy_lpdf_d_d_d(vi_val(a), vi_val(b), c), cauchy_dloc(vi_val(a), vi_val(b), c), cauchy_dscale(vi_val(a), vi_val(b), c))

DEF_VARI2_REAL_MOD(vari, gumbel_lpdf, gumbel_lpdf_d_d_d(vi_val(a), vi_val(b), c), gumbel_dloc(vi_val(a), vi_val(b), c), gumbel_dscale(vi_val(a), vi_val(b), c))

DEF_VARI2_REAL_MOD(vari, logistic_lpdf, logistic_lpdf_d_d_d(vi_val(a), vi_val(b), c), logistic_dloc(vi_val(a), vi_val(b), c), logistic_dscale(vi_val(a), vi_val(b), c))

DEF_VARI2_REAL_MOD(vari, laplace_lpdf, laplace_lpdf_d_d_d(vi_val(a), vi_val(b), c), laplace_dloc(vi_val(a), vi_val(b), c), laplace_dscale(vi_val(a), vi_val(b), c))

DEF_VARI1_REAL_MOD(vari, chi_square_lpdf, chi_square_lpdf_d_d(vi%val_, b), chi_square_dnu(a%val_, b))

DEF_VARI1_REAL_MOD(vari, inv_chi_square_lpdf, inv_chi_square_lpdf_d_d(vi%val_, b), inv_chi_square_dnu(a%val_, b))
