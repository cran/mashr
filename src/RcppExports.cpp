// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// inv_chol_tri_rcpp
List inv_chol_tri_rcpp(const arma::mat& x_mat);
RcppExport SEXP _mashr_inv_chol_tri_rcpp(SEXP x_matSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type x_mat(x_matSEXP);
    rcpp_result_gen = Rcpp::wrap(inv_chol_tri_rcpp(x_mat));
    return rcpp_result_gen;
END_RCPP
}
// calc_lik_rcpp
List calc_lik_rcpp(const arma::mat& b_mat, const arma::mat& s_mat, const arma::mat& v_mat, const arma::mat& l_mat, NumericVector U_3d, NumericVector sigma_3d, bool logd, bool common_cov, int n_thread);
RcppExport SEXP _mashr_calc_lik_rcpp(SEXP b_matSEXP, SEXP s_matSEXP, SEXP v_matSEXP, SEXP l_matSEXP, SEXP U_3dSEXP, SEXP sigma_3dSEXP, SEXP logdSEXP, SEXP common_covSEXP, SEXP n_threadSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type b_mat(b_matSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type s_mat(s_matSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type v_mat(v_matSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type l_mat(l_matSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type U_3d(U_3dSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type sigma_3d(sigma_3dSEXP);
    Rcpp::traits::input_parameter< bool >::type logd(logdSEXP);
    Rcpp::traits::input_parameter< bool >::type common_cov(common_covSEXP);
    Rcpp::traits::input_parameter< int >::type n_thread(n_threadSEXP);
    rcpp_result_gen = Rcpp::wrap(calc_lik_rcpp(b_mat, s_mat, v_mat, l_mat, U_3d, sigma_3d, logd, common_cov, n_thread));
    return rcpp_result_gen;
END_RCPP
}
// calc_lik_precomputed_rcpp
List calc_lik_precomputed_rcpp(const arma::mat& b_mat, NumericVector rooti_3d, bool logd, bool common_cov, int n_thread);
RcppExport SEXP _mashr_calc_lik_precomputed_rcpp(SEXP b_matSEXP, SEXP rooti_3dSEXP, SEXP logdSEXP, SEXP common_covSEXP, SEXP n_threadSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type b_mat(b_matSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type rooti_3d(rooti_3dSEXP);
    Rcpp::traits::input_parameter< bool >::type logd(logdSEXP);
    Rcpp::traits::input_parameter< bool >::type common_cov(common_covSEXP);
    Rcpp::traits::input_parameter< int >::type n_thread(n_threadSEXP);
    rcpp_result_gen = Rcpp::wrap(calc_lik_precomputed_rcpp(b_mat, rooti_3d, logd, common_cov, n_thread));
    return rcpp_result_gen;
END_RCPP
}
// calc_post_rcpp
List calc_post_rcpp(const arma::mat& b_mat, const arma::mat& s_mat, const arma::mat& s_alpha_mat, const arma::mat& s_orig_mat, const arma::mat& v_mat, const arma::mat& l_mat, const arma::mat& a_mat, NumericVector U_3d, const arma::mat& posterior_weights, bool common_cov, int report_type, int n_thread);
RcppExport SEXP _mashr_calc_post_rcpp(SEXP b_matSEXP, SEXP s_matSEXP, SEXP s_alpha_matSEXP, SEXP s_orig_matSEXP, SEXP v_matSEXP, SEXP l_matSEXP, SEXP a_matSEXP, SEXP U_3dSEXP, SEXP posterior_weightsSEXP, SEXP common_covSEXP, SEXP report_typeSEXP, SEXP n_threadSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type b_mat(b_matSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type s_mat(s_matSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type s_alpha_mat(s_alpha_matSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type s_orig_mat(s_orig_matSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type v_mat(v_matSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type l_mat(l_matSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type a_mat(a_matSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type U_3d(U_3dSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type posterior_weights(posterior_weightsSEXP);
    Rcpp::traits::input_parameter< bool >::type common_cov(common_covSEXP);
    Rcpp::traits::input_parameter< int >::type report_type(report_typeSEXP);
    Rcpp::traits::input_parameter< int >::type n_thread(n_threadSEXP);
    rcpp_result_gen = Rcpp::wrap(calc_post_rcpp(b_mat, s_mat, s_alpha_mat, s_orig_mat, v_mat, l_mat, a_mat, U_3d, posterior_weights, common_cov, report_type, n_thread));
    return rcpp_result_gen;
END_RCPP
}
// calc_sermix_rcpp
List calc_sermix_rcpp(const arma::mat& b_mat, const arma::mat& s_mat, const arma::mat& v_mat, NumericVector vinv_3d, NumericVector U_3d, NumericVector Uinv_3d, NumericVector U0_3d, const arma::mat& posterior_mixture_weights, const arma::mat& posterior_variable_weights, double sigma0, bool common_cov, int n_thread);
RcppExport SEXP _mashr_calc_sermix_rcpp(SEXP b_matSEXP, SEXP s_matSEXP, SEXP v_matSEXP, SEXP vinv_3dSEXP, SEXP U_3dSEXP, SEXP Uinv_3dSEXP, SEXP U0_3dSEXP, SEXP posterior_mixture_weightsSEXP, SEXP posterior_variable_weightsSEXP, SEXP sigma0SEXP, SEXP common_covSEXP, SEXP n_threadSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type b_mat(b_matSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type s_mat(s_matSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type v_mat(v_matSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type vinv_3d(vinv_3dSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type U_3d(U_3dSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type Uinv_3d(Uinv_3dSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type U0_3d(U0_3dSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type posterior_mixture_weights(posterior_mixture_weightsSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type posterior_variable_weights(posterior_variable_weightsSEXP);
    Rcpp::traits::input_parameter< double >::type sigma0(sigma0SEXP);
    Rcpp::traits::input_parameter< bool >::type common_cov(common_covSEXP);
    Rcpp::traits::input_parameter< int >::type n_thread(n_threadSEXP);
    rcpp_result_gen = Rcpp::wrap(calc_sermix_rcpp(b_mat, s_mat, v_mat, vinv_3d, U_3d, Uinv_3d, U0_3d, posterior_mixture_weights, posterior_variable_weights, sigma0, common_cov, n_thread));
    return rcpp_result_gen;
END_RCPP
}
// fit_teem_rcpp
List fit_teem_rcpp(const arma::mat& x_mat, const arma::vec& w_vec, NumericVector U_3d, int maxiter, double converge_tol, double eigen_tol, bool verbose);
RcppExport SEXP _mashr_fit_teem_rcpp(SEXP x_matSEXP, SEXP w_vecSEXP, SEXP U_3dSEXP, SEXP maxiterSEXP, SEXP converge_tolSEXP, SEXP eigen_tolSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type x_mat(x_matSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type w_vec(w_vecSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type U_3d(U_3dSEXP);
    Rcpp::traits::input_parameter< int >::type maxiter(maxiterSEXP);
    Rcpp::traits::input_parameter< double >::type converge_tol(converge_tolSEXP);
    Rcpp::traits::input_parameter< double >::type eigen_tol(eigen_tolSEXP);
    Rcpp::traits::input_parameter< bool >::type verbose(verboseSEXP);
    rcpp_result_gen = Rcpp::wrap(fit_teem_rcpp(x_mat, w_vec, U_3d, maxiter, converge_tol, eigen_tol, verbose));
    return rcpp_result_gen;
END_RCPP
}

RcppExport SEXP _mashr_extreme_deconvolution_rcpp (SEXP ydataSEXP, SEXP ycovarSEXP, SEXP projectionSEXP, SEXP logweightsSEXP, SEXP ampSEXP, SEXP xmeanSEXP, SEXP xcovarSEXP, SEXP fixamp_intSEXP, SEXP fixmean_intSEXP, SEXP fixcovar_intSEXP, SEXP tolSEXP, SEXP maxiterSEXP, SEXP likeonlySEXP, SEXP wSEXP, SEXP logfilenameSEXP, SEXP splitnmergeSEXP, SEXP convlogfilenameSEXP, SEXP noprojSEXP, SEXP diagerrsSEXP, SEXP noweightSEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_mashr_extreme_deconvolution_rcpp", (DL_FUNC) &_mashr_extreme_deconvolution_rcpp, 20},
    {"_mashr_inv_chol_tri_rcpp", (DL_FUNC) &_mashr_inv_chol_tri_rcpp, 1},
    {"_mashr_calc_lik_rcpp", (DL_FUNC) &_mashr_calc_lik_rcpp, 9},
    {"_mashr_calc_lik_precomputed_rcpp", (DL_FUNC) &_mashr_calc_lik_precomputed_rcpp, 5},
    {"_mashr_calc_post_rcpp", (DL_FUNC) &_mashr_calc_post_rcpp, 12},
    {"_mashr_calc_sermix_rcpp", (DL_FUNC) &_mashr_calc_sermix_rcpp, 12},
    {"_mashr_fit_teem_rcpp", (DL_FUNC) &_mashr_fit_teem_rcpp, 7},
    {NULL, NULL, 0}
};

RcppExport void R_init_mashr(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
