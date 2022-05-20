// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// ind_to_coord
NumericVector ind_to_coord(const int index, const int grid_nrow);
RcppExport SEXP _sousmarin_ind_to_coord(SEXP indexSEXP, SEXP grid_nrowSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const int >::type index(indexSEXP);
    Rcpp::traits::input_parameter< const int >::type grid_nrow(grid_nrowSEXP);
    rcpp_result_gen = Rcpp::wrap(ind_to_coord(index, grid_nrow));
    return rcpp_result_gen;
END_RCPP
}
// coord_to_dist
double coord_to_dist(const NumericVector& p1, const NumericVector& p2);
RcppExport SEXP _sousmarin_coord_to_dist(SEXP p1SEXP, SEXP p2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericVector& >::type p1(p1SEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type p2(p2SEXP);
    rcpp_result_gen = Rcpp::wrap(coord_to_dist(p1, p2));
    return rcpp_result_gen;
END_RCPP
}
// cov_model_exp_cpp
double cov_model_exp_cpp(const double d, double a);
RcppExport SEXP _sousmarin_cov_model_exp_cpp(SEXP dSEXP, SEXP aSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const double >::type d(dSEXP);
    Rcpp::traits::input_parameter< double >::type a(aSEXP);
    rcpp_result_gen = Rcpp::wrap(cov_model_exp_cpp(d, a));
    return rcpp_result_gen;
END_RCPP
}
// cov_model_gauss
double cov_model_gauss(const double d, double a, double m);
RcppExport SEXP _sousmarin_cov_model_gauss(SEXP dSEXP, SEXP aSEXP, SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const double >::type d(dSEXP);
    Rcpp::traits::input_parameter< double >::type a(aSEXP);
    Rcpp::traits::input_parameter< double >::type m(mSEXP);
    rcpp_result_gen = Rcpp::wrap(cov_model_gauss(d, a, m));
    return rcpp_result_gen;
END_RCPP
}
// cov_model_sphere
double cov_model_sphere(const double d, double a, double m);
RcppExport SEXP _sousmarin_cov_model_sphere(SEXP dSEXP, SEXP aSEXP, SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const double >::type d(dSEXP);
    Rcpp::traits::input_parameter< double >::type a(aSEXP);
    Rcpp::traits::input_parameter< double >::type m(mSEXP);
    rcpp_result_gen = Rcpp::wrap(cov_model_sphere(d, a, m));
    return rcpp_result_gen;
END_RCPP
}
// get_cov_row_exp
NumericVector get_cov_row_exp(const int i, const int grid_nrow, const double a);
RcppExport SEXP _sousmarin_get_cov_row_exp(SEXP iSEXP, SEXP grid_nrowSEXP, SEXP aSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const int >::type i(iSEXP);
    Rcpp::traits::input_parameter< const int >::type grid_nrow(grid_nrowSEXP);
    Rcpp::traits::input_parameter< const double >::type a(aSEXP);
    rcpp_result_gen = Rcpp::wrap(get_cov_row_exp(i, grid_nrow, a));
    return rcpp_result_gen;
END_RCPP
}
// get_cov_matrix_exp
NumericMatrix get_cov_matrix_exp(const int grid_nrow, const double a);
RcppExport SEXP _sousmarin_get_cov_matrix_exp(SEXP grid_nrowSEXP, SEXP aSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const int >::type grid_nrow(grid_nrowSEXP);
    Rcpp::traits::input_parameter< const double >::type a(aSEXP);
    rcpp_result_gen = Rcpp::wrap(get_cov_matrix_exp(grid_nrow, a));
    return rcpp_result_gen;
END_RCPP
}
// get_cov_matrix_gauss
NumericMatrix get_cov_matrix_gauss(const int grid_nrow, const double a, const double m);
RcppExport SEXP _sousmarin_get_cov_matrix_gauss(SEXP grid_nrowSEXP, SEXP aSEXP, SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const int >::type grid_nrow(grid_nrowSEXP);
    Rcpp::traits::input_parameter< const double >::type a(aSEXP);
    Rcpp::traits::input_parameter< const double >::type m(mSEXP);
    rcpp_result_gen = Rcpp::wrap(get_cov_matrix_gauss(grid_nrow, a, m));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_sousmarin_ind_to_coord", (DL_FUNC) &_sousmarin_ind_to_coord, 2},
    {"_sousmarin_coord_to_dist", (DL_FUNC) &_sousmarin_coord_to_dist, 2},
    {"_sousmarin_cov_model_exp_cpp", (DL_FUNC) &_sousmarin_cov_model_exp_cpp, 2},
    {"_sousmarin_cov_model_gauss", (DL_FUNC) &_sousmarin_cov_model_gauss, 3},
    {"_sousmarin_cov_model_sphere", (DL_FUNC) &_sousmarin_cov_model_sphere, 3},
    {"_sousmarin_get_cov_row_exp", (DL_FUNC) &_sousmarin_get_cov_row_exp, 3},
    {"_sousmarin_get_cov_matrix_exp", (DL_FUNC) &_sousmarin_get_cov_matrix_exp, 2},
    {"_sousmarin_get_cov_matrix_gauss", (DL_FUNC) &_sousmarin_get_cov_matrix_gauss, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_sousmarin(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
