// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// rcpp_backtest
NumericMatrix rcpp_backtest(NumericMatrix gr, NumericMatrix wt, NumericVector init, NumericVector cont, int rebal);
RcppExport SEXP _firer_rcpp_backtest(SEXP grSEXP, SEXP wtSEXP, SEXP initSEXP, SEXP contSEXP, SEXP rebalSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type gr(grSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type wt(wtSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type init(initSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type cont(contSEXP);
    Rcpp::traits::input_parameter< int >::type rebal(rebalSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_backtest(gr, wt, init, cont, rebal));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_firer_rcpp_backtest", (DL_FUNC) &_firer_rcpp_backtest, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_firer(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}