#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix rcpp_backtest(NumericMatrix gr, NumericMatrix wt, NumericVector init, NumericVector cont, int rebal) {
  int n = gr.nrow();
  int m = gr.ncol();

  NumericMatrix init_share(n, m);
  NumericMatrix cont_share(n, m);
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < m; j++) {
      init_share(i, j) = init(i) * wt(i, j);
      cont_share(i, j) = cont(i) * wt(i, j);
    }
  }
  
  NumericMatrix bal(n, m);
  
  // init
  NumericVector tot(n);
  for (int j = 0; j < m; j++) {
    bal(0, j) = init_share(0, j) * gr(0, j) + cont_share(0, j);
    tot(0) += bal(0, j);
  }
  
  // loop
  for (int i = 1; i < n; i++) {
    if (i % rebal != 0) {
      // maintaining
      for (int j = 0; j < m; j++) {
        bal(i, j) = bal(i-1, j) * gr(i, j) + cont_share(i, j);
        tot(i) += bal(i, j);
      }
    } else {
      // re-balancing
      for (int j = 0; j < m; j++) {
        bal(i, j) = tot(i-1) * wt(i, j) * gr(i, j) + cont_share(i, j); // weighting
        tot(i) += bal(i, j);
      }
    }
  }
  return bal;
}

