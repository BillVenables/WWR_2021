#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector localWeightCpp(NumericVector x, 
                             NumericVector y, 
                             double scale) {
  int n = y.size();
  NumericVector y_wtd(n);
  
  for(int i = 0; i < n; i++) {
    double w, swy = 0.0, sw = 0.0;
    for(int j = 0; j < n; j++) {
      w = exp(-pow((x[i] - x[j])/scale, 2));
      swy += w * y[j];
      sw += w;
    }
    y_wtd[i] = swy/sw;
  }
  return y_wtd;
}

