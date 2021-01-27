#include <Rcpp.h>
using namespace Rcpp;

/*
 //' Local weighting
 //'
 //' Smooth with local weighting
 //'
 //' @param x numeric vector defining spacing
 //' @param y numeric vector defining the series
 //' @param scale scalar numeric quantity defining the reach
 //' @return a numeric vector of smooth values parallel to y
 //' @export
*/

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

