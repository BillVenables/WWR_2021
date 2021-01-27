#include <Rcpp.h>
using namespace Rcpp;

//' Convolution with Rcpp
//'
//' Compute the convolution of two numeric sequences
//' @param a a numeric vector
//' @param b a numeric vector
//' @export
//' @return a numeric vector, the convolution of a and b
//' @examples
//' p <- pascal <- c(1,1)
//' for(i in 1:10) {
//'   cat(paste0(i, ": ", paste(pascal, collapse=" "), "\n"))
//'   pascal <- convolveCpp(p, pascal)
//' }
//' rm(p, pascal)
// [[Rcpp::export]]
NumericVector convolveCpp(NumericVector a, NumericVector b)
{
    int na = a.size();
    int nb = b.size();
    NumericVector c(na + nb - 1, 0.0);
    for(int i = 0; i < na; ++i) {
    	for(int j = 0; j < nb; ++j) {
	      c[i+j] += a[i]*b[j];
	    }
    }
    return c;
}


