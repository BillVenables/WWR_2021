#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector convolve3a(NumericVector x, NumericVector y)
{
    int nx = x.size();
    int ny = y.size();
    int nz = nx + ny - 1;
    NumericVector z(nz);
    //    std::fill(z.begin(), z.end(), 0.0);

    for(int i = 0; i < nx; ++i) {
	for(int j = 0; j < ny; ++j) {
	    z[i+j] += x[i]*y[j];
	}
    }
    return z;
}


