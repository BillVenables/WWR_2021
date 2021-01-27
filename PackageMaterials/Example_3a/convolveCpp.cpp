#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector convolveCpp(NumericVector a, NumericVector b)
{
    int na = a.size();
    int nb = b.size();
    NumericVector ab(na + nb - 1, 0.0);
    for(int i = 0; i < na; ++i) {
    	for(int j = 0; j < nb; ++j) {
	      ab[i+j] += a[i]*b[j];
	    }
    }
    return ab;
}


