#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector localWeightCpp(NumericVector x,
                             NumericVector y,
                             double scale,
                             NumericVector xo)
{
  int no = xo.size(), n = x.size();
  NumericVector y_smo(no);

  for(int i = 0; i < no; i++)
  {
    double w, swy = 0.0, sw = 0.0;
    for(int j = 0; j < n; j++)
    {
      w = exp(-pow((xo[i] - x[j])/scale, 2)/2);
      swy += w * y[j];
      sw += w;
    }
    y_smo[i] = swy/sw;
  }
  return y_smo;
}

// [[Rcpp::export]]
NumericVector densityCpp(NumericVector x, double scale, NumericVector xo)
{
  int n = x.size(), no = xo.size();
  NumericVector d(no);
  for(int i = 0; i < no; i++)
  {
    d[i] = 0.0;
    for(int j = 0; j < n; j++)
    {
      d[i] += exp(-pow((xo[i] - x[j])/scale, 2)/2);
    }
  }
  double A = 0.0;
  for(int i = 1; i < no; i++)
    A += (d[i-1] + d[i])*(xo[i] - xo[i-1])/2;
  for(int i = 0; i < no; i++)
    d[i] /= A;
  return d;
}
