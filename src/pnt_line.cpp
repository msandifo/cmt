#include "Rcpp.h"
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector

pnt_line(const NumericVector& p0,
         const NumericVector& l0,
         const NumericVector& l1) {

  int n = p0.size();
  NumericVector lv(n);
  lv[0]=l1[0]-l0[0];
  lv[1]=l1[1]-l0[1];
  double l = sqrt(lv[0]*lv[0]+lv[1]*lv[1]);
  lv[0]= lv[0]/l;
  lv[1] =lv[1]/l;
  double t = -(-(lv[0] * p0[0]+lv[1]*p0[1]) +  (l0[0] * lv[0]+l0[1]*lv[1]))/ (lv[0] * lv[0]+lv[1]*lv[1]);
  NumericVector pl(n);
  pl[0]= t * lv[0] + l0[0];
  pl[1]= t * lv[1] + l0[1];

  return pl;
}
