#include "Rcpp.h"
using namespace Rcpp;

//'  projection of points onto line
//' 
//' This function returns projection point
//' @param p0 A nuemric matrix (n*2) defining points in (long lat)  format to be projected onto line defined by l0, l1 
//' @param l0 A numeric vector
//' @param l1 A numeric vector
//' @param profiel A integer identfier for the profile
//' @export
// [[Rcpp::export]]
NumericMatrix

pnt_line_vec(const NumericMatrix& p0,
             const NumericVector& l0,
             const NumericVector& l1,
             const int& profile) {

  /*    input p0 matrix (n*2) defining points in (long lat)  format to be projected onto line defined by l0, l1  */
    /*    return  four column matrix, projected lon, projected lat, distance from line, whethr projected coords are inside[1] or outside [0] line   */

    int nrow = p0.nrow();   /* ncol = p0.ncol(); */
      NumericVector lv(2);
    NumericMatrix pl(nrow,5);
    double t = 0 ;

    lv[0]=l1[0]-l0[0];
    lv[1]=l1[1]-l0[1];
    double l = sqrt(lv[0]*lv[0]+lv[1]*lv[1]);
    lv[0]= lv[0]/l;
    lv[1] =lv[1]/l;
    double denom = (lv[0]* lv[0]+lv[1]*lv[1]);
    double numer1= (l0[0] * lv[0]+ l0[1]*lv[1]);
    for (int i = 0; i < nrow; i++) {
      /*  REprintf("%i ", i); */
        t = -(-(lv[0] * p0(i,0)+lv[1]*p0(i,1)) +  numer1)/ denom;
        pl(i, 0)= t * lv[0] + l0[0];
        pl(i, 1)= t * lv[1] + l0[1];
        pl(i, 2) = sqrt( (p0(i,0) - pl(i,0))* (p0(i,0) - pl(i,0)) + (p0(i,1) - pl(i,1))*(p0(i,1) - pl(i,1)) );
        if (t<0 || t>l) {pl(i, 3) = 0;} else {pl(i, 3)=1;}
        pl(i, 4)=profile;
    }

    return pl;
}
