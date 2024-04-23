#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

//' From observed inheritance, determine
//'  inheritance that is been given
//'
//' @param x Observed inheritance vector
//' @param deduction Level of tax deduction
//' @param bounds_legal Bounds for the inheritance
//'    tax rates
//' @param taxes Tax rates to be applied
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector taxation_inheritance(
    Rcpp::NumericVector x,
    double deduction,
    Rcpp::NumericVector bounds_legal,
    Rcpp::NumericVector taxes){


  if (taxes.length() != bounds_legal.length() + 1){
    stop("length(taxes) must be equal to length(bounds_legal)+1");
  }


  Rcpp::NumericVector transmission_nette(x.length()) ;
  Rcpp::NumericVector transmission_avant_impot(x.length()) ;
  Rcpp::NumericVector taxes_paid(x.length());
  Rcpp::NumericVector lower_bound(taxes.length());
  Rcpp::NumericVector bounds(taxes.length());

  for (int j=0 ; j < taxes.length() ; j++){
    bounds[j] += bounds_legal[j]*(1+taxes[j]);
    lower_bound[j] = bounds[j-1];
  }

  for (int i = 0 ; i < transmission_nette.length(); i++){

    transmission_nette[i] += (x[i] - deduction) ;

    for (int j = 0 ; j < lower_bound.length()-1; j++){


      if (transmission_nette[i] > lower_bound[j]){

        if (transmission_nette[i] < lower_bound[j+1]){
          taxes_paid[i] += taxes[j]*(transmission_nette[i] - (lower_bound[j]-1));
        } else{
          taxes_paid[i] += taxes[j]*(lower_bound[j+1] - (lower_bound[j]-1));
        }

      }

    }


    if (transmission_nette[i]>max(bounds)){
      taxes_paid[i] += (transmission_nette[i] - max(bounds))*max(taxes);
    }

    transmission_avant_impot[i] += x[i] + taxes_paid[i] ;

  }

  return transmission_avant_impot;
}
