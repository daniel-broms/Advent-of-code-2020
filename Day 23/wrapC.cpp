#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar).

// [[Rcpp::export]]
double wrapC(NumericVector x, NumericVector nrofcups) {
  
  if(x[0] > nrofcups[0]){
    return x[0] - nrofcups[0];
  }
  
  if(x[0] < 1){
    return x[0] + nrofcups[0];
  }

  return x[0];
}

/*** R
wrapC(0,9)
*/


// wrap<- function(x,nrofcups){
//   if(x>nrofcups){x<- x-nrofcups}
//   if(x<1){x<- x+nrofcups}
//   return(x)
// }
