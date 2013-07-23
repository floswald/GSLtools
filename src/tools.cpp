
// [[Rcpp::depends(RcppGSL)]]

#include <Rcpp.h>
#include <RcppGSL.h>
#include <gsl/gsl_interp.h>
#include <gsl/gsl_errno.h>

using namespace Rcpp;

// [[Rcpp::export]]
List rcpp_hello_world() {
   
    CharacterVector x = CharacterVector::create( "foo", "bar" )  ;
    NumericVector y   = NumericVector::create( 0.0, 1.0 ) ;
    List z            = List::create( x, y ) ;
    
    return z ;
}

//' Matrix Linear Interpolator
//'
//' takes 2 matrices Y and Xi, and a grid x. x and Y are
//' original function values on x and Xi are new points
//' at which to evaluate the function. Linear Interpolation.
//' @param x grid on which Y=f(x) defined
//' @param Y matrix of funciton values. each row is a different function defined on same grid.
//' @param Xi matrix of new grid values at which to obtain Xi = f(Yi).
//' @examples
//' x <- seq(0,1,le=10)
//' Y <- log(outer(1:5,x,"+"))
//' Xi <- matrix(seq(0.1,0.99,le=12),5,12,byrow=T)
//' res <- matinterp(x,Y,Xi)
// [[Rcpp::export]]
NumericMatrix matinterp( NumericVector x, NumericMatrix Y, NumericMatrix Xi ){

  int n = Y.nrow();
	int m = Y.ncol();
	int mm = Xi.ncol();		// Xi could have more points in theory

	NumericMatrix R(Xi);  //result
	

	if (x.length() != m){
		throw Rcpp::exception( "util_module::ufuns.h::matinterp grid x != ncol(X)");
		return R_NilValue;
	}
		
	NumericVector y(m);
	NumericVector xi(mm);
	
	gsl_interp_accel *acc = gsl_interp_accel_alloc() ;
	gsl_interp *lin = gsl_interp_alloc( gsl_interp_linear , m );
	
	for (int i=0; i<n; i++){

		// plug in current row into row vectors
		y = Y(i,_);
		xi = Xi(i,_);

		if (( xi(0) < x(0)) || (xi(mm-1) > x(m-1))){
			throw Rcpp::exception( "util_module::ufuns.h::matinterp new value xi outside x domain.");
			return R_NilValue;
		}

		// initiate interpolation object
		gsl_interp_init( lin, x.begin(), y.begin(), m );

		// evaluate at new points
		
		for (int j = 0; j < mm; j++){

			// crashes if xi lies outside x
			R(i,j) = gsl_interp_eval(lin, x.begin(), y.begin(), xi(j),acc);
		}
		gsl_interp_accel_reset( acc );
	}
	gsl_interp_free( lin );
	return R;
}
