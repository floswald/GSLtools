\name{matinterp}
\alias{matinterp}
\title{Matrix Interpolator}
\usage{
  matinterp(x, Y, Xi)
}
\arguments{
  \item{x}{grid on which Y=f(x) defined}

  \item{Y}{matrix of funciton values. each row is a
  different function defined on same grid.}

  \item{Xi}{matrix of new grid values at which to obtain Xi
  = f(Yi).}
}
\description{
  takes 2 matrices Y and Xi, and a grid x. x and Y are
  original function values on x and Xi are new points at
  which to evaluate the function
}
\examples{
x <- seq(0,1,le=10)
Y <- log(outer(1:5,x,"+"))
Xi <- matrix(seq(0.1,0.99,le=12),5,12,byrow=T)
res <- matinterp(x,Y,Xi)
}

