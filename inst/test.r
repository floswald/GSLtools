


# test interpolator

x <- seq(0,1,le=10)
Y <- log(outer(1:5,x,"+"))
Xi <- matrix(seq(0.1,0.99,le=12),5,12,byrow=T)
res <- matinterp(x,Y,Xi)

test_that("interpolated points lie between known points",{
	for (i in 1:nrow(Y)){
		tmp <- findInterval(res[i,],Y[i,])
		tmp2 <- c()
		for (j in 1:ncol(Xi)) tmp2 <- c(tmp2,( (res[i,j] < Y[i,tmp[j]+1]) & (res[i,j] >= Y[i,tmp[j]]) ) )
		expect_that( all(tmp2) ,is_true() )
	}
})

test_that("no point lies outside original funciton values",{
  for (i in 1:nrow(Y)){
    tmp2[i] <- ( min(res[i,]) >= min(Y[i,]) & max(res[i,]) <= max(Y[i,]) )
  }
  expect_that( all(tmp2) ,is_true() )
})
				

res <- matinterp(x,Y,matrix(x,nrow(Y),ncol(Y),byrow=T))
test_that("points on grid are exact",{
	for (i in 1:nrow(Y)){
		tmp2 <- c()
		for (j in 1:ncol(Y)) tmp2 <- c(tmp2, all.equal(res[i,j],  Y[i,j]) )
		expect_that( all(tmp2) ,is_true() )
	}
})


