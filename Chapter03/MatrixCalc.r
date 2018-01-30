X = matrix( c(1,2,3,4,5,6), nrow=3,ncol=2)
X
t(X)%*%X
solve(t(X)%*%X)