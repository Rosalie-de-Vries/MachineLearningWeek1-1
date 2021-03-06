distmat<-function (X, Y) 
{
  if (!is.numeric(X) || !is.numeric(Y)) 
    stop("X and Y must be numeric vectors or matrices.")
  if (is.vector(X)) 
    dim(X) <- c(1, length(X))
  if (is.vector(Y)) 
    dim(Y) <- c(1, length(Y))
  if (ncol(X) != ncol(Y)) 
    stop("X and Y must have the same number of columns.")
  m <- nrow(X)
  n <- nrow(Y)
  XY <- X %*% t(Y)
  XX <- matrix(rep(apply(X * X, 1, sum), n), m, n, byrow = F)
  YY <- matrix(rep(apply(Y * Y, 1, sum), m), m, n, byrow = T)
  return(sqrt(pmax(XX + YY - 2 * XY, 0)))
}