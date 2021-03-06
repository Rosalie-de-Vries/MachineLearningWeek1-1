my_kmeans <- function(x,K=4,maxiter=15,do_plot=TRUE)
{
  # import packages
  if (!require('pracma')) install.packages('pracma')
  library(pracma) # Practical Math operations, like distmat to compute a distance matrix
  
  # Code your k-means
  ################################
  # R4
  # YOUR CODE HERE
  # initialize
  indeces <- sample(1:nrow(x), K, replace = T)
  cluCentroids <- as.matrix(x[indeces, ])
  D <- distmat(as.matrix(x), cluCentroids)
  assigned_clusterIDs <- apply(D, 1, which.min)
    
  #if (do_plot=TRUE) {
    plot(x[,1], x[,2], col = palette()[assigned_clusterIDs])
    points(cluCentroids[,1], cluCentroids[,2], col = palette(),pch=19,cex=2)
    Sys.sleep(0.5)
    print(0)
  #}
  
  for (i in 1:maxiter) {

          
    D <- distmat(as.matrix(x), cluCentroids)
    assigned_clusterIDs <- apply(D, 1, which.min)
    for (k in 1:K) {
      cluCentroids[k,] <- colMeans(Data2D_scaled[assigned_clusterIDs==k,])
    }
    ################################
    if (do_plot) {
      plot(x[,1], x[,2], col = palette()[assigned_clusterIDs])
      points(cluCentroids[,1], cluCentroids[,2], col = palette(),pch=19,cex=2)
      Sys.sleep(0.5)
      print(i)
    }
  }
  out <- {}
  out$cluster <- assigned_clusterIDs
  out$centers <- cluCentroids
  return(out)  
}