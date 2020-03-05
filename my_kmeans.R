my_kmeans <- function(x,K=4,maxiter=15,do_plot=FALSE)
{
  # import packages
  library(pracma) # Practical Math operations, like distmat to compute a distance matrix
  
  # Code your k-means
  ################################
  # R4
  # YOUR CODE HERE
  # initialize
  indeces <- #?
  cluCentroids <- #?
  D <- #?
  assigned_clusterIDs <- #?
  
  if (do_plot) {
    plot(x[,1], x[,2], col = palette()[assigned_clusterIDs])
    points(cluCentroids[,1], cluCentroids[,2], col = palette(),pch=19,cex=2)
    Sys.sleep(0.5)
    print(0)
  }
  
  for (i in 1:maxiter) {
    
    
    D <- #?
    assigned_clusterIDs <- #?
    for (k in 1:K) {
      cluCentroids[k,] <- #?
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