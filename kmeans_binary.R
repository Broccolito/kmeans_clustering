kmeans_binary = function(x, y){
  
  # Define distance function
  distance = function(point1, point2){
    return(((point1[1] - point2[1]) ^ 2 + (point1[2] - point2[2]) ^ 2)^ 0.5)
  }
  
  plot(x,y)
  pointset = cbind(x,y)
  
  # Starting position of two points
  p1 = c(quantile(x, 0.75),quantile(y, 0.75))
  p2 = c(quantile(x, 0.25),quantile(y, 0.25))
  names(p1) = NULL
  names(p2) = NULL
  
  # Plot out the starting centers
  # points(p1[1], p1[2], cex = 2, col = "blue", pch = 8)
  # points(p2[1], p2[2], cex = 2, col = "red", pch = 8)
  
  for(it in 1:100){
    
    dist_p1 = vector()
    for(i in 1:dim(pointset)[1]){
      dist_p1 = c(dist_p1, distance(pointset[i,], p1))
    }
    
    dist_p2 = vector()
    for(i in 1:dim(pointset)[1]){
      dist_p2 = c(dist_p2, distance(pointset[i,], p2))
    }
    
    cluster1 = pointset[dist_p1 <= dist_p2,]
    cluster2 = pointset[dist_p1 > dist_p2,]
    
    p1 = c(mean(cluster1[,1]), mean(cluster1[,2]))
    p2 = c(mean(cluster2[,1]), mean(cluster2[,2]))
    
  }
  
  points(cluster1, col = "blue", pch = 16)
  points(cluster2, col = "red", pch = 16)
  points(p1[1], p1[2], cex = 2, col = "blue", pch = 8)
  points(p2[1], p2[2], cex = 2, col = "red", pch = 8)
  
  return(list(center1 = p1, center2 = p2, cluster1 = cluster1, cluster2 = cluster2))
  
}

kmeans_binary(cars$speed, cars$dist)
