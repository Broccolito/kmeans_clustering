kmeans_cluster = function(x, y, n, graphic = TRUE){
  
  # Define distance function
  distance = function(point1, point2){
    return(((point1[1] - point2[1]) ^ 2 + (point1[2] - point2[2]) ^ 2)^ 0.5)
  }
  
  # Setup pointset
  pointset = cbind(x,y)
  
  for(it in 1:100){
    
    # Starting position of two points
    starting_points = matrix(data = rep(0, 2 * n), nrow = n, ncol = 2)
    for(i in 1:n){
      starting_points[i,] = c(quantile(x, (i-0.5)/n),quantile(y, (i-0.5)/n))
    }
    
    dist_mat = matrix(data = rep(0, dim(pointset)[1] * n), nrow = dim(pointset)[1], ncol = n)
    
    for(i in 1:n){
      dist_pi = vector()
      for(j in 1:dim(pointset)[1]){
        dist_pi = c(dist_pi, distance(pointset[j,], starting_points[i,]))
      }
      dist_mat[,i] = dist_pi
    }
    
    which_group = vector()
    for(i in 1:dim(pointset)[1]){
      which_group[i] = which.min(dist_mat[i,])
    }
    
    for(i in 1:n){
      one_group = pointset[which_group == i,]
      if(length(one_group) > 0){
        starting_points[i,] = c(mean(one_group[,1]), mean(one_group[,2]))
      }
    }
    
  }
  
  if(graphic){
    
    plot(x, y, col = which_group + 1, xlab = deparse(substitute(x)), ylab = deparse(substitute(y)), pch = 16)
    
    # Plot out centers
    for(i in 1:n){
      points(starting_points[i,1], starting_points[i,2], cex = 2.5, col = i + 1, pch = 8)
    }
    
  }
  
  return(which_group)
  
}

kmeans_cluster(cars$dist, cars$speed, 5)