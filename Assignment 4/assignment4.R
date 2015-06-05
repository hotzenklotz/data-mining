library("parallel")

# task 3a)
data = mtcars[3:6]
pairs(data)

#task 3b)
model <- lm(mtcars$mpg ~ ., data = data)
summary(model)

#task 3c)
disp <- 230 
hp <- 146 
wt <- 3.2
drat <- 3.6

new_data = data.frame(disp, hp, wt, drat)
predict.lm(model, new_data)

# task 4

wine_data = data.matrix(read.csv("winequality-white.csv")[0:11])


euclidian_distance <- function(point1, point2) { 
  return(dist(rbind(point1, point2), method = "euclidian"))
}

manhattan_distance <- function(point1, point2) {
  return(dist(rbind(point1, point2), method ="manhattan"))
}

k_means <- function(data, k, dist_func) {
  n_row <- nrow(data)
  means <- data[sample(n_row, k), ]
  iterations = 0
  
  while(TRUE) {
    iterations <- iterations + 1
    
    cluster_mapping <- mapply(function(i) {
      which.min(sapply(1:k, function(j) { dist_func(t(data[i,]), t(means[j,])) }))  
    }, 1:n_row)#, mc_cores=4)
    
    new_means <- means
    
    for( j in c(1:k)) {
      cluster_j <- data[cluster_mapping == j, ]
      new_means[j,] <- colMeans(cluster_j)
    }
    
    if (sum(means - new_means) == 0) {
      means <- new_means
      break
    } else {
      means <- new_means
    }
  }
  return(list(means=means, cluster_mapping=cluster_mapping, iterations=iterations))
}

medoid <- function(cluster, distance_func) {
  
 distances <- matrix(,nrow=nrow(cluster), ncol=ncol(cluster))  
 #distances <- cluster
 for (i in 1:nrow(cluster)) {
   distances[i,] <- sum(
     mapply(function(j) {
       distance_func(cluster[i,], cluster[nrow(cluster) - j + 1,])
     }, 1:nrow(cluster))
    )
 }
 min_distance_index = which.min(distances)
 return(cluster[min_distance_index, ])
}

# Task 4 Evaluation
result_euclidian = k_means(wine_data, 7, euclidian_distance)
evalutate_cluster(result_euclidian, euclidian_distance)

result_manhattan = k_means(wine_data, 7, manhattan_distance)
evaluate_cluster(result_manhattan, manhattan_distance)

evaluate_cluster <- function(clustering_result, distance_func) {
  print("Iterations")
  print(clustering_result$iterations)
  
  mapply(function(i) {
    cluster <- wine_data[clustering_result$cluster_mapping == i,]
    cat("######## Cluster ", i, "###########\n")
    print("number of data points:")
    print(nrow(cluster))
    print("Centroids:")
    print(clustering_result$means)
    print("Menoids: ")
    print(medoid(cluster, distance_func))
    
  }, 1:7)
}


# Task 5
results = mcmapply(function(i) {
  list(
    mean=k_means(wine_data, i, euclidian_distance),
    median=k_means(wine_data, i, manhattan_distance))
}, 2:10, mc.cores=4)

wine_data = data.frame(wine_data)

#results_mean = results[1,]
#results_mean_1 = results_mean[[1]]
#points = split(wine_data, results_mean_1$cluster_mapping)
#points_1 = points[[1]]
#distances = apply(points_1, 1, function(point) euclidian_distance(point, results_mean_1$means[1,]))

cluster_squared_distances = function(points, mean) {
  squared_distances_cluster = apply(points, 1, function(point) euclidian_distance(point, mean)^2)
  sum(squared_distances_cluster)
}

cluster_manhattan_distances = function(points, mean) {
  distances_cluster = apply(points, 1, function(point) manhattan_distance(point, mean))
  sum(distances_cluster)
}

squared_distances = function(clusterings, cluster_distances) {
  sapply(clusterings, function(clustering) {
    clusters = split(wine_data, clustering$cluster_mapping)
    sum(mapply(cluster_distances, clusters, split(clustering$means, row(clustering$means))))
  })
}

plot(2:10, squared_distances(results[1,], cluster_squared_distances),
     main="K-means clustering",
     xlab="k",
     ylab="Sum of square distances")

plot(2:10, squared_distances(results[2,], cluster_manhattan_distances),
     main="M-medians clustering",
     xlab="k",
     ylab="Sum of manhattan distances")

library("NbClust")
nbclust_results = NbClust(wine_data, min.nc=2, max.nc=10, method="kmeans", index="all")


barplot(table(nbclust_results$Best.nc[1,])[0:8 > 1],
     main="NbClust results",
     xlab="k",
     ylab="Number of indices that support k")