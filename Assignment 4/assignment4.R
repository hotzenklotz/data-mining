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

k_means <- function(data, k) {
  n_row <- nrow(data)
  means <- data[sample(n_row, k), ]
  iterations = 0
  
  while(TRUE) {
    iterations <- iterations + 1
    
    cluster_mapping <- mapply(function(i) {
      which.min(sapply(1:k, function(j) { euclidian_distance(t(data[i,]), t(means[j,])) }))  
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