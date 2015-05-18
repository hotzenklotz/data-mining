library(plyr)

customers <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12")
textiles <- c("medium", "few", "medium", "many", "few", "many", "few", "medium", "many", "few", "few", "many")
gifts <- c("few", "medium", "many", "few", "medium", "medium", "many", "few", "few", "few", "many", "many")
avgprice <- c("medium", "low", "medium", "high", "high", "low", "low", "low", "low", "high", "medium", "high")
category <- c("T", "N", "TG", "T", "G", "TG", "G", "N", "T", "N", "G", "TG")

d <- data.frame(customers, textiles, gifts, avgprice, category)
features <- c("textiles", "gifts", "avgprice")

# TODO 
# grow_tree <- function(data, features) {
#   
#   if homogenous(data) {
#     return label(data)
#   } else {
#     split <- best_split(data, features)
#   }
#   #split into subsets
#   sapply(data, funtion(data, index){
#     if (length(data) == 0) {
#       tree <- grow_tree(data, features)
#     } else {
#       # Ti is a leaf labelled with Label(D);
#     }
#   })
#   
#   return #a tree whose root is labelled with S and whose children are Ti
# }

homogenous <- function(data) {
  # homogenous if there is only one category left
  nrow(count(data$category)) == 1
}

best_split <- function(data, features) {
  i_min <- 1
  #f_best <- ""
  
  for(feature in features) {

    splits <- split(data, data[feature])
    print(splits)
    imp <- impurity(splits)
    if (imp < i_min) {
      i_min <- imp
      f_best <- feature
    }
  }

  f_best
}

impurity <- function(splits) {
  
  # parent entropy
  parent_frequencies <- count(d["category"])$freq / length(d$category)
  parent_entropy <- entropy(parent_frequencies)
  
  # child entropies
  frequencies <- sapply(splits, function(s) {
    frq <- count(s["category"])$freq / length(splits)
  })

  child_entropies = sapply(frequencies, entropy)
  weighted_average = sapply(frequencies, sum) * child_entropies 
  
  # result
  result <- parent_entropy - mean(weighted_average)
  print(result)
  result
}

entropy <- function(values) {
  -sum(sapply(values, function(x) { x * log2(x)}))
}