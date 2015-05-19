library(plyr)

homogeneous = function(data) {
  return(nrow(unique(data[class_feature])) == 1)
}

label = function(data) {
  # TODO: return category of max frequency
  return(data[1,][class_feature])
}

bestSplit = function(data) {
  iMin <- .Machine$double.max
  
  for(feature in features) {
    
    splits <- split(data, data[[feature]])
    imp <- impurity(splits)
    if (imp < iMin) {
      iMin <- imp
      f_best <- feature
    }
  }
  
  f_best
}

impurity <- function(splits) {
  
  splitRows = sapply(splits, nrow)
  
  frequencies <- sapply(splits, function(s) {
    count(s[class_feature])$freq / nrow(s)
  })
  
  weights = splitRows / sum(splitRows)
  child_entropies = sapply(frequencies, entropy)
  weighted_average = sum(child_entropies * weights)
  
  weighted_average
}

entropy <- function(values) {
  if (length(values) == 0) {
    return(0)
  }
  -sum(sapply(values, function(x) { x * log2(x)}))
}


# Represent the decision tree by its root node.
# 
# Nodes have the following attributes:
# - splitFeature (only internal nodes) Feature by which children are distinguished
# - children (only internal nodes) Vector of children nodes
# - label (only leaf nodes) Predicted class
# - edgeValue Value of splitFeature as defined by the parent node

leafNode = function(label, edgeValue, data) {
  return(list(edgeValue=edgeValue, label=label))
}

internalNode = function(splitFeature, children, edgeValue, data) {
  return(list(edgeValue=edgeValue, splitFeature=splitFeature, children=children))
}

growTree = function(data, edgeValue=NULL) {
  
  if (homogeneous(data)) {
    return(leafNode(label(data), edgeValue, data))
  }
  
  splitFeature = bestSplit(data)
  literals = unique(allData[[splitFeature]])
  children = list()
  
  for (literal in literals) {
    dataSubset = subset(data, data[splitFeature] == literal)
    
    if (nrow(dataSubset) > 0) {
      children[[length(children) + 1]] = growTree(dataSubset, literal)
    } else {
      children[[length(children) + 1]] = leafNode(label(data), literal, dataSubset)
    }
  }
  
  return(internalNode(splitFeature, children, edgeValue, data))
}

tree_stats <- function(tree) {
  nodes <- 1
  leaves <- 0
  
  for (child in tree$children) {
    if ("children" %in% names(child)) {
      nodes <- nodes + 1
      subtree <- tree_stats(child)
      nodes <- nodes + subtree$nodes
      leaves <- leaves + subtree$leaves
    } else {
      leaves <- leaves + 1
    }
  }
  return(list(nodes=nodes, leaves=leaves))
}

### Task 1 ###
allData = read.csv("data.csv")
features = c("Textiles", "Gifts", "Price")
class_feature = "Category"

tree <- growTree(allData)
stats <- tree_stats(tree)
print(tree)
##############

### Task 2 ###
allData = read.csv("winequality-white.csv")
features = c("fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar", "chlorides", "free.sulfur.dioxide", "total.sulfur.dioxide", "density", "pH", "sulphates", "alcohol")
class_feature = "quality"

tree <- growTree(allData)
print(tree)
##############