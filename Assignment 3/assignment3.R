library(plyr)

homogeneous = function(data) {
  return(nrow(unique(data[class_feature])) == 1)
}

label = function(data) {
  # TODO: return category of max frequency
  return(data[1,][class_feature])
}

bestSplit = function(data) {
  maxGain <- 0
  
  for(feature in features) {
    
    splits <- split(data, data[[feature]])
    imp <- impurity(data, splits)
    if (imp > maxGain) {
      maxGain <- imp
      f_best <- feature
    }
  }
  
  f_best
}

impurity <- function(data, splits) {
  
  splitRows = sapply(splits, nrow)
  
  # parent entropy
  parent_frequencies <- count(data[class_feature])$freq / nrow(data)
  parent_entropy <- entropy(parent_frequencies)
  
  # child entropies
  frequencies <- sapply(splits, function(s) {
    count(s[class_feature])$freq / nrow(s)
  })
  
  weights = splitRows / sum(splitRows)
  child_entropies = sapply(frequencies, entropy)
  weighted_average = sum(child_entropies * weights)
  
  # result
  return(parent_entropy - weighted_average)
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

### Task 1 ###
allData = read.csv("data.csv")
features = c("Textiles", "Gifts", "Price")
class_feature = "Category"

tree <- growTree(allData)
print(tree)
##############