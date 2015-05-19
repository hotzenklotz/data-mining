library(plyr)
library(infotheo)

homogeneous = function(data) {
  return(nrow(unique(data[class_feature])) == 1)
}

label = function(data) {
  # TODO: return category of max frequency
  return(data[1,][[class_feature]])
}

bestSplit = function(data) {
  maxGain <- 0
  f_best <- FALSE
  
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

leafNode = function(label, edgeValue) {
  return(list(edgeValue=edgeValue, label=label))
}

internalNode = function(splitFeature, children, edgeValue, label) {
  return(list(edgeValue=edgeValue, label=label, splitFeature=splitFeature, children=children))
}

growTree = function(data, edgeValue=NULL) {
  splitFeature = bestSplit(data)
  
  if (homogeneous(data) || splitFeature == FALSE) {
    return(leafNode(label(data), edgeValue))
  }
  
  literals = unique(allData[[splitFeature]])
  children = list()
  
  for (literal in literals) {
    dataSubset = subset(data, data[splitFeature] == literal)
    
    if (nrow(dataSubset) > 0) {
      children[[length(children) + 1]] = growTree(dataSubset, literal)
    } else {
      children[[length(children) + 1]] = leafNode(label(data), literal)
    }
  }
  
  return(internalNode(splitFeature, children, edgeValue, label(data)))
}

treeStats <- function(tree) {
  internalNodes <- 1
  leaves <- 0
  minDepth <- .Machine$integer.max
  maxDepth <- 1
  
  for (child in tree$children) {
    if ("children" %in% names(child)) {
      subtree <- treeStats(child)
      internalNodes <- internalNodes + subtree$internalNodes
      leaves <- leaves + subtree$leaves
      maxDepth <- max(maxDepth, 1 + subtree$maxDepth)
      minDepth <- min(minDepth, 1 + subtree$minDepth)
    } else {
      leaves <- leaves + 1
      minDepth <- 1
    }
  }
  
  return(list(
      internalNodes=internalNodes,
      leaves=leaves,
      totalNodes=(internalNodes + leaves),
      maxDepth=maxDepth,
      minDepth=minDepth))
}

prediction = function(tree, data) {
  walkTree = function(tree, dataItem) {
    if ("children" %in% names(tree)) {
      # Internal Nodes
      splitFeature = tree$splitFeature
      value = dataItem[[splitFeature]]
      
      for (child in tree$children) {
        if (value == child$edgeValue) {
          return(walkTree(child, dataItem))
        }
      }
      
    } else {
      # Leaves
      return(tree$label)
    }
  }
  
  actual = c()
  predicted = c()
  
  for (i in 1:nrow(data)) {
    row = data[i,]
    actual[[length(actual) + 1]] = row[[class_feature]]
    predicted[[length(predicted) + 1]] = walkTree(tree, row)
  }
  
  return(list(actual=actual, predicted=predicted))
}

# Takes a tree and clones it, except that it removes all leaves which
# only have leaves as siblings.
prune = function(tree) {
  stats = treeStats(tree)
  
  if (stats$maxDepth == 1) {
    # remove Children
    return(leafNode(tree$label, tree$edgeValue))
  }
  
  children = list()
  for (child in tree$children) {
    if ("children" %in% names(child)) {
      children[[length(children) + 1]] = prune(child)
    } else {
      children[[length(children) + 1]] = child
    }
  }
  
  return(internalNode(
      tree$splitFeature, children, tree$edgeValue, tree$label))
}

### Task 1 ###
allData = read.csv("data.csv")
features = c("Textiles", "Gifts", "Price")
class_feature = "Category"

shoppingTree <- growTree(allData)
stats <- treeStats(shoppingTree)
print(shoppingTree)
##############

### Task 2 ###
wineData = read.csv("winequality-white.csv")

# discretize all the wine values (except quality) to avoid overfitting
numCols = ncol(wineData)
discreteData = discretize(wineData[1:numCols - 1])
allData = data.frame(discreteData, wineData[numCols:numCols])

features = c("fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar", "chlorides", "free.sulfur.dioxide", "total.sulfur.dioxide", "density", "pH", "sulphates", "alcohol")
class_feature = "quality"

wineTree <- growTree(allData)
stats <- treeStats(wineTree)
print(stats)
##############

### Task 3 ###
prunedTree = prune(wineTree)
print(treeStats(prunedTree))
##############


### Task 4 ###
testData = allData[1:200,]
predictions = prediction(wineTree, testData)
classes = unique(predictions$actual)

stats = sapply(classes, function(class) {
  predicted = stats$predicted == class
  actual = stats$actual == class
  
  tp = sum(predicted & actual)
  fp = sum(predicted & !actual)
  fn = sum(!predicted & actual)
  
  precision = tp / (tp + fp)
  recall = tp / (tp + fn)
  f1 = 2 * precision * recall / (precision + recall)
  
  list(precision=precision, recall=recall, f1=f1)
})
##############


