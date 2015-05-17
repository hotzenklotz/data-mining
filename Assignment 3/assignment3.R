
allData = read.csv("data.csv")

# Dummy implementations for now

homogeneous = function(data) {
  return(F)
}

label = function(data) {
  return("Gifts")
}

bestSplit = function(data) {
  return("Textiles")
}


# Represent the decision tree by its root node.
# 
# Nodes have the following attributes:
# - splitFeature (only internal nodes) Feature by which children are distinguished
# - children (only internal nodes) Vector of children nodes
# - label (only leaf nodes) Predicted class
# - edgeValue Value of splitFeature as defined by the parent node

leafNode = function(label, edgeValue) {
  return(list(label=label, edgeValue=edgeValue))
}

internalNode = function(splitFeature, children, edgeValue) {
  return(list(splitFeature=splitFeature, children=children, edgeValue=edgeValue))
}

growTree = function(data, edgeValue=NULL) {
  if (homogeneous(data)) {
    return(leafNode(label(data), edgeValue))
  }
  
  splitFeature = bestSplit(data)
  literals = unique(allData[,splitFeature])
  children = NULL
  
  for (literal in literals) {
    dataSubset = subset(allData, allData[splitFeature] == literal)
    
    if (nrow(dataSubset) > 0) {
      children = c(children, growTree(dataSubset, literal))
    } else {
      children = c(children, leafNode(label(dataSubset), literal))
    }
  }
  
  return(internalNode(splitFeature, children, edgeValue))
}