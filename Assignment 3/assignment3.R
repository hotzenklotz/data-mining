
allData = read.csv("data.csv")

homogeneous = function(data) {
  return(length(unique(data$Category)) == 1)
}

label = function(data) {
  # Only called if catory is the same within data, so return that category
  return(data[1,]$Category)
}

bestSplit = function(data) {
  return(sample(c("Textiles", "Gifts", "Price"), 1))
}


# Represent the decision tree by its root node.
# 
# Nodes have the following attributes:
# - splitFeature (only internal nodes) Feature by which children are distinguished
# - children (only internal nodes) Vector of children nodes
# - label (only leaf nodes) Predicted class
# - edgeValue Value of splitFeature as defined by the parent node

leafNode = function(label, edgeValue, data) {
  return(list(edgeValue=edgeValue, label=label, data=data))
}

internalNode = function(splitFeature, children, edgeValue, data) {
  return(list(edgeValue=edgeValue, data=data, splitFeature=splitFeature, children=children))
}

growTree = function(data, edgeValue=NULL) {
  
  if (homogeneous(data)) {
    return(leafNode(label(data), edgeValue, data))
  }
  
  splitFeature = bestSplit(data)
  literals = unique(allData[,splitFeature])
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