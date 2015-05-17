
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

growTree = function(data) {
  if (homogeneous(data)) {
    return(list(label=label(data)))
  }
  
  splitFeature = bestSplit(data)
  literals = unique(allData[,splitFeature])
  children = NULL
  
  for (literal in literals) {
    dataSubset = subset(allData, allData[splitFeature] == literal)
    
    if (nrow(dataSubset) > 0) {
      children = c(children, growTree(dataSubset))
    } else {
      children = c(children, list(label=label(dataSubset)))
    }
  }
  
  return(list(label=splitFeature, children=children))
}