# 1a
metrics <- function(values) {
  print(mean(values))
  print(median(values))
  print(var(values))
}
 
print("-- 1a --")
values <- c(0.03, 0.04, 0.05, 0.49, 0.50, 0.59, 0.66, 0.72, 0.83, 1.17)
metrics(values)

#1b
print("-- 1b --")
metrics(runif(100, 0, 2))

#1c
values = runif(10000, 0, 1)
hist(values, breaks=20)

#2
print("-- 2 --")
geyser_metrics <- function(values) {
  print(min(values))
  print(max(values))
  mean <- mean(values) 
  print(mean)
  median <- median(values)
  print(median)
  print(abs(mean - median))
}

geyser_metrics(faithful$eruptions)
geyser_metrics(faithful$waiting)

#3a
print("-- 3a --")
library("plyr") 

iris = read.csv("iris.data")

uniqueSpecies <- length(unique(iris$Species))
cat("# unique species", uniqueSpecies, Sep="\n")

speciesCount <- count(iris, "Species")
print(speciesCount)

#3b
plot(iris$Sepal.length, iris$Petal.length, main="All Species: Petal length vs. Sepal length", xlab="Sepal Length", ylab="Petal Length")

#3c
setosa <- subset(iris, Species == "Iris-setosa")
plot(setosa$Sepal.length, setosa$Petal.length, main="Iris Setosa: Petal length vs. Sepal length", xlab="Sepal Length", ylab="Petal Length")

#3d
plot(iris$Sepal.length, iris$Petal.length, main="All Species: Petal length vs. Sepal length", xlab="Sepal Length", ylab="Petal Length", col="blue")
points(setosa$Sepal.length, setosa$Petal.length, col="red")

#4
m <- matrix(runif(50* 100), 50, 100)
correlation <- cor(m)
# Two or one histograms for this task?
hist(correlation, breaks=100, xlim=range(-0.6, 0.6), col="blue")

#5 
age <- c(16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27)
height <- c(76.2, 77.1, 78.1, 78.4, 78.8, 79.7, 79.7, 81.1, 81.2, 81.4, 82.8, 83.5)
plot(height~age)
abline(lm(height~age))

# Bonus task 1
totalPositives = 13
totalNegatives = 6
truePositives = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 8, 9, 10, 10, 11, 11, 12, 12, 13)
falsePositives = c(0, 0, 1, 1, 1, 1, 1, 2, 2, 3, 3, 3, 4, 4, 5, 5, 6, 6)

tpRate = truePositives / totalPositives
fpRate = falsePositives / totalNegatives

plot(fpRate, tpRate,
     main="Bonus Assignment 1: ROC curve",
     xlab="False Positive Rate",
     ylab="True Positive Rate")
lines(fpRate, tpRate)