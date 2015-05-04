# 1
values <- c(58, 74, 69, 81, 64, 120, 55, 71, 77, 65, 23000)

empVar <- function(values) {
  mean <- mean(values)
  factor = 1 / length(values)
  factor * sum(sapply(values, function(x) { (mean - x)^2 }))
}

sampVar <- function(values) {
  mean <- mean(values)
  factor = 1 / (length(values) - 1)
  factor * sum(sapply(values, function(x) { (mean - x)^2 }))
}

empVar(values)
sampVar(values)
var(values)

# 2a
# Nicht sicher ob das Histgram so richtig ist oder die axen getauscht werden sollten?
binom <- rbinom(1000, 100, 1/2)
data1 = hist(binom, breaks=1000)

# 2b
x <- seq(1, 1000)
data2 <- dbinom(x, 100, 1/2)
data3 <- dbinom(x, 1000, 1/2)
data4 <- dbinom(x, 10000, 1/2)

plot(data2, type="n")
lines(data2, col="green")
lines(data3, col="red")
lines(data4, col="blue")

# 2c
empVar <- function(values) {
  mean <- mean(values)
  factor = 1 / length(values)
  factor * sum(sapply(values, function(x) { (mean - x)^2 }))
}

metrics <- function(val) {
  print(mean(val))
  print(median(val))
  print(empVar(val))
  print(var(val))
}

metrics(data1$counts)
metrics(data2)
metrics(data3)
metrics(data4)

#3 
uniDist <- runif(100000)
normDist <- rnorm(100000, 3, 5)

uniMeans <- colMeans(replicate(5000, sample(uniDist, 100)))
normMeans <- colMeans(replicate(5000, sample(normDist, 100)))

graphs <- function(dist, means) {
    par(mfrow=c(2,4))
    hist(dist)
    plot(density(dist))
    hist(means)
    plot(density(means))
}

graphs(uniDist, uniMeans)
graphs(normDist, normMeans)

#5 
meanDiameter = 2.012
deviation = 0.1
n <- 200

z = (meanDiameter - 2) / (deviation / sqrt(n))

# P(Z < z)
pt(z,df=n-1)


#7
#a
entropy <- function(values) {
  -sum(sapply(values, function(x) { x * log2(x)}))
}

values <- replicate(6, 1/6)
entropy(values)

#b
values <- c(8/100, 15/100, 1/2, 4/100, 15/100, 8/100)
entropy(values)

