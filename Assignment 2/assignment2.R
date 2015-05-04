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
empVar <- function(values) {
  mean <- mean(values)
  factor = 1 / length(values)
  factor * sum(sapply(values, function(x) { (mean - x)^2 }))
}

dist1 = rbinom(100, 100, 0.5)
dist2 = rbinom(1000, 100, 0.5)
dist3 = rbinom(10000, 100, 0.5)

par(mfrow=c(1,3))
hist(dist1, main="Heads per 100 Flips")
hist(dist2, main="Heads per 1000 Flips")
hist(dist3, main="Heads per 1000 Flips")

# 2b
library("plyr")
mapply(each(mean, median, empVar, var), data.frame(
  "100 Flips" = dist1,
  "1000 Flips" = dist2,
  "10000 Flips" = dist3)
)

# 2c
# TODO georg
x <- seq(1, 1000)
data2 <- dbinom(x, 100, 1/2)
data3 <- dbinom(x, 1000, 1/2)
data4 <- dbinom(x, 10000, 1/2)

plot(data2, type="n")
lines(data2, col="green")
lines(data3, col="red")
lines(data4, col="blue")

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

# 4d
algoA <- c(0.90, 0.87, 0.92, 0.88, 0.90, 0.85, 0.91, 0.90, 0.87, 0.95, 0.90)
algoB <- c(0.89, 0.95, 0.87, 0.94, 0.92, 0.86, 0.84, 0.92, 0.88, 0.83, 0.90)
k = 11

eabar = mean(algoA)
ebbar = mean(algoB)
dbar = eabar - ebbar

s_d = sqrt(var(algoA) / k + var(algoB) / k)

#4e
y = (dbar) * sqrt(k) / s_d

alpha = 1 - 0.95
t = qt(1-alpha/2, df=(k-1))

acceptH0 = abs(y) < t

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

