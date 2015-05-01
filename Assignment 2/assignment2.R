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

plot(data2, col="green")
points(data3, col="red")
points(data4, col="blue")

# 2c
empVar <- function(values) {
  mean <- mean(values)
  factor = 1 / length(values)
  factor * sum(sapply(values, function(x) { (mean - x)^2 }))
}

metrics <- function(val) {
  mean(val)
  median(val)
  empVar(val)
  var(val)
}

metrics(data1$counts)
metrics(data2)
metrics(data3)
metrics(data4)

#3 
uniDist <- runif(100000)
samples <- replicate(5000, function() { samples(uniDist, 100) })

#5 
meanDiameter = 2.012
deviation = 0.1
n <- 200

z = (meanDiameter - 2) / (deviation / sqrt(n))

# P(Z < z)
pt(z,df=n-1)


#7
#a
values <- c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6)
-sum(sapply(values, function(x) { x * log(x)}))

#b
values <- c(8/100, 15/100, 1/2, 4/100, 15/100, 8/100)
-sum(sapply(values, function(x) { x * log(x)}))
