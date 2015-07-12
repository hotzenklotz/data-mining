require(mixtools)

data <- scan("gaussians.txt")

# Task 7

hist(data, breaks=100)

model <- normalmixEM(data, k=3)
summary.mixEM(model)
plot.mixEM(model, whichplots=2, main2=paste("Gaussian Mixture Model (k = 3)"), breaks=100)
