library("parallel")
library("MASS")

# task 8
sigmoid = Vectorize(function(x) {
  1 / (1 + exp(1)^-x)
})

par(mfrow=c(1,2))
plot(Menarche/Total ~ Age, data=menarche, main="Menarche Age vs Menache / Total", xlab="Age", ylab = "Menarche / Total")
plot(sigmoid,-10, 10, main="Sigmoid Function")

par(mfrow=c(1,1))
glm.out = glm(cbind(Menarche, Total-Menarche) ~ Age, family=binomial(logit), data=menarche)
plot(Menarche/Total ~ Age, data=menarche, main="Menarche Data with Fitted Logistic Regression Line")
lines(menarche$Age, glm.out$fitted, type="l", col="red")

