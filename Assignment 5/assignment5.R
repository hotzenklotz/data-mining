install.packages("prob")
install.packages("mvtnorm")
library("MASS")
library("prob")
library("mvtnorm")

######## task 4 ############

# save some outcomes of a roulette game
r = roulette(european = TRUE)

winnings = 0
rounds = 0
bet = 1
winningBets = c()

while(winnings < 3000) {

  # spin the wheel and draw 
  draw = r[sample(nrow(r), 1), ]
  
  if (draw$color == "Red") {
    winnings = winnings + bet
    winningBets = c(winningBets, bet)
    bet = 1
  } else {
    winnings = winnings - bet
    bet = bet * 2
  }
  
  rounds = rounds + 1
}

print("Rounds")
print(rounds)
print("Winning Bets")
print(winningBets)
summary(winningBets)
plot(table(winningBets), log='x', main = "Winning Bets", xlab = "Single Bets in $", ylab = "Frequency")

########## task 7 ##########

data = iris[c(1,3,5)]
dataByClass = split(data, data$Species)
classes = levels(iris$Species)

classMeans = lapply(dataByClass, function(classData) {
  colMeans(classData[c(1,2)])
})

classSigmas = lapply(dataByClass, function(classData) {
  cov(classData[c(1,2)])
})

predict = function(instance) {
  likelihoods = mapply(function(mean, sigma) {
    dmvnorm(instance, mean, sigma)
  }, classMeans, classSigmas)
  
  classes[which.max(likelihoods)]
}

predict(c(4.5, 2))
predict(c(5, 1.5))
predict(c(5.5, 3))
predict(c(6, 4.3))
predict(c(7, 7))

########## task 8 ##########
sigmoid = Vectorize(function(x) {
  1 / (1 + exp(1)^-x)
})

par(mfrow=c(1,2))
plot(Menarche/Total ~ Age, data=menarche, main="Menarche Age vs Menarche / Total")
plot(sigmoid,-10, 10, main="Sigmoid Function")

par(mfrow=c(1,1))
glm.out = glm(cbind(Menarche, Total-Menarche) ~ Age, family=binomial(logit), data=menarche)
plot(Menarche/Total ~ Age, data=menarche, main="Menarche Data with Fitted Logistic Regression Line")
lines(menarche$Age, glm.out$fitted, type="l", col="red")

