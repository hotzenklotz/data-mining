install.packages("prob")
library("parallel")
library("MASS")
library("prob")

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

########## task 8 ##########
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

