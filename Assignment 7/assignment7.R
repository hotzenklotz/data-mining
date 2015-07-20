# 3 Ada Boosting
library("ada")

## Override iris species with TRUE / FALSE for binary classifier for "setosa"
iris[, 5] <- as.logical(iris[, 5] == "Iris-setosa")

## 75% of the sample size
n <- nrow(iris)
sample_size <- floor(0.75 * n)

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(n), size = sample_size)

train <- iris[train_ind, ]
test <- iris[-train_ind, ]

classifier <- ada(Species~., data = train, type = "gentle", iter = 70)
classifier <- addtest(classifier, test[, -nrow(test)], test[, nrow(test)])

plot(classifier,TRUE,TRUE)
varplot(classifier)
pairs(classifier, train ,maxvar=2)
summary(classifier)

## Prediciton + Evalution
prediction <- predict(classifier, test[, 1:4])

truePositives <- test[, 5] & as.logical(prediction)
truePositives <- truePositives[truePositives == TRUE]

trueNegatives <-  !test[, 5] & !as.logical(prediction)
trueNegatives <- trueNegatives[trueNegatives == FALSE]

accuracy <- (length(truePositives) + length(trueNegatives)) / nrow(test)
