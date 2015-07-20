# 3 Ada Boosting
library("ada")
library(MASS)

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
validation <- iris[-train_ind, 1:4]

classifier <- ada(Species~., data = train, type = "gentle", iter = 70)
classifier <- addtest(classifier, test[, -nrow(test)], test[, nrow(test)])

plot(classifier,TRUE,TRUE)
varplot(classifier)
pairs(classifier, train ,maxvar=2)
summary(classifier)

## Prediciton + Evalution

evaluate <- function(prediciton, test) {
  
  truePositives <- test[, 5] & as.logical(prediction)
  truePositives <- truePositives[truePositives == TRUE]
  
  trueNegatives <-  !test[, 5] & !as.logical(prediction)
  trueNegatives <- trueNegatives[trueNegatives == TRUE]
  
  accuracy <- (length(truePositives) + length(trueNegatives)) / nrow(test)
  
  list(accuracy = accuracy, TN = trueNegatives, TP = truePositives)
}

predictions <- predict(classifier, validation)
print(evaluate(predictions, test))

## 4 Comparison

# Linear Classifier
fit <- vglm(Species~., family=multinomial, data=iris)
predictions <- predict(fit, validation)$class
print(evaluate(predictions, test))

#LDA
fit <- lda(Species~., data=train)
predictions <- predict(fit, validation)$class
print(evaluate(predictions, test))
