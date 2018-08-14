
library(rpart)

train <- read.csv('./data/train.csv')
test <- read.csv('./data/test.csv')

fit <- rpart(
  Survived ~ Sex + Age + SibSp + Fare + Embarked + Pclass + Parch,
  method = "class",
  data = train,
  control = rpart.control(minsplit = 2, cp = 0.0076)
)
# 
plot(fit)
text(fit)

printcp(fit)
plotcp(fit)


Prediction <- predict(fit, test, type = "class")
Prediction
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
submit
write.csv(submit, file = "./submissions/trevorstephensdtree.csv", row.names = FALSE)