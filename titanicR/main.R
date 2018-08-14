require(rpart)

train <- read.csv('./data/train.csv')
test <- read.csv('./data/test.csv')

model.train <- rpart(
  Survived ~ Sex + Age + SibSp + Fare + Embarked + Pclass + Parch,
  method = "class",
  data = train,
  control = rpart.control(minsplit = 2, cp = 0)
)



# model.train.pruned = prune(model.train, cp = 0.022)

# printcp(model.train)
# plotcp(model.train)
# 
# printcp(model.train.pruned)
# plotcp(model.train.pruned)
# 
# cptarg = sqrt(model.train$cptable[4,1] * model.train$cptable[5,1])
# model.train.pruned = prune(model.train, cp=cptarg)


cprow <- model.train$cptable[model.train$cptable[,4] == min(model.train$cptable[,4]),]

cprow[4]
cprow[5]
mintarget = cprow[4] + cprow[5]
mintarget
maxxerror <-  max(model.train$cptable[model.train$cptable[,4] < mintarget,][,4])
maxxerror
maxcp <- model.train$cptable[model.train$cptable[,4] == maxxerror,]
maxcp
fit = prune(model.train, cp=maxcp[1])


par(mfrow = c(1, 1), mar = c(1, 1, 1, 1))
plot(fit, uniform = T, compress = T, 
     margin = 0.1, branch = 0.3)
text(fit, use.n = T, digits = 3, cex = 0.6)

Prediction <- predict(fit, test, type = "class")
# Prediction
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
# submit
write.csv(submit, file = "./submissions/titanicdtree.csv", row.names = FALSE)

