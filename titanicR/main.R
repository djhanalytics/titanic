
install.packages("mice")
require(rpart)
library(mice)

generate_pred <- function(dta, filename, prn = T){
  model.train <- rpart(
    Survived ~ Sex + Age + SibSp + Fare + Embarked + Pclass + Parch,
    method = "class",
    data = dta,
    control = rpart.control(minsplit = 2, cp = 0)
  )
  
  cptables = model.train$cptable
  cprow <- cptables[cptables[,4] == min(cptables[,4]),]
  
  if(prn){
    opt <- which.min(model.train$cptable[,"xerror"])
    cp <- model.train$cptable[opt, "CP"]
    fit = prune(model.train, cp=cp)
  }else{
    fit = model.train 
  }
 
  printcp(fit)
  plotcp(fit)
  
  par(mfrow = c(1, 1), mar = c(1, 1, 1, 1))
  plot(fit, uniform = T, compress = T, 
       margin = 0.1, branch = 0.3)
  text(fit, use.n = T, digits = 3, cex = 0.6)
  
  Prediction <- predict(fit, test, type = "class")
  # Prediction
  submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
  # submit
  write.csv(submit, file = paste("./submissions/titanic", filename, ".csv", sep=""), row.names = FALSE)
  
  
}

train <- read.csv('./data/train.csv')
test <- read.csv('./data/test.csv')
train.imputed.mean <- train
train.imputed.median <- train
train.imputed.mode <- train

md.pattern(train)
generate_pred(train, "pruned")

# TODO: Before I do anything else, I should really deal with the missing data.

# What can we do?
# 1. Delete observations with missing data
# train[complete.cases(train),]
md.pattern(train[complete.cases(train),])
generate_pred(train[complete.cases(train),], "deleted")

# 2. Impute Mean/Median/Mode
# impute(train$Age, mean)
train.imputed.mean$Age[is.na(train$Age)] <- mean(train$Age, na.rm = T)
generate_pred(train.imputed.mean, "mean")
# impute(train$Age, median)
train.imputed.median$Age[is.na(train$Age)] <- median(train$Age, na.rm = T)
generate_pred(train.imputed.median, "median")
# impute(train$Age, mode)
train.imputed.mode$Age[is.na(train$Age)] <- mode(train$Age)

# 3. Predict
# This involves predicting the Age (in this case) using rpart





