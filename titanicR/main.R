
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

# 2. Impute Mean/Median
# impute(train$Age, mean)
train.imputed.mean$Age[is.na(train$Age)] <- mean(train$Age, na.rm = T)
generate_pred(train.imputed.mean, "mean")
# impute(train$Age, median)
train.imputed.median$Age[is.na(train$Age)] <- median(train$Age, na.rm = T)
generate_pred(train.imputed.median, "median")

# 3. Linear Regression
train.correlate <- train
nums <- unlist(lapply(train.correlate, is.numeric))
cor(train[nums], use="complete.obs")
symnum(cor(train[nums], use="complete.obs"))
#this doesn't seem super correlated with anything.

# 4. kNN
install.packages("VIM")
library(VIM)

train.knn <- kNN(train)
train.knn <- subset(train.knn, select=c(1:12))
generate_pred(train.knn, "knn")

# 5. Predict the ages with mice?
library(mice)
# No iteration. But I want to get Predictor-Matrix
init = mice(train, maxit=0) 
predM = init$predictorMatrix
# Do not use following columns to impute values in 'Age'. Use the rest.
predM[, c("PassengerId", "Name","Ticket","Cabin")]=0    
imp<-mice(train, m=5, predictorMatrix = predM)
# Get the final data-frame with imputed values filled in 'Age'
train.mice <- complete(imp)
View(train.mice)
md.pattern(train.mice)
generate_pred(train.mice, "mice")

# 6. Predict age with rpart?

train.model.age <- rpart(
  Age ~ Fare + Sex
)

