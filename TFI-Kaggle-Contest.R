setwd("E:/Sharan/Hobby Work/Kaggle/Restaurant-revenue-prediction Image/ernest")
# Reading Data
train <- read.csv('train.csv')
test <- read.csv('test.csv')

# Replacing categorical variables with numbers
train$ty [train$Type == 'DT'] <- 1
train$ty [train$Type == 'FC'] <- 2
train$ty [train$Type == 'IL'] <- 3
# try MB to IL
test$ty [test$Type == 'DT'] <- 1
test$ty [test$Type == 'FC'] <- 2
test$ty [test$Type == 'IL'] <- 3
test$ty [test$Type == 'MB'] <- 2

train$cg [train$City.Group == 'Big Cities'] <- 1
train$cg [train$City.Group == 'Other'] <- 2

test$cg [test$City.Group == 'Big Cities'] <- 1
test$cg [test$City.Group == 'Other'] <- 2
# Modifying data to match the test with train dataset
test$P1 [test$P1==15] <- 12
test$P2 [test$P2==1.5] <- 2
test$P4 [test$P4==2] <- 3
test$P7 [test$P7==6] <- 5
test$P9 [test$P9==6] <- 5
test$P15 [test$P15==6] <- 5
test$P16 [test$P16==6] <- 5
test$P17 [test$P17==12] <- 15
test$P18 [test$P18==2] <- 3
test$P18 [test$P18==15] <- 12
test$P21 [test$P21==12] <- 15
test$P25 [test$P25==6] <- 5
test$P27 [test$P27==7.5] <- 5
test$P29 [test$P29==10] <- 7.5
test$P30 [test$P30==2] <- 3
test$P33 [test$P33==1] <- 0
test$P34 [test$P34==1] <- 0
test$P34 [test$P34==6] <- 5
test$P34 [test$P34==30] <- 24
test$P36 [test$P36==1] <- 0
test$P36 [test$P36==8] <- 5

date <- as.Date('01/01/2015',"%m/%d/%Y")
train$Open.Date <- as.Date(train$Open.Date,"%m/%d/%Y")
train$days <- date - train$Open.Date
test$Open.Date <- as.Date(test$Open.Date,"%m/%d/%Y")
test$days <- date - test$Open.Date
train$days <- as.numeric(train$days)
test$days <- as.numeric(test$days)
test$days[test$days >6812] <- 6812

train$ty <- as.factor(train$ty)
train$cg <- as.factor(train$cg)

test$ty <- as.factor(test$ty)
test$cg <- as.factor(test$cg)

write.csv(train,file='trainclean.csv',row.names=FALSE)
write.csv(test,file='testclean.csv',row.names=FALSE)

###Checking
head(train)
head(test)
nrow(test)
summary(train$revenue)
cutoff<-quantile(train$revenue, c(.75))
cutoff
head(train1)

##### Extreme values in the data

train <- read.csv('trainclean.csv')
train$rev <- 0
train$rev[train$revenue > 9000000] <- 1



a <- c(1,6:42,44:47)
train1 <- train[,a]
submit1 <- data.frame(Id=train1$Id,rev=train1$rev)
write.csv(submit1,'train3003.csv',row.names=FALSE)
train1$rev <- as.factor(train1$rev)
# Predicting the extreme values and hence use in the model for the final prediction
require(randomForest)
set.seed(100)
fit1 <- randomForest(rev ~ . -Id,data =train1,importance=TRUE, ntree=5000,mtry=5,replace=FALSE)
test1 <- read.csv('testclean.csv')
prediction <- predict(fit1,test1)
submit <- data.frame(Id = test1$Id, rev = prediction)
write.csv(submit,'test3003.csv',row.names=FALSE)

train <- read.csv('trainclean.csv')
test <- read.csv('testclean.csv')
submit1 <- read.csv('train3003.csv')
submit <- read.csv('test3003.csv')
train <- merge(x=train, y=submit1, by ='Id', all=TRUE)
test <- merge(x=test, y=submit, by ='Id', all=TRUE)

train$ty <- as.factor(train$ty)
train$cg <- as.factor(train$cg)
train$rev <- as.factor(train$rev)

test$ty <- as.factor(test$ty)
test$cg <- as.factor(test$cg)
test$rev <- as.factor(test$rev)


## Remove highly correlated columns
library(caret)
names(train1)
tooHigh <- findCorrelation(cor(rbind(train[,6:42],test[,6:42])), .53)
tooHigh
train<-train[,-tooHigh]
test<-test[,-tooHigh]

# Final Prediction by random forest as well as svm
require(randomForest)
set.seed(100)
fit <- randomForest(revenue ~ . -Id -Open.Date -City -City.Group -Type,data=train, importance=TRUE, ntree=5000,mtry=5,replace=FALSE)
prediction <- predict(fit, test)
submit <- data.frame (Id = test$Id, Prediction = prediction)
write.csv(submit,file="RF0304-01-rf.csv", row.names =FALSE)

#### SVM
library(e1071)
fitsvm <- svm(revenue ~ . -Id -Open.Date -City -City.Group -Type,data=train, type="eps-regression", scale=TRUE)
prediction <- predict(fitsvm, test)
submitsvm <- data.frame (Id = test$Id, Prediction = prediction)
write.csv(submitsvm,file="RF0304-01svm.csv", row.names =FALSE)


### Cross validation to check accuracy

train1 <- train
train1$cv <- 0
set.seed(375)
cv <- sample(1:137,120,replace=F)
for (i in cv) {
  train1$cv[i] <- 1
}

train2 <- train1[train1$cv==1,]
train3 <- train1[train1$cv==0,]
require(randomForest)
set.seed(100)
# Choose any one
fit <- randomForest(revenue ~ . -Id -Open.Date -City -City.Group -Type,data=train2, importance=TRUE, ntree=5000,mtry=5,replace=FALSE)
fit <- svm(revenue ~ . -Id -Open.Date -City -City.Group -Type,data=train, cost=10, type="eps-regression", scale=TRUE)

print(fit)
prediction <- predict(fit,train3)
train3$revenue1 <- prediction
train3$devsq <- (train3$revenue - train3$revenue1)^2
mean <- mean(train3$devsq)
rmse <- sqrt(mean)
print(rmse)

####
new <- cbind(train3$revenue, train3$revenue1)
head(new, 20)
names(train)
