# Question 1
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
training <- segmentationOriginal[segmentationOriginal$Case =='Train', ]
testing <- segmentationOriginal[segmentationOriginal$Case =='Test', ]
set.seed(125)
mod <- train(Class~., method='rpart',data=training)
print(mod$finalModel)
library(rattle)
fancyRpartPlot(mod$finalModel)

# Question 3
data(olive)
olive <- olive[,-1]
mod <- train(Area~., method="rpart",data=olive)
print(mod$finalModel)
plot(mod$finalModel,uniform=TRUE)
text(mod$finalModel,use.n=TRUE, all=TRUE,cex=.5)

newdata=as.data.frame(t(colMeans(olive)))
predict(mod,newdata)

# Question 4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
set.seed(13234)
mod <- train(chd ~ age+alcohol+obesity+tobacco+typea+ldl, method="glm",family="binomial",data=trainSA)
missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
missClass(testSA$chd,predict(mod,testSA))
missClass(trainSA$chd,predict(mod,trainSA))

# Question 5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
vowel.train$y = factor(vowel.train$y)
vowel.test$y = factor(vowel.test$y)
set.seed(33833)
mod <- randomForest(y ~., data=vowel.train)
varImp(mod)
