

## Question 1
library(caret)
library(plyr)
library(gbm)
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 
vowel.train$y <- factor(vowel.train$y)
vowel.test$y <- factor(vowel.test$y)
set.seed(33833)
modRF <- train( y ~ ., data = vowel.train, method = 'rf' )
modGBM <- train( y ~ ., data = vowel.train, method = 'gbm' )
predRF <- predict( modRF, vowel.test )
predGBM <- predict( modGBM, vowel.test )
confusionMatrix( predRF, vowel.test$y )
confusionMatrix( predGBM, vowel.test$y )

## Question 2

library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
set.seed(62433)

modRF <- train( diagnosis ~ ., data = training, method = 'rf' )
modGBM <- train( diagnosis ~ ., data = training, method = 'gbm' )
modLDA <- train( diagnosis ~ ., data = training, method = 'lda' )

predTrainRF <- predict( modRF, training )
predTrainGBM <- predict( modGBM, training )
predTrainLDA <- predict( modLDA, training )

combTrainData = data.frame(
  diagnosis = training$diagnosis,
  RF = predTrainRF,
  GBM = predTrainGBM,
  LDA = predTrainLDA
  )

modComb = train( diagnosis~., data = combTrainData, method = 'rf' )

predRF <- predict( modRF, testing )
predGBM <- predict( modGBM, testing )
predLDA <- predict( modLDA, testing )

combTestData = data.frame(
  RF = predRF,
  GBM = predGBM,
  LDA = predLDA
)

confRF <- confusionMatrix( predRF, testing$diagnosis )
confGBM <- confusionMatrix( predGBM, testing$diagnosis )
confLDA <- confusionMatrix( predLDA, testing$diagnosis )

predComb <- predict( modComb, combTestData )

confComb <- confusionMatrix( predComb, testing$diagnosis )

confRF$overall[1]
confGBM$overall[1]
confLDA$overall[1]
confComb$overall[1]
