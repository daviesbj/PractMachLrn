

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

## Question 3

set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
librray( elasticnet )
set.seed(233)
modLAS <- enet( as.matrix(training[,1:8]), as.matrix(training[,9]), trace = TRUE)
plot( modLAS, xvar = 'penalty' )

## Question 4
# theURL = "https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv"
# download.file( url = theURL, destfile = basename( theURL ), method = 'curl' )
library(lubridate)  # For year() function below
dat = read.csv( "gaData.csv" )
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)
library(forecast)
modTS = bats(tstrain)
predTS <- forecast( modTS, h=235 )
plot(predTS)
1-sum(( testing$visitsTumblr > predTS$upper[,2] )|( testing$visitsTumblr < predTS$lower[,2] ))/length(testing$visitsTumblr)

## Question 5
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
set.seed(325)
modSVM <- svm( CompressiveStrength~., data = training )
prdSVM <- predict( modSVM, testing )
sqrt(mean((prdSVM - testing$CompressiveStrength)^2))
