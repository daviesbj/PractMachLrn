## Question 1

library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
segTest <- subset(segmentationOriginal, Case == 'Test' )[,3:119]
segTrain <- subset(segmentationOriginal, Case == 'Train' )[,3:119]
set.seed(125)
segModel <- train( Class ~ ., data = segTrain, method = 'rpart' )
testSet <- data.frame(
  TotalIntenCh2 = c( 23000, 50000, 57000, NA ),
  FiberWidthCh1 = c( 10, 10, 8, 8 ),
  PerimStatusCh1 = c( 2, NA, NA, 2 ),
  VarIntenCh4 = c( NA, 100, 100, 100 )
  )

## Question 3

library(pgmm)
data(olive)
olive = olive[,-1]
modTree <- train( Area ~ . , data = olive, method = "rpart" )
newdata = as.data.frame(t(colMeans(olive)))
newArea <- predict( modTree, newdata )

## Question 4

library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
set.seed(13234)
glmMod <- train( chd ~ age + alcohol + obesity + tobacco + typea + ldl,
               data = trainSA, family = 'binomial', method = 'glm'
                 )
glmReg <- glm (chd ~ age + alcohol + obesity + tobacco + typea + ldl,
               data = trainSA, family = 'binomial'
               )
trainPred <- predict( glmMod, trainSA )
testPred <- predict( glmMod, testSA )
missClass <- function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
missClass( trainSA$chd, trainPred )
missClass( testSA$chd, testPred )

## Question 5

library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 
modRF <- train( y ~ ., method = 'rf', data = vowel.train )

