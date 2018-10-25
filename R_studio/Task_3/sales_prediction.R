


setwd("C:/Users/Ruthesan-PC/Desktop/Ubiqum_class/R_studio/Task_3")

existingproducts <- read.csv("existing_Age_Professional_Competitors.csv", sep = ";", dec = ",")
newproducts <- read.csv("newproduct_Age_Professional_Competitors.csv", sep = ";", dec = ",")   

newproducts$X <- NULL
existingproducts$X <- NULL

str(existingproducts)

#change factors into numeric

exproducts <- existingproducts[,-12] 

newDataframe <- dummyVars(" ~ .", data = exproducts) 

readyData <- data.frame(predict(newDataframe, newdata = exproducts))

corrData <- cor(readyData)
corrplot(corrData)

library(outliers)
library(ggpubr)
ggboxplot(data= exproducts, 
          x='Product_type',
          y='Volume',
          fill= 'grey') + 
  theme(axis.text.x = element_text(angle = 90))

library(Hmisc)

r_p <- rcorr(as.matrix(readyData))

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

result <- flattenCorrMatrix(r_p$r, r_p$P)

result[,3] <- round(result[,3],3)

result_1 <- subset(result, result$cor > 0)

###way to find row names of outliers

boxplot(existingproducts$X2Stars)$out
which(existingproducts$X2Stars %in% c(370,104,154))

##Remove rows having outliers

readyData <- readyData[-c(2,18,23,48,50,73,109),]
readyData$X5Stars <- NULL
readyData$X3Stars <- NULL
readyData$X1Stars <- NULL

##Machine Learning

set.seed(431)

inTraing <- createDataPartition(readyData$Volume, p=0.75, list = F)
training <- readyData[inTraing,]
testing <- readyData[-inTraing,]

##Linear Regression 

lmFit <- train(
  Volume ~ .,
  data = training,
  method = "lm"
  )

summary(lmFit)

lm_pred <- predict(lmFit, testing)

postResample(pred = lm_pred, obs = testing$Volume)


##SVM Linear regression

set.seed(134)

inTraing <- createDataPartition(readyData$Volume, p=0.75, list = F)
training <- readyData[inTraing,]
testing <- readyData[-inTraing,]

svmFit <- train(
  Volume ~ .,
  data = training,
  method = "svmLinear"
  )

summary(svmFit)

svm_pred <- predict(svmFit, testing)

postResample(pred = svm_pred, obs = testing$Volume)



##SVM Polynomial Regression######

set.seed(44)

inTraing <- createDataPartition(readyData$Volume, p=0.75, list = F)
training <- readyData[inTraing,]
testing <- readyData[-inTraing,]

svmFit <- train(
  Volume ~ .,
  data = training,
  method = "svmRadial"
)

summary(svmFit)

svm_pred <- predict(svmFit, testing)

postResample(pred = svm_pred, obs = testing$Volume)

##SVM Radial Basis Function Regression

set.seed(33)

inTraing <- createDataPartition(readyData$Volume, p=0.75, list = F)
training <- readyData[inTraing,]
testing <- readyData[-inTraing,]

rfFit <- train(
  Volume ~ .,
  data = training,
  method = "ranger",
  min.node.size = 2
  )

summary(rfFit)

rf_pred <- predict(rfFit, testing)

postResample(pred = rf_pred, obs = testing$Volume)


##Random Forest Regression

set.seed(33)

inTraing <- createDataPartition(readyData$Volume, p=0.75, list = F)
training <- readyData[inTraing,]
testing <- readyData[-inTraing,]

rfFit <- train(
  Volume ~ .,
  data = training,
  method = "ranger",
  min.node.size = 2,
  splitrule = "maxstat"
 )

summary(rfFit)

rf_pred <- predict(rfFit, testing)

postResample(pred = rf_pred, obs = testing$Volume)


##Gradient Boosting Machines Regression

set.seed(91)

inTraing <- createDataPartition(readyData$Volume, p=0.75, list = F)
training <- readyData[inTraing,]
testing <- readyData[-inTraing,]

gbmFit <- train(
  Volume ~ .,
  data = training,
  method = 'xgbTree'
)

summary(gbmFit)

gbm_pred <- predict(gbmFit, testing)

postResample(pred = gbm_pred, obs = testing$Volume)


#######################################

nproducts <- newproducts
nproducts[,c(4,6,8,12)] <- NULL

newDataframe1 <- dummyVars(" ~ .", data = nproducts) 
nproducts1 <- data.frame(predict(newDataframe1, newdata = nproducts))

colnames(nproducts1)[c(3,4,9)] <- colnames(training)[c(3,4,9)]

rf_nprod_pred <- predict(rfFit, nproducts1)


########################################












