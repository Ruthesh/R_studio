


setwd("C:/Users/Ruthesan-PC/Desktop/Ubiqum_class/R_studio/Task_2")

Incomplete <- read.csv("SurveyIncomplete.csv")
Complete <- readxl::read_excel(path="Survey_Key_and_Complete_Responses_excel.xlsx", sheet = 2)

Complete$elevel <- as.factor(Complete$elevel)
Complete$car <- as.factor(Complete$car)
Complete$zipcode <- as.factor(Complete$zipcode)
Complete$brand <- as.factor(Complete$brand)
Complete$age<- as.factor(findInterval(Complete$age, c(20,30,40,50,60,70,80)))

set.seed(123)

inTrain <- createDataPartition(
  y = Complete$brand,
  ## the outcome data are needed
  p = .75,
  ## The percentage of data in the
  ## training set
  list = FALSE)
str(inTrain)
training <- Complete[ inTrain,]
testing  <- Complete[-inTrain,]
nrow(training)
nrow(testing)

# ctrl <- trainControl(method = "repeatedcv", repeats = 3)
# Model <- train(
#   brand ~ .,
#   data = training,
#   method = "pls",
#   ## Center and scale the predictors for the training
#   ## set and all future samples.
#   preProc = c("center", "scale") 
#   tuneLength = 15,
#   ## added:
#   trControl = ctrl   )

Model <-  train(training[,1:6], training[,7],
                method = "knn",
                preProcess = c("center", "scale"),
                tuneLength = 10,
                trControl = trainControl(method = "cv"))

brand_predict <- predict(Model, newdata = testing)

