df <- read.csv("cars.csv")
colnames(df) <- c("Names","Speed","Distance")
str(df) 
is.na(df)
set.seed(123)
trainsize <- round(nrow(df)*0.7)
testsize <- nrow(df) - trainsize
train_indices <- sample(seq_len(nrow(df)),trainsize)
trainset <- df[train_indices,]
testset <- df[-train_indices,]
cars_model <- lm(Distance ~ Speed, trainset)
summary(cars_model)
prediction <- predict(cars_model,testset)
