IrisDataset <- read.csv("iris.csv")
IrisDataset <- IrisDataset[1:150,2:6]

attributes(IrisDataset)


summary(IrisDataset) 

str(IrisDataset)

names(IrisDataset)

hist(IrisDataset$Species)

plot(IrisDataset$Sepal.Length)
     
# qqnorm(IrisDataset)
     
IrisDataset$Species<- as.numeric(IrisDataset$Species) 
     
set.seed(123)
     
trainSize <- round(nrow(IrisDataset) * 0.8)
     
testSize <- nrow(IrisDataset) - trainSize
training_indices<-sample(seq_len(nrow(IrisDataset)),size =trainSize)

trainset <- IrisDataset[training_indices, ]
     
testSet <- IrisDataset[-training_indices, ]
     
     # set.seed(405)
     # 
     # trainSet <- IrisDataset[training_indices, ]
     # 
     # testSet <- IrisDataset[-training_indices, ]
     
LinearModel<- lm(Petal.Width ~ Petal.Length, trainset)
     
summary(LinearModel)
     
prediction<-predict(LinearModel, testSet)
     
prediction