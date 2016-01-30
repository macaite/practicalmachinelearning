download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv","data/pml-training.csv")
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv","data/pml-testing.csv")

rm(list = ls())
library(caret);library(randomForest);library(rpart)
set.seed(1234)

xtraining = read.csv("data/pml-training.csv")

training = read.csv("data/pml-training.csv",na.strings = c("NA","#DIV/0!",""))
test = read.csv("data/pml-testing.csv",na.strings = c("NA","#DIV/0!",""))
#str(training);summary(training)

#remove data not needed
training = training[,-c(1:7)]

#remove all columns with only NA
training = training[,colSums(is.na(training))==0]

#createTrain and Test data sets

inTrain = createDataPartition(training$classe,p=0.6)[[1]]
trainingSet = training[inTrain,]
testingSet = training[-inTrain,]

modrpart = train(classe ~ .,data = trainingSet, method = "rpart" )
modrf = train(classe ~ .,data = trainingSet, method = "rf" )

rpart.results = predict(modrpart,testingSet)
rf.results = predict(modrf,testingSet)

confusionMatrix(testingSet$classe,rpart.results)
confusionMatrix(testingSet$classe,rf.results)

predict(modrf,test)

#rfcv(trainingSet[,1:ncol(trainingSet)-1],trainingSet$classe,cv.fold = 3)
#Sys.time()
#predict(modrf,test)
#[1] B A B A A E D B A A B C B A E E A B B B
#Levels: A B C D E

Sys.time()
library(parallel)
library(doParallel)
cluster = makeCluster(detectCores()-1)
registerDoParallel(cluster)
fitControl = trainControl(method = "cv", number = 10, allowParallel = TRUE)
modrf_p = train(classe ~ .,data = trainingSet, method = "rf", trControl = fitControl)
stopCluster(cluster)
Sys.time()
