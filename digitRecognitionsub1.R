library(caret)
library(foreach)
library(doMC)
registerDoMC(cores = 3)

train <- read.csv("train.csv", header=TRUE)
test <- read.csv("test.csv", header=TRUE)


labels <- train[,1]
labels <- foreach(label = labels, .combine=rbind) %dopar% {
    if(label==1)  label = c(1,0,0,0,0,0,0,0,0,0)
    else if(label==2)  label = c(0,1,0,0,0,0,0,0,0,0)
    else if(label==3)  label = c(0,0,1,0,0,0,0,0,0,0)
    else if(label==4)  label = c(0,0,0,1,0,0,0,0,0,0)
    else if(label==5)  label = c(0,0,0,0,1,0,0,0,0,0)
    else if(label==6)  label = c(0,0,0,0,0,1,0,0,0,0)
    else if(label==7)  label = c(0,0,0,0,0,0,1,0,0,0)
    else if(label==8)  label = c(0,0,0,0,0,0,0,1,0,0)
    else if(label==9)  label = c(0,0,0,0,0,0,0,0,1,0)
    else if(label==0)  label = c(0,0,0,0,0,0,0,0,0,1)

}
colnames(labels) = c("digit1","digit2","digit3","digit4","digit5","digit6","digit7","digit8","digit9","digit0")
train <- train[,-1]

myformula = as.formula(paste0(paste(colnames(labels),collapse="+"),'~','.'))
trainData = cbind(labels,train)

nnmodel.grid <- expand.grid(.size=c(400,50),.decay=c(0,0.01))
myTrainControl = trainControl(method="boot",number=4,allowParallel=TRUE)
nnmodel.fit <- train(myformula, data = trainData, method = "nnet", tuneGrid = nnmodel.grid, trControl = myTrainControl) 
predictions <- predict(nnmodel.fit, newdata = test)

write(predictions, file="nnet1.csv", ncolumns=1)