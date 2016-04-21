## Machine Learning Assignment 
Author: "R. De Gouveia"

### Introduction.
The main goal of this project is to predict how well people do Weight Lifting Exercise. 6 participants were asked to perform barbell fits correctly and incorrectly. In the following data set you'll find a variable called "classe". 

Class A: Exactly according to the specification.
Class B, C, D and E weren't according to the specification. 
More information is available at: http://groupware.les.inf.puc-rio.br/har 

The participants were wearing sensors in different part of their body. These sensors took measurements during the exercise. You'll find these measurements in the data set. The idea is to predict "classe" from thoses measurements in the testing data set.

### Loading the data & packages.
First I load the data & the packages necessary for the analysis.

Data
```{r}
set.seed(1234)
url1<-"http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
download.file(url1,destfile="pml-training.csv")

url2<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(url2,destfile="pml-testing.csv")

if(file.exists("pml-training.csv") && file.exists("pml-testing.csv"))
  {
  training<-read.csv("pml-training.csv",na.strings=c("#DIV/0!","","NA"),header=T)
  testing<-read.csv("pml-testing.csv",na.strings=c("#DIV/0!",""),header=T)
  }else 
    {
      stop("One or both files do not exist. Please download the files manually")
    }
```

Pml-training.csv will be the data for training purpose. Pml-testing.csv will be the data for testing purpose. 

Packages
```{r}
library(caret)
library(rpart)
library(randomForest)
```

### Cleaning the data & basic preprocessing.
There are some variables that seems to be not relevant for the study as: X, "user_name", "raw_timestamp_part_1","raw_timestamp_part_2","cvtd_timestamp", "new_window" and "num_window" . I exclude these variables from the data set.

Some variables have an important number of NA. In most of the cases the % of NA is > 90%, I exclude these variables from the data set.

Finally I remove near zero covariates. 

pml_training
```{r}
training<-training[,8:160]
NaPercentage<-sapply(training, function(x) sum(is.na(x)/nrow(training)))
notDataNames<-names(NaPercentage[which(NaPercentage>0.9)])
for(i in 1:length(notDataNames))
{
  columnName<-notDataNames[i]
  training[,which(names(training)==columnName)]<-NULL
}
nsv<-nearZeroVar(training,saveMetrics=T)
notDataNames<-rownames(nsv[which(nsv$nzv==T),])
i<-NULL
for(i in 1:length(notDataNames))
{
  columnName<-notDataNames[i]
  training[,which(names(training)==columnName)]<-NULL
}
```

Removing the same variables from pml_testing
```{r}
testing<-testing[, names(testing) %in% names(training)]
```

### Cross-validation.
I split training in two data sets. One of them called "validation". It's going to be used to test the model.
```{r}
inTrain<-createDataPartition(y=training$classe,p=0.6,list=F)
trainingData<-training[inTrain,]
validation<-training[-inTrain,]
```

### Preprocess the data.
There migth be some correlation between variables. 
```{r}
M<-abs(cor(trainingData[,-53]))
diag(M)<-0
nrow(which(M>0.8,arr.ind=T))
```

There are `r nrow(which(M>0.8,arr.ind=T))` variables correlated to each other. I apply PCA.

```{r}
preProc<-preProcess(trainingData[,-53],method="pca",thresh = 0.95)
trainPC<-predict(preProc,trainingData[,-53])
```
  
### Machine learning model.
I do two models as follows:
```{r}
model1<-train(trainingData$classe~.,method="rf", ntree=500, data=trainPC, tuneGrid=data.frame(.mtry = 3))
model2<-train(trainingData$classe~.,method="rpart",data=trainPC)
```

### Accuracy. 
I validate the models using the "validation" data set.

```{r}
pred1<-predict(preProc,validation[,-53])
confusionMatrix(validation$classe,predict(model1,pred1))
confusionMatrix(validation$classe,predict(model1,pred1))$overall[1]
```

```{r, results='hide'}
pred2<-predict(preProc,validation[,-53])
confusionMatrix(validation$classe,predict(model2,pred2))
confusionMatrix(validation$classe,predict(model2,pred2))$overall[1]
```

I choose the model1 which have an accuracy of `r confusionMatrix(validation$classe,predict(model1,pred1))$overall[2]` vs model2 : `r confusionMatrix(validation$classe,predict(model2,pred2))$overall[1]`

### Test model on "Testing" data set.
I use model1 to predict the classe from "testing data set".

```{r}
testPC<-predict(preProc,testing)
predTesting<-predict(model1,testPC)
predTesting
```

