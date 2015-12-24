
#Practical Machine Learning project - Writeup
# Tesfaye Belay
# November 2015

library(caret)
require(randomForest)
library(ggplot2) 


# Background
# 
# 
# Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity 
# relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements 
# about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that 
# people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project,
# your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform 
# barbell lifts correctly and incorrectly in 5 different ways. More information is available from the 
# website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset). 




# For the purpose of repeatablility, lets set a seed

set.seed(1001)

# reading the files

trainingdata<- read.csv("D:/My_Database/Coursera/coursera/Practical_Machine_Learning/Signature_track/pml-training.csv", na.strings=c("NA",""))
testingdata<- read.csv("D:/My_Database/Coursera/coursera/Practical_Machine_Learning/Signature_track/pml-testing.csv", na.strings=c("NA",""))

# Doing some exploratory analysis 

str(trainingdata)
summary(trainingdata)

# Ploting seems a bid challenging for this dataset has queit a number of variables. Just to see an example

pairs(trdata[,grep("roll", names(trdata))])

# The file has 19622 rows and 160 variables. Quite big dataset. It seems to have lots of 'NA' in the data. Let us investigate the
# number of rows which are 'NA'

apply(is.na(trainingdata),2,sum)

# some of the variables have 19,216 rows of NA, i.e about 98% or the rows contain 'NA'. I think these variables will not help much in the 
# building of the model, and thus decided to drop them


important_fields<-apply(!is.na(trainingdata),2,sum)>19216

# so the training and testing datsets in the following assignment contains the important fields only

trdata<-trainingdata[,important_fields]

testdata<-testingdata[,important_fields]

#The first 7 columns seem unimportant for the model building

trdata<-trdata[,8:60]
tstdata<-testdata[,8:60]

#I sub split further the training set into training and testing set for the purpose of Cross validation and estimation of the out of sample error

InTrain<-createDataPartition(y=trdata$classe,p=0.75,list=FALSE)
tr1<-trdata[InTrain,]
tst1<-trdata[-InTrain,]

#So I used caret with random forest as my model with 5 fold cross validation. The data set need preprocessing as well

rf_model<-train(classe~.,data=tr1,method="rf", preProcess="pca")

# rf_model<-train(classe~.,data=tr1,method="rf", 
#                 preProcess="pca",
#                 trControl=trainControl(method="cv",number=3, p=0.75),
#                 prox=TRUE,allowParallel=TRUE)



# the model performance is seen in the following out puts

print(rf_model)
plot(rf_model)
print(rf_model$finalModel)

# Now let's see how the model performs on the tst1 data

answer0<-predict(rf_model, newdata=tst1)

confusionMatrix(answer0,tst1$class)

#So the out of sample error estimate is 1-Accuracy displayed in the output of the confusionMatrix


#The performance of the model on the test data is performed as follows


# The actual performace on the model on the test data reserved for the excercise is undertaken belop
answer<-predict(rf_model, newdata = tstdata)

#The pridiction of the model found 17 out of 20 test data correctly. Thus, the out of sample error in this case is 3/20 
# which equals to .15

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(answer)

