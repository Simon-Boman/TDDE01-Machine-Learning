library(kknn)
#1
#read in thee data and divide it into training set (50%), validation set (25%), and test sets (25%)
data=read.csv("optdigits.csv")

n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
traindata=data[id,] 

id1=setdiff(1:n, id)
set.seed(12345)
id2=sample(id1, floor(n*0.25))
validationdata=data[id2,] 

id3=setdiff(id1, id2)
testdata=data[id3,] 

#2
#fitting 30-NN classifier using the *training* set for training, and using the same *training* set for testing the models accuracy
kknnModelTrain <- kknn(formula=as.factor(traindata[,65]) ~ . ,  train=traindata, test=traindata, k=30, kernel="rectangular")
#the predicted classification from using the model to predict the classes of the training set
predictionsTrain <- kknnModelTrain[["fitted.values"]]

#Confusion matrix, original labels on the x-axis, predicted labels on the y-axis
ConfusionMatrixTrain = table(traindata[,65], predictionsTrain)
#plot(ConfusionMatrixTrain, xlab="true label", ylab="predicted label")
misclassificationsTrain = ( 1-sum(diag(ConfusionMatrixTrain))/length(traindata[,65]))


#fitting 30-NN classifier using the *training* set for training, and using the *test* set for testing the models accuracy
kknn_testdata <- kknn(formula=as.factor(traindata[,65]) ~ . ,  train=traindata, test=testdata, k=30, kernel="rectangular")
#the predicted classification from using the model to predict the classes of the test set
predictionsTest <- kknn_testdata[["fitted.values"]]

ConfusionMatrixTest = table(testdata[,65], predictionsTest)
#plot(ConfusionMatrixTest, xlab="true label", ylab="predicted label")
misclassificationTest = ( 1-sum(diag(ConfusionMatrixTest))/length(testdata[,65]))


#3
#extract class probabilities for each row, and extract the class probability for only class 8
classProbabilitiesTrain = kknnModelTrain[["prob"]]

predictionsTrain = kknnModelTrain[["fitted.values"]]

#since predictionsTrain is factored with levels, need to convert it to numeric and take -1 so 
#since level 1 corresponds to 0, level 2 to 1 etc.

#combine the class probabilities with indexes, and prediction and true label
probsClassify8 = cbind(classProbabilitiesTrain, index=c(1:nrow(traindata)), pred = as.numeric(predictionsTrain)-1, true = traindata[,65])

#remove rows where predicted value was not 8 or the true value was not 8, i.e. where we did not
#correctly predict an 8. 
probsClassify8 = subset(probsClassify8, probsClassify8[,12]==8 & probsClassify8[,13]==8)


#we can now easily check the probsClassify8-matrix and see that 
#the 2 easiest cases to classify are the numbers on row 34 and 55 of the training data
#and the 3 hardest are row 1368, 1572 and 77 in the training data 


#for plotting heatmap for a certain row of our data
heatmapPlot <- function(row) {
  bitmapdata = as.numeric(traindata[row,1:64])
  bitmapMatrix = matrix(bitmapdata, nrow=8, byrow=TRUE)
  heatmap(bitmapMatrix, Rowv = NA, Colv = NA, main = row)
}

heatmapPlot(34)
heatmapPlot(55)
heatmapPlot(1368)
heatmapPlot(1572)
heatmapPlot(77)


#4
#function for calculating the misclassification for a data set for k=1...30. parameter data is data to test against,
#i.e. can be traindata, validationdata, or testdata
calc_Missclassification_k_1_to_30 <- function(data) {
  missclassifications = numeric(30)
  for (i in 1:30) {
    kknn_model <- kknn(formula=as.factor(traindata[,65])~.,  train=traindata, test=data, k=i, kernel="rectangular")
    predictions <- kknn_model[["fitted.values"]]
    confusionMatrix = table(data[,65], predictions)
    missclass = ( 1-sum(diag(confusionMatrix))/length(data[,65]))
    missclassifications[i] = missclass
  }
  return(missclassifications)
}

kvalues = c(1:30)
missclassificationsTrain = calc_Missclassification_k_1_to_30(traindata)
plot(kvalues, missclassificationsTrain, col="blue", ylim=c(0,0.045), main = "Misclassification error for training set and validation set for k= 1..30",
     xlab="k-values", ylab="misclassification error" )

missclassificationsValidation = calc_Missclassification_k_1_to_30(validationdata)
points(kvalues, missclassificationsValidation, col="orange")

legend(0,0.045,legend=c("training set","validation set"), col=c("blue","orange"),pch=c("o","o"))


#### not part of the assignment ####
#below 2 lines just for testing if our assumtion of k=3 is optimal is reasonable by comparing with test data aswell.
missclassificationsTest = calc_Missclassification_k_1_to_30(testdata)
points(kvalues, missclassificationsTest, col="green")
#### not part of the assignment ####


#missclassifcation for test data when k=3. 
kknn_testdata <- kknn(formula=as.factor(traindata[,65])~.,  train=traindata, test=testdata, k=3, kernel="rectangular")
predictionsTest <- kknn_testdata[["fitted.values"]]
confusionMatrixTest = table(testdata[,65], predictionsTest)
missclassificationTestdata_k3 = ( 1-sum(diag(confusionMatrixTest))/length(testdata[,65]))


#5
#calculate cross entropy for k=1..30
crossEntroypValidation = numeric(30)
#loop over k=1,...,30
for (j in 1:30) {
  #set crossEntropy to 0 and fit new model using current k value, and extract the class probabilities
  crossEntropy = 0
  kknn_validationdata <- kknn(formula=as.factor(traindata[,65])~.,  train=traindata, test=validationdata, k=j, kernel="rectangular")
  classProbabilitiesValidation = kknn_validationdata[["prob"]]
  
  #for each row in our validation set, and for each class in our data, i.e. number 0 to 9 
  for (i in 1:nrow(validationdata)) { 
    for (m in 0:9) { 
      #if the actual class label of validationdata[i] == m
      #then calculate probability that row i of validation data belongs to class m
      if (validationdata[i,65] == m) { 
        probability_datai_classm = as.numeric(classProbabilitiesValidation[i,m+1]) 
        log_prob = log(probability_datai_classm+1e-15) 
        crossEntropy = crossEntropy + log_prob
      }
    }
  }
  #since we generally for computational purposes minimize the log likelihood
  crossEntroypValidation[j] = -crossEntropy 
}

plot(kvalues,crossEntroypValidation, type="b", col="red", main = "Cross-entropy for validation set for k= 1..30",
     xlab="k-values", ylab="cross-entropy" )
