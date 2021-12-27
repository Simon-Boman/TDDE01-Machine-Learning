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
classProbabilitiesTrain = kknnModelTrain[["prob"]]
class_8_ProbabilitiesTrain = classProbabilitiesTrain[,9]

#for each iteration, find the highest probability of all probabilities of classifying a 8, 
#where the true label was actually an 8 
easy8s_index_prob = matrix(0, nrow=2, ncol=2)
for (i in 1:2) { 
  for (j in 1:length(class_8_ProbabilitiesTrain)) {
    if (traindata[j,65] == 8 && class_8_ProbabilitiesTrain[j] > easy8s_index_prob[i,2]) {
      easy8s_index_prob[i,1] = j 
      easy8s_index_prob[i,2] = class_8_ProbabilitiesTrain[j]
    } 
  }
  class_8_ProbabilitiesTrain[ easy8s_index_prob[i,1] ] = 0 #needed so we dont keep saving the same element in all 2 iterations
}

#since I modified this vector above, I set it to its original value
class_8_ProbabilitiesTrain = classProbabilitiesTrain[,9]

#for each iteration, find the highest probability of all probabilities of classifying a 8, 
#where the true label was actually an 8 (but the prediction was not necessary an 8)
hard8s_index_prob = matrix(1, nrow=3, ncol=2)
for (i in 1:3) { 
  for (j in 1:length(class_8_ProbabilitiesTrain)) {
    if (traindata[j,65] == 8 && class_8_ProbabilitiesTrain[j] < hard8s_index_prob[i,2]) {
      hard8s_index_prob[i,1] = j 
      hard8s_index_prob[i,2] = class_8_ProbabilitiesTrain[j]
    } 
  }
  class_8_ProbabilitiesTrain[ hard8s_index_prob[i,1] ] = 1 #needed so we dont keep saving the same element in all 3 iterations
}


#for plotting heatmap for a certain row of our data
heatmapPlot <- function(row) {
  bitmapdata = as.numeric(traindata[row,1:64])
  bitmapMatrix = matrix(bitmapdata, nrow=8, byrow=TRUE)
  heatmap(bitmapMatrix, Rowv = NA, Colv = NA, main = row)
}

heatmapPlot(34)
heatmapPlot(55)
heatmapPlot(1624)
heatmapPlot(1663)
heatmapPlot(1873)


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

legend(0,0.045,legend=c("training set","validation set"), col=c("blue","orange"),
       pch=c("o","o"))


#### not part of the assignment ####
#below 2 lines just for testing if our assumtion of k=3 is optimal is reasonable by comparing with test data aswell.
#missclassificationsTest = calc_Missclassification_k_1_to_30(testdata)
#points(kvalues, missclassificationsTest, col="green")
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
  
  #for each row in our validation set, and for each class in our data, zi.e. number 0 to 9 
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

plot(kvalues,crossEntroypValidation, col="red", main = "Cross-entropy for validation set for k= 1..30",
     xlab="k-values", ylab="cross-entropy" )
