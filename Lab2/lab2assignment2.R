library(tree)
library(partykit)

#stringsAsFactors = TRUE to convert strings to factors!
data=read.csv2("bank-full.csv", stringsAsFactors = TRUE)
#csv - Reads a file in table format and creates a data frame from it, with cases corresponding to lines and variables to fields in the file.
#csv2 - (read.csv2) the variant used in countries that use a comma as decimal point and a semicolon as field separator. 

data = subset(data, select=-duration) #same asdata = data[,-12]
n=dim(data)[1] #same as nrow(data), i.e. how many observations
set.seed(12345)
id=sample(1:n, floor(n*0.4))
traindata=data[id,] 
id1=setdiff(1:n, id)
set.seed(12345)
id2=sample(id1, floor(n*0.3))
validationdata=data[id2,] 
id3=setdiff(id1, id2)
testdata=data[id3,] 


#2a
#tree.control default: function (nobs, mincut = 5, minsize = 10, mindev = 0.01)
#n observations in training set, minimum nr of observations to include in either child node, 
#smallest allowed node size, 
#The within-node deviance must be at least this times that of the root node for the node to be split.
defaultTree = tree(y ~., data=traindata)
plot(defaultTree)
text(defaultTree, pretty=0)
defaultTree
summary(defaultTree)

#2b
#tree.control
nodeSizeTree = tree(y ~. ,data=traindata, control=tree.control(nrow(traindata), mincut = 5, minsize = 7000, mindev = 0.01))
plot(nodeSizeTree)
text(nodeSizeTree, pretty=0)
nodeSizeTree
summary(nodeSizeTree)

#2c
devianceTree = tree(y ~., data=traindata, control=tree.control(nrow(traindata), mincut = 5, minsize = 10, mindev = 0.0005))
plot(devianceTree)
devianceTree
#summary(devianceTree)


#can access the misclassification rate from summary(defaultTree)
misclassDefaultTrain = summary(defaultTree)[["misclass"]][1]/summary(defaultTree)[["misclass"]][2]
defaultPred = predict(defaultTree, newdata=validationdata, type="class")
confusionMatrixDefault = table(validationdata$y, defaultPred)
misclassDefaultValidation = (1-sum(diag(confusionMatrixDefault))/length(validationdata$y))

misclassNodeSizetTrain = summary(nodeSizeTree)[["misclass"]][1]/summary(nodeSizeTree)[["misclass"]][2]
nodeSizePred = predict(nodeSizeTree, newdata=validationdata, type="class")
confusionMatrixNodeSize = table(validationdata$y, nodeSizePred)
misclassNodeSizetValidation = (1-sum(diag(confusionMatrixNodeSize))/length(validationdata$y))

misclassDevianceTrain = summary(devianceTree)[["misclass"]][1]/summary(devianceTree)[["misclass"]][2]
deviancePred = predict(devianceTree, newdata=validationdata, type="class")
confusionMatrixDeviance = table(validationdata$y, deviancePred)
misclassDeviancetValidation = (1-sum(diag(confusionMatrixDeviance))/length(validationdata$y))

misclassDefaultTrain
misclassNodeSizetTrain
misclassDevianceTrain
misclassDefaultValidation
misclassNodeSizetValidation
misclassDeviancetValidation

#z<- as.party(defaultTree)
#plot(z)


#3
trainScore = rep(0,50)
validationScore = rep(0,50)

#?prune.tree
#Determines a nested sequence of subtrees of the supplied tree by recursively "snipping" off the least important splits.
#best = number of leaves. i=2 we just do 1 split, root + 2 leaves. 
#If best is supplied, a tree object of size best is returned.
#can send in loss matrix here! default is 0-1 loss with 0 on diagonal. 

for(i in 2:50) {
  prunedTree = prune.tree(devianceTree, best=i)
  predValidation=predict(prunedTree, newdata=validationdata, type="tree")
  trainScore[i] = deviance(prunedTree)
  validationScore[i] = deviance(predValidation)
  #?deviance - Returns the deviance of a fitted model object.
  #In statistics, deviance is a goodness-of-fit statistic for a statistical model
  #It is a generalization of the idea of using the sum of squares of residuals (RSS)
  #in ordinary least squares to cases where model-fitting is achieved by maximum likelihood. 
}

plot(2:50, trainScore[2:50]/nrow(traindata), type="b", col="red", xlab="number of leaves", ylab="deviance",
     main="deviance for train(red) and validation(blue) data for different number of leaves")
points(2:50, validationScore[2:50]/nrow(validationdata), type="b", col="blue")

validationScore[1] = Inf #so which.min doesnt pick the first which is 0
optLeaves = which.min(validationScore)  #22 optimal number of leaves

optLeavesTree = prune.tree(devianceTree, best=22)
plot(optLeavesTree)
text(optLeavesTree, pretty=0)
plot(optLeavesTree)
optLeavesTree
summary(optLeavesTree)


#4
pred = predict(optLeavesTree, newdata=testdata, type="class")

confusionMatrix = table(true = testdata$y, pred = pred) #true yes/no-label on y-axis, predicted yes/no on x-axis
#TN FP   indexx: #1 3
#FN TP           #2 4
misclass = (1-sum(diag(confusionMatrix))/length(testdata$y))
CM = confusionMatrix
TP = confusionMatrix[2,2]
FP = confusionMatrix[1,2]
TN = confusionMatrix[1,1]
FN = confusionMatrix[2,1]
#accuracy looks at the data row wise:
#accuracy = (TP + TN) / (P+N), where P is FN+PN and N is TN + FP - i.e. those that actually are P or N. 
accuracy = (TP + TN ) / (TP+FN + FP+TN)

#F1 = 2*precision * recall /(precision + recall)
precision = TP / (TP+FP)
recall = TP / (TP+FN)
F1 = 2*precision*recall/(precision+recall)

accuracy
F1


#5
#probs[,1] - "no", probs[,2] - "yes"
probs = predict(optLeavesTree, newdata=testdata, type="vector")
probs_new_lossMatrix = ifelse((probs[,"no"]/probs[,"yes"]>5), "no", "yes")

confusionMatrix_NewLoss = table(true = testdata$y, pred = probs_new_lossMatrix) #true yes/no-label on y-axis, predicted yes/no on x-axis

misclass = (1-sum(diag(confusionMatrix_NewLoss))/length(testdata$y))
CM = confusionMatrix_NewLoss
TP = confusionMatrix_NewLoss[2,2]
FP = confusionMatrix_NewLoss[1,2]
TN = confusionMatrix_NewLoss[1,1]
FN = confusionMatrix_NewLoss[2,1]


accuracy_newLossMatrix = (TP + TN ) / (TP+FN + FP+TN)

precision = TP / (TP+FP)
recall = TP / (TP+FN)
F1_newLossMatrix = 2*precision*recall/(precision+recall)

accuracy_newLossMatrix
F1_newLossMatrix


#6 - "good" - should say "yes" in lab PM

#######USING FOR LOOPS########### - 
pi_vec = seq(from=0.05, to=0.95, by=0.05)

#using the same optimal tree model
probs_tree = predict(optLeavesTree, newdata=testdata, type="vector")
TPR_tree = numeric(length(pi_vec))
FPR_tree = numeric(length(pi_vec))

for (i in 1:length(pi_vec)) {
  predictions = ifelse((probs_tree[,2]>pi_vec[i]), "yes", "no")
  confMatrix = table(testdata$y, predictions)
  if(ncol(confMatrix) == 1) {
  confMatrix = cbind(confMatrix, c(0,0))
  }
  TP = confMatrix[2,2]
  FP = confMatrix[1,2]
  TPR = TP/(sum(confMatrix[2,]))
  FPR = FP/(sum(confMatrix[1,]))
  TPR_tree[i] = TPR
  FPR_tree[i] = FPR
}

#for the logistic regression
logistic_regression = glm(y ~., data=testdata, family=binomial)
probs_logistic = predict.glm(logistic_regression, newdata=testdata, type="response")
TPR_logreg = numeric(length(pi_vec))
FPR_logreg = numeric(length(pi_vec))

for (i in 1:length(pi_vec)) {
  predictions = ifelse(probs_logistic>pi_vec[i], "yes", "no")
  confMatrix = table(testdata$y, predictions)
  if(ncol(confMatrix) == 1) {
    confMatrix = cbind(confMatrix, c(0,0))
  }
  TP = confMatrix[2,2]
  FP = confMatrix[1,2]
  TPR = TP/(sum(confMatrix[2,]))
  FPR = FP/(sum(confMatrix[1,]))
  TPR_logreg[i] = TPR
  FPR_logreg[i] = FPR
}

#ROC plot
plot(FPR_tree, TPR_tree, col="blue", type="l", main="ROC", xlab="FPR", ylab="TPR")
points(FPR_logreg, TPR_logreg, col="red", type="l")
legend(x="bottomright", legend=c("Optimal tree", "Logistic regression"), lty = c(1, 1), col = c(4, 2))


#########USING SAPPLY########### gör om till en function som tar in threshold, samt probs (så kan vi använda för tree och logreg i samma)
tpr_fpr <- function(threshold_r, probabilities) {
  predictions = ifelse((probabilities>threshold_r), "yes", "no")
  confMatrix = table(testdata$y, predictions)
  if(ncol(confMatrix) == 1) {
    confMatrix = cbind(confMatrix, c(0,0))
  }
  TP = confMatrix[2,2]
  FP = confMatrix[1,2]
  TPR = TP/(sum(confMatrix[2,]))
  FPR = FP/(sum(confMatrix[1,]))
  return(c(FPR,TPR))
}

pi_vec = seq(from=0.05, to=0.95, by=0.05)

#using the same optimal tree model
probs_tree = predict(optLeavesTree, newdata=testdata, type="vector")
tree_FPR_TPR = sapply(pi_vec, function(x) tpr_fpr(x, probs_tree[,2]))

#using logistic regression
logistic_regression = glm(y ~., data=testdata, family=binomial)
probs_logistic = predict.glm(logistic_regression, newdata=testdata, type="response")
logreg_FPR_TPR = sapply(pi_vec, function(x) tpr_fpr(x, probs_logistic))

#plot ROC curves
plot(tree_FPR_TPR[1,], tree_FPR_TPR[2,], col="blue", type="l", main="ROC 2", xlab="FPR", ylab="TPR")
points(logreg_FPR_TPR[1,], logreg_FPR_TPR[2,], col="red", type="l")
legend(x="bottomright", legend=c("Optimal tree", "Logistic regression"), lty = c(1, 1), col = c(4, 2))
