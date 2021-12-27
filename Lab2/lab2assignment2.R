library(tree)
library(partykit)

data=read.csv2("bank-full.csv", stringsAsFactors = TRUE)
#csv - Reads a file in table format and creates a data frame from it, with cases corresponding to lines and variables to fields in the file.
#csv2 - (read.csv2) the variant used in countries that use a comma as decimal point and a semicolon as field separator. 

data = subset(data, select=-duration) #samma som data = data[,-12]
n=dim(data)[1] #samma som nrow(data), dvs hur många observationer
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

#features = data[,-16]
#target = data[,16]
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
#text(devianceTree, pretty=0)
devianceTree
summary(devianceTree)


#can access the misclassification rate from summary(defaultTree)
misclassDefaultTrain = summary(defaultTree)[["misclass"]][1]/summary(defaultTree)[["misclass"]][2]
##defaultgg = predict(defaultTree, newdata=traindata, type="class")
#confusionMatrix2 = table(traindata$y, defaultgg)
#misclass2 = (1-sum(diag(confusionMatrix2))/length(traindata$y))
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

#why are they the same for the first 2 models?
misclassDefaultTrain
misclassDefaultValidation
misclassNodeSizetTrain
misclassNodeSizetValidation
misclassDevianceTrain
misclassDeviancetValidation

#a,b same misclassificaiton errors.c lower for training, but higher for validation (overfitting)
#and c also has way more leaf nodes, so probably overfitting.
#larger minimum node size (a -> b), 2 leaves were removes (reduced from 6 to 5 leaves), 
#since we restricted how small the nodes could be. b less complex than a. 
#smaller minimum deviance -> The within-node deviance must be at least this times that of the root node for the node to be split.
#since we lower this, we will do more splits. which we can see going from tree a to c, which had 100+ leaves vs 6.


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
optLeaves = which.min(validationScore)

#interpret this graph in terms of bias-variance tradeoff

#for the training deviance, the more complex model (higher nr of leaves), the lower the error becomes since we overfit
#the model more and more, allowing it to be more flexible and adapt better to the training data. thus this is not 
#relevant to look at.

#for the validation deviance:
#as number of leaves increaes, the model complexity increases. 
#for a very simple model (low nr of leaves), the model underfits and we have a large bias error, but the variance is 
#small, since the model is simple and thus not flexible, so we can expect a small variance if we repeat this with new data.
#but for a complex model (high nr of leaves), thee model overfitts and we have a small bias error but a large variance, since now
#the model is very flexible and adapts to data very well. i.e. the mapping of input to output is very good due to the
#flexibilityy of the model, and thus a low bias, but if we repeat this for other data sets, due to the high flexibility 
#we will get a large variance.

#thus the optimal value (k=22 in this case) will be in the "middle", where the model is not too complex and not too simple,
#and has both moderate bias and variance.


# which variables seem to be most important for decision making in this tree

optLeavesTree = prune.tree(devianceTree, best=13)
plot(optLeavesTree)
text(optLeavesTree, pretty=0)
plot(optLeavesTree)
optLeavesTree
summary(optLeavesTree)

#Variables actually used in tree construction:
#[1] "poutcome" "month"    "contact"  "pdays"    "age"      "day"      "balance"  "housing"  "job"    
#så antar dessa

#Interpret the information provided by the tree
#structure (not everything but most important findings).
#poutcome firrst split: so prolly most important?  
#seems to be a dealbreaker, since if failure/other/unkown left, and if succes right.
#and if this was a success (Right), looks easy to determine y.
#followed by month, then job.
#where as we can see, both of these also quickly leads to an answer for y(?)


#4

pred = predict(optLeavesTree, newdata=testdata, type="class")

confusionMatrix = table(testdata$y, pred) #true label på y-axel, pred på x-axel
#TN FP   indexx: #1 3
#FN TP           #2 4
misclass = (1-sum(diag(confusionMatrix))/length(testdata$y))
CM = confusionMatrix
TP = confusionMatrix[2,2]
FP = confusionMatrix[1,2]
TN = confusionMatrix[1,1]
FN = confusionMatrix[2,1]
#accuracy looks at the data row wise:
#accuracy = (TP + TN) / (P+N), där P är FN+PN och N är TN + FP - dvs de som faktiskt är P eller N
accuracy = (TP + TN ) / (TP+FN + FP+TN)

#F1 = 2*precision * recall /(precision + recall)
precision = TP / (TP+FP)
recall = TP / (TP+FN)
F1 = 2*precision*recall/(precision+recall)

accuracy
F1
#ye idk not very good model due to the imbalance(?) and it predicts mcuch fewer as yes than actually are yes. 
#predicting a true y=yes as preediction no happens very frequently here. 1371 out of 1371+214 of the "yes" are classified as "no".

#f1 better, since takes into account imbalance. 
#whereas accuracy does not. and since so imbalanced, just always predicting "no" would give us good accuracy here.
#So we should look at the F1-score here since it considers both the precision and the recall. and we have a low F1 score (0.22),
#so our model performs bad. 
#acuracy 0.89, but yeah this is irrelevant

#5
#TP TN = 0, FN = 5, FP = 1
#so we punish FN more than FP.
#thus, the model must be 5 times as certain classifying as no (classifying a true yes as predicted no)
#in order to recduce the number of FN.
#in this case, we prefer to have more FP and less FN ofc, compared to like a healthcare case. 

#probs[,1] - yes, probs[,2] - no
probs = predict(optLeavesTree, newdata=testdata, type="vector")
probs_new_lossMatrix = ifelse((probs[,1]/probs[,2]>5), "no", "yes")

confusionMatrix_NewLoss = table(testdata$y, probs_new_lossMatrix) #true labela <- y-axel, pred på ^ x-axel

misclass = (1-sum(diag(confusionMatrix_NewLoss))/length(testdata$y))
CM = confusionMatrix_NewLoss
TP = confusionMatrix_NewLoss[2,2]
FP = confusionMatrix_NewLoss[1,2]
TN = confusionMatrix_NewLoss[1,1]
FN = confusionMatrix_NewLoss[2,1]


accuracy_newLossMatrix = (TP + TN ) / (TP+FN + FP+TN)

#F1 = 2*precision * recall /(precision + recall)
precision = TP / (TP+FP)
recall = TP / (TP+FN)
F1_newLossMatrix = 2*precision*recall/(precision+recall)

accuracy_newLossMatrix
F1_newLossMatrix

#accuracy slightly lower at 0.87, but it is irreelvant.
#f1 is not 0.49, high icnrease from 5. i.e. with the new lossm atrix, where the model punsishes FN harder than FP,
#we get a much better model, which we can see from the increase in F1-score.


#6
#"good" - should say "yes" in lab PM

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



#The models have very similar performance. The area under the curve (AUC) can by simply looking at the plot barely be differentiated.
#A Precision-Recall-Curve (PR-Curve) would in this case be better due to the imbalanced data. 
#like with accuracy vs F1-score, F1-score is better for imbalanced data, and the same goes for ROC-curve vs PR-curve,
#where PR-curve in this case would be a better way to evaluate the models. 
#this is due to, similarily to the argument with accuracy vs F1-score, the ROC-curve will still show good performance
#if we would just classify everything as the majority class, i.e. "no", whereas a PR-curve would take this imbalance into account. 
