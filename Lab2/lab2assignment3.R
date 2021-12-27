library(ggplot2)

data=read.csv("communities.csv")

#1

#index 101 is VoelentCrimesPerPop
###########???is it enough to scale like this, or shoudl we do it manually? 
dataScaled_x = scale(data[,-101])

#only scaled the features, original output y
dataScaled = cbind(dataScaled_x, data[,101])


#Covariance matrix, S = 1/n %*% X^T %*% X, thus exclude target y
cov = cov(dataScaled_x) 

#Eigenvectors and egeinvalues of S(covariance matrix)
#The principal componenets (directions) are the eigenvectors of the sample covariance. 
#the "amount" in each diretion, i..e how much variation each direction corresponds to, will be the eigenvalues. 
PCA_eigen = eigen(cov)

#we look at the eigenvalues since they correspond to the "size" of the variances
eigenvalues = PCA_eigen[["values"]]

#sprintf("%2.3f", PCA_eigen$values/sum(PCA_eigen$values)*100)
eigenvalues = as.numeric(format(eigenvalues, scientific = F, digits = 5))

acumulatedVariance <- function(percentage) {
  for (i in 1:length(eigenvalues)) {
    print(sum(eigenvalues[1:i]))
    if (sum(eigenvalues[1:i]) > percentage) {
      return (i)
    }
  }
}

numberOfFeatures = acumulatedVariance(95)
numberOfFeatures #35 features needed to obtain at least 95% of the variance in the data

#the proportion of variation explained by each of the first two principal components
firstPC = eigenvalues[1]
firstPC #25.02%
secondPC = eigenvalues[2]
secondPC #16.94%


#2

#Repeat PCA analysis by using princomp() function and make the trace plot of the first principle component".   TRACE!!!

res = princomp(dataScaled_x[,-101])
plot(res) #samme som screeplot(res)
#biplot(res)

#U - matrix of variable loadings, a matrix whose columns contain the eigenvectors (and the eigenvectors = PC's)
#This is the actual U used in PCA for Z = X%*%U, and this U consists of the eigenvectors. 
#100x100 matrix, how the PC's are expressed in terms of the original features.
#I.e. what the PC coordinates are in the respective original features.
#so if we plot a column that is in U as a trace plot, we see the dependence on the 
#y-axis showing how much each feature from the original data contributes to the respective PC.
U = res$loadings
PC1 = U[,1]
plot(PC1, main="Traceplot, PC1")

#Plot showing how much each of the 100 original features from X (index 1 to  100)
#contribute to PC1 = eigenvector1 of U by their value on the y-axis. 
#only a few features have a contribution which seems to be close to 0, 
#and quite many have a high contribution at 0.15 or higher (and -0.15 or smaller)
#and the rest seem to have a moderate contribution
#but overall, yes many features have a notable contribution, since relatively few have 
#a contribution close to 0.


#find the 5 features that contribute the most to the first PC:
#sorts the absolute values of PC1 in decreasing order.
#thus the first 5 features are the largest ones 
#PC1_abs_decreasing_order = order(PC1_abs, decreasing = TRUE)
PC1_abs = abs(PC1)
PC1_abs_decreasing_sort = sort(PC1_abs, decreasing=TRUE)

PC1_abs_decreasing_sort[1:5]
PC1["medFamInc"] 
PC1["medIncome"]
PC1["PctKids2Par"]
PC1["pctWInvInc"]
PC1["PctPopUnderPov"]

#median family income (differs from household income for non-family households)
#median household income 
#percentage of kids in family housing with two parents 
#percentage of households with investment / rent income in 1989 
#percentage of people under the poverty level 

#1,2,5 quite clearly connected, all have to do with the income level. 
#3 also probably has a high correlation with the above 3, since a family  with kids and only 1 parent have a
#much lower everage income per family member than a family with 2 parents
#4 is also connected to the economical situation
#thus, all the 5 features which contributed the most to PC1 (remember, PC1 is the PC that corresponds to the
#largest variance in the original data, thus these 5 features overall correspond to the largest variance),
#are related to the economical sitation. Makes sense that this would have a high effect on crime rates.



#Scores - The scores of the supplied data on the principal components
#plots how the data looks like in the first 2 principal components. 
#res$scores is 1994 rows x 100 cols, just like original data. 
#so here each data point is transfered from the original 100 cols to the PC's. 
scoresPC1 = res$scores[,1]
scoresPC2 = res$scores[,2]
scores = data.frame(res$scores[,1], res$scores[,2], dataScaled[,101])
names(scores) = c("component1", "component2", "crimesPerPop")
ggplot(scores, mapping=aes(x=component1, y=component2)) + geom_point(aes(colour = crimesPerPop))

ViolentCrimesPerPop = dataScaled[,101]
#quickplot, gg quickplot!
qplot(scoresPC1, scoresPC2, color=ViolentCrimesPerPop)

#it seems as if for high values on the PC2 axis, the rate of crimes decreases
#it also seems that as the value on the PC1 axis increases, the rate crime increases.
#from earlier, we saw that 2 PCs is enough to explain 25+17 = 42% of the variation. 
#seems like PC2 has a larger impact than PC1? still many light blue at both
#high and low values of PC1.
#but for PC2, its clear that the dots get ligher as PC2 increases??? 

####??? what does this tell us?


#3
#The correct way is to scale the training data and then apply the same transformation to the test data.
dataScaled_xy = scale(data)
n=dim(dataScaled_xy)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
traindata=as.data.frame(dataScaled_xy[id,])
testdata=as.data.frame(dataScaled_xy[-id,])

linreg_model = lm(ViolentCrimesPerPop ~ . , data = traindata)
#linreg_model = lm(traindata[101] ~ . , data = traindata)
trainFit = linreg_model$fitted.values
#trainFit2 = predict(object = linreg_model, newdata = traindata)
testFit = predict(object = linreg_model, newdata = testdata)

MSETrain = sum( (traindata[,101]-trainFit)^2 ) / length(trainFit)
MSETest = (sum((testdata[,101]-testFit)^2)/length(testFit))
MSETrain
MSETest
R2 = sum((MSETest-mean(testdata[,101]))^2) / sum((testdata[,101]-mean(testdata[,101]))^2) 
R2

plot(testdata[,101], testFit)
abline(0,1)
#MSE test 0.4, MSEtrain 0.26. 
#ofc MSE train lower than MSE test. Thus possible that the model overfits. 
#but R2 basically 0, thus the model cant explain any variability in the outcome y?

#if we plot the predictions with the true labels ,we see that the model does not perform so well. 


#4

xtrain = as.matrix(traindata[,-101])
xtest = as.matrix(testdata[,-101])
ytrain = as.matrix(traindata$ViolentCrimesPerPop)
ytest = as.matrix(testdata$ViolentCrimesPerPop)

trainErrors = list()
testErrors = list()
k = 0
theta_0 = c(rep(0,100))
theta_opt = numeric(0)

costf <- function(theta) {
  #instead of transpoing, take x * theta below
  trainPred = xtrain%*%theta
  testPred = xtest%*%theta
  MSETrain = mean((ytrain-trainPred)^2 )
  MSETest = mean((ytest-testPred)^2)
  .GlobalEnv$k = .GlobalEnv$k+1
  .GlobalEnv$trainErrors[[k]]=MSETrain
  .GlobalEnv$testErrors[[k]]=MSETest
  if (k == 2183) {
    .GlobalEnv$theta_opt = theta
  }
  return(MSETrain)
}

res <- optim(theta_0, fn=costf, method="BFGS")


which.min(testErrors) #2183
MSETrain = trainErrors[2183] #0.285
MSETest = testErrors[2183]  #0.377

trainErrors = as.numeric(trainErrors[1400:k])
testErrors = as.numeric(testErrors[1400:k])

gg_df = data.frame(y1 = trainErrors, y2 = testErrors, it = 1400:k)
ggplot(data=gg_df, aes(x=it)) + geom_line(aes(y=y1), color="black") + geom_line(aes(y=y2), color="blue") + 
  labs(x="Iteration, first 1400 removed", y="Test errors (blue), and train errors(black)")


#iteration number 2183 is optimal. 
#here we minimize our training error
#as can be seen in the grapoh, as the iterations go on, the train error decreases due t overfitting,
#while the test error starts increasing. thus we must apply early stopping, and we find the optimal iteration to be 
#iteration 2183. 
#here, the MSE train is higher than in part 3, 0.285, which is expected since we stop earlier and it 
#has overfitted less.
#and the MSE test is 0.285, which is lower than in part 3, since this hasent started increasing again yet.
#So this is an improvement compared to part 3 since we managed to decrease our testing error! 
#had we not performed early stopping, we would have arrived in a similar conclusion here as in part 3,
#i.e. we would overfit, and as we can see, the train error in the graph goes towards 0.4, 
#and the train error towards 0.26.
