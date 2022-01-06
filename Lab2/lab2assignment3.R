library(ggplot2)

data=read.csv("communities.csv")

#1
#index 101 is VoelentCrimesPerPop
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
#Repeat PCA analysis by using princomp() function and make the trace plot of the first principle component".
res = princomp(dataScaled[,-101])
plot(res) #same as screeplot(res)
#biplot(res)

#U - matrix of variable loadings, a matrix whose columns contain the eigenvectors (PC's)
#This is the actual U used in PCA for Z = X%*%U, and this U consists of the eigenvectors. 
#100x100 matrix, how the PC's are expressed in terms of the original features.
#I.e. what the PC coordinates are in the respective original features.
#so if we plot a column that is in U as a trace plot, we see the dependence on the 
#y-axis showing how much each feature from the original data contributes to the respective PC.
U = res$loadings
PC1 = U[,1]
#Plot showing how much each of the 100 original features from X (index 1 to  100)
#contribute to PC1 (eigenvector1 of U) by their value on the y-axis. 
plot(PC1, main="Traceplot, PC1")


#find the 5 features that contribute the most to the first PC:
#sorts the absolute values of PC1 in decreasing order.
#thus the first 5 features are the largest ones 
#PC1_abs_decreasing_order = order(PC1_abs, decreasing = TRUE)
PC1_abs = abs(PC1)
PC1_abs_decreasing_sort = sort(PC1_abs, decreasing=TRUE)

PC1_abs_decreasing_sort[1:5]
PC1_orig_val = c(PC1["medFamInc"], PC1["medIncome"], PC1["PctKids2Par"], PC1["pctWInvInc"], PC1["PctPopUnderPov"])
PC1_orig_val


#Scores - The scores of the supplied data on the principal components
#plots what the coordinates of our data looks like in the coordinates of the first 2 principal components. 
#res$scores is 1994 rows x 100 cols, just like original data. 
#so here each data point is transferred from the original 100 cols to the PC's. 
scoresPC1 = res$scores[,1]
scoresPC2 = res$scores[,2]
scores = data.frame(res$scores[,1], res$scores[,2], dataScaled[,101])
names(scores) = c("PC1", "PC2", "crimesPerPop")
ggplot(scores, mapping=aes(x=PC1, y=PC2)) + geom_point(aes(colour = crimesPerPop))

ViolentCrimesPerPop = dataScaled[,101]
#quickplot, gg quickplot!
qplot(scoresPC1, scoresPC2, color=ViolentCrimesPerPop)


#3
dataScaled_xy = scale(data)
n=dim(dataScaled_xy)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
traindata=as.data.frame(dataScaled_xy[id,])
testdata=as.data.frame(dataScaled_xy[-id,])

linreg_model = lm(ViolentCrimesPerPop ~ . , data = traindata)
#linreg_model = lm(traindata[101] ~ . , data = traindata)
trainFit = linreg_model$fitted.values
#trainFit = predict(object = linreg_model, newdata = traindata)
testFit = predict(object = linreg_model, newdata = testdata)

MSETrain = sum( (traindata[,101]-trainFit)^2 ) / length(trainFit)
MSETest = (sum((testdata[,101]-testFit)^2)/length(testFit))
R2 = sum((MSETest-mean(testdata[,101]))^2) / sum((testdata[,101]-mean(testdata[,101]))^2) 
MSETrain
MSETest
R2

plot(testdata[,101], testFit, xlab="true value", ylab="predicted value")
abline(0,1)


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
  #instead of transposing, take x * theta below
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

#removing first 1400 iterations
trainErrors = as.numeric(trainErrors[1400:k])
testErrors = as.numeric(testErrors[1400:k])

gg_df = data.frame(y1 = trainErrors, y2 = testErrors, it = 1400:k)
ggplot(data=gg_df, aes(x=it)) + geom_line(aes(y=y1), color="black") + geom_line(aes(y=y2), color="blue") + 
  labs(x="Iteration, first 1400 removed", y="Test errors (blue), and train errors(black)")

plot(trainErrors, type="l", col="black")
points(testErrors, type="l", col="blue")
