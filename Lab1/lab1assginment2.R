#1
data=read.csv("parkinsons.csv")
dataScaled = scale(data) 
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.6))
traindata=dataScaled[id,]
testdata=dataScaled[-id,]

#2
# fit a linear regression model using training data. uses ordinary least squares for fitting. 
#remove columns we are not interested in, and -1 to remove intercept
lm_traindata = lm(motor_UPDRS ~ . -age -sex -subject. -test_time -motor_UPDRS -total_UPDRS -1, data=as.data.frame(traindata) )

#predict using our model on the training data 
pred_train <- predict(lm_traindata, se.fit=TRUE) #contains prediction, residuals, degrees of freedom
#pred_train$fit - the actual predictions vs lm_traindata$fitted.values[1:50] - fitted values, i.e. the predictions from our lm_traindata

#predict using our model on new data using our test data. 
pred_test <- predict(object=lm_traindata, newdata=as.data.frame(testdata), se.fit=TRUE)

MSETrain = sum((traindata[,5] - pred_train$fit)^2) / nrow(traindata)
MSETest = sum((testdata[,5] - pred_test$fit)^2) / nrow(testdata)

#3a Loglikelihood
Loglikelihood <- function(trainingData, theta, sigma) {
  xtrain = trainingData[,7:22]
  ytrain = trainingData[,5]
  n = nrow(xtrain)
  
  theta_x = t(theta)%*% t(xtrain)
  
  #diff - the differences between our predictions, i.e. theta * x, and our true labels i.e. y. 
  diff = t(theta_x)-as.matrix(ytrain)
  
  loglik = - n/2 * log(2*pi*sigma^2) -1/(2*sigma^2) * sum(diff^2)
  return(loglik)
}

#3b Ridge
#taking in theta_sigma as one vector, where we later must extract theta and sigma from it. 
Ridge <- function(trainingData, theta_sigma, lam) {
  theta = theta_sigma[1:16]
  sigma = theta_sigma[17]
  
  loglik = Loglikelihood(trainingData, theta, sigma)
  ridge = -loglik + lam*sum(theta^2)
  return(ridge)
}

#3c) RidgeOpt
RidgeOpt <- function(trainingDataIn, theta, sigma, lambdaIn) {
  initialValues = c(theta, sigma)
  #optimizing over c(theta,sigma), which is why we in the ridge function have theta_sigma as a parameter. 
  #sending trainingDataIn and lambdaIn as parameters not to optimize over to the Ridge function
  res = optim(par = initialValues, fn=Ridge, gr = NULL, trainingData=trainingDataIn, lam=lambdaIn,  method="BFGS")
  return(res)
}

model3 = lm(motor_UPDRS ~ . -age -sex -subject. -test_time -motor_UPDRS -total_UPDRS -1, data=as.data.frame(traindata) )
theta = model3[["coefficients"]]
sigma = summary(model3)$sigma

#3d DF - #trace, i.e. the diagonal of X(X^T X + lambda*I)^-1 *X^T  hat matrix
DF <- function(trainingData, lambda) {
  X = trainingData[,7:22]

  nforDiagMatrix = nrow(t(X) %*% X)
  hatMatrix =  X %*% solve(( t(X) %*% X + lambda*diag(nforDiagMatrix)) ) %*% t(X) 
  df = sum(diag(hatMatrix))
  return(df)
}

#for testing the functions, not part of the assignment
#loglikelihood3a = Loglikelihood(traindata, theta, sigma) 
#logLik(model3) - built in function
#ridge3b = Ridge(traindata, c(theta, sigma), 1)
#ridge3c = RidgeOpt(traindata, theta, sigma, 1)
#df = DF(traindata, 0.5)


#4
ridge1 = RidgeOpt(traindata, theta, sigma, 1 )
#ridge1$par contains the optimal parameters calculated by RidgeOpt function.
#these are theta + sigma in one array, this first 16 are theta, the 17th is sigma. 
OptimalParametersLambda_1 = ridge1$par[1:16]

ridge2 = RidgeOpt(traindata, theta, sigma, 100 )
OptimalParametersLambda_100 = ridge2$par[1:16]

ridge3 = RidgeOpt(traindata, theta, sigma, 1000 )
OptimalParametersLambda_1000 = ridge3$par[1:16]


#predict. prediction y-hat = theta-hat * X. 
#transposing both to get correct dimension for matrix multiplication (or could have taken X * theta-hat), 
#but wanted to stay consistent with the mathematical formula
prediction <- function(thetaHat, data) {
  X = data[,7:22]
  return(t(as.matrix(thetaHat)) %*% t(X))
}

calcMSE <-function(yHat, y) {
  #differences between our predicted y-hat and actual output y
  diff = (yHat-y)^2
  #the sum of squared errors SSE
  SSE = sum(diff)
  #MSE, SSE / n observations
  MSE = SSE / length(yHat)
  return(MSE)
}

#lambda = 1 train
predTrainLambda1 = prediction(OptimalParametersLambda_1, traindata)
calcMSE(predTrainLambda1, traindata[,5])
dfTrainLambda1 = DF(traindata, 1)

#lambda = 1 test
predTestLambda1 = prediction(OptimalParametersLambda_1, testdata)
calcMSE(predTestLambda1, testdata[,5])
dfTestLambda1 = DF(testdata, 1)

#lambda = 100  train
predTrainLambda100 = prediction(OptimalParametersLambda_100, traindata)
calcMSE(predTrainLambda100, traindata[,5])
dfTrainLambda100 = DF(traindata, 100)

#lambda = 100 test
predTestLambda100 = prediction(OptimalParametersLambda_100, testdata)
calcMSE(predTestLambda100, testdata[,5])
dfTestLambda100 = DF(testdata, 100)

#lambda = 1000 train
predTrainLambda1000 = prediction(OptimalParametersLambda_1000, traindata)
calcMSE(predTrainLambda1000, traindata[,5])
dfTrainLambda1000 = DF(traindata, 1000)

#lambda = 1000 test
predTestLambda1000 = prediction(OptimalParametersLambda_1000, testdata)
calcMSE(predTestLambda1000, testdata[,5])
dfTestLambda1000 = DF(traindata, 1000)