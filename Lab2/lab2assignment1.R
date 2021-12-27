library(glmnet)
data=read.csv("tecator.csv")

#remove unwanted data columns, keep only fat(y) and channels(x_i)
data = data[,-1]
data = subset(data, select=-c(Protein, Moisture))
#divide data
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
traindata=data[id,]
testdata=data[-id,] 


#1
#fit linear regression model
model_linreg = lm(traindata$Fat ~ ., data=traindata )
#extract fitted target values from traindata
trainFit = model_linreg$fitted.values
#predict target values from testdata using our above model
testPred = predict(object = model_linreg, newdata = testdata)
#calculate MSE for train and test data
MSETrain = sum( (traindata[,101]-trainFit)^2 ) / length(trainFit)
MSETest = (sum((testdata[,101]-testPred)^2)/length(testPred))
#plot preddictions on testdata with true labels from test data
plot(testPred, testdata[,101],xlim=c(0,100), ylim=c(0,100) )
abline(0,1)


#3 
#extract features(channels x_i) and target (fat) from traindata
features = traindata[,-101]
target = traindata[,101]

#alpha=1 specifies lasso. this model only has 77 variables, i.e. it automatically performs parameter selection
lasso_model=glmnet(x = as.matrix(features), y = target, alpha=1, family="gaussian")
summary(lasso_model)
#plot the lasso models coefficients dependency on the log of lambda
plot(lasso_model, xvar="lambda", label=TRUE, main="Regression coefficients dependency on the log of penalty factor lambda")
#zoom on where we only have 3 non-zero variables
plot(lasso_model, xvar="lambda", label=TRUE,xlim=c(-1,0), ylim=c(-1,1), main="plotted lines for log(lambda) = -0.62 and -0.35")
abline(v=-0.35, col="red")
abline(v=-0.62, col="red")
#predict target values from our test data using above model
lasso_testPred = predict(lasso_model, newx=as.matrix(testdata[,1:100], type="response"))
#calculate MSE for test data
MSETestLasso = (sum((testdata[,101]-lasso_testPred)^2)/length(lasso_testPred))
MSETestLasso


#4
#alpha=0 specifies ridge
ridge_model=glmnet(as.matrix(features), target, alpha=0, family="gaussian")
#plot the ridge models coefficients dependency on the log of lambda
plot(ridge_model, xvar="lambda", label=TRUE)
#predict target values from our test data using above model
ridge_testPred = predict(ridge_model, newx=as.matrix(testdata[,1:100], type="response"))
#calculate MSE for test data
MSETestRidge = (sum((testdata[,101]-ridge_testPred)^2)/length(ridge_testPred))
MSETestRidge


#5
features = traindata[,-101]
target = traindata[,101]

#calculate optimal lasso model (by finding optimal lambda) using cross validation
lasso_optimal_model = cv.glmnet(as.matrix(features), target, alpha = 1, family="gaussian")

lasso_optimal_model$lambda.min #value of lambda that gives minimal cross validation error. lambda =  0.05744535 
#index lambda.min = 51
lasso_optimal_model$cvm[51] #mean cross validation error = 14.21175 
lasso_optimal_model$nzero[51] #features are non-zero, i.e. model with 8 variables, for lambda = 0.05744535
##plot the dependency between Cross-validation error and the log(lambda)
plot(lasso_optimal_model)
#zoom in on lambda_min (left dotted lines) and lambda_1se(right dotted lines) 
#to determine which lambda to use, we choose lambda_min
plot(lasso_optimal_model, xlim=c(-3,-2), ylim=c(11,22))
#zoom in to compare cross-validation error of log(lambda) = -4 with our selected lambda
plot(lasso_optimal_model, xlim=c(-4.1,-2.7), ylim=c(11,18))

#If we train a new model using our optimal lambda using all train data, we get a slightly more accurate model, 
#since we now use all of our training data for training, compared to using 9/10 parts for training and 1/10 parts for validation 
#as we do when using 10-fold cross-validation. 
opt_lambda_glmnet = glmnet(x = as.matrix(features), y = target, alpha=1, family="gaussian", lambda=lasso_optimal_model$lambda.min)
#predict using our new model
ypred = predict(opt_lambda_glmnet, newx= as.matrix(testdata[,-101]))
y = testdata[,101]
#plot predictions vs true values
plot(y, ypred, ylim=c(0,60), xlim=c(0,60))
abline(0,1)

#MSE for test data for above model, i.e. our final optimal lasso model
MSETestOptimalLasso = sum((ypred-y)^2)/length(y)
MSETestOptimalLasso

#MSE for train data for above model
ypredTrain = predict(opt_lambda_glmnet, newx= as.matrix(traindata[,-101]))
MSETrainOptimalLasso = sum((ypredTrain-traindata[,101])^2)/length(y)
MSETrainOptimalLasso

#compare with previous MSE for test data
MSETest #part1
MSETestLasso #part3
MSETestRidge #part4


