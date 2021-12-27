library(ggplot2)
data = read.csv(file = 'communities.csv')

# ------------------
#       3.1
# ------------------

# scale all features except ViolentCrimesPerPop 
X = scale.default(data[,1:100])
X = cbind(X,data$ViolentCrimesPerPop)

# manually calculate variance
n = dim(data)[1]
S = 1/n * t(X) %*% X 
eig = eigen(S)
eigenval = eig$values 

# Display components variance  
variance = (eigenval/sum(eigenval)*100) 
sprintf("%2.3f", variance)
print(cumsum(variance))
sprintf("PCA1: %2.3f | PCA2: %2.3f", eigenval[1], eigenval[2])

# ------------------
#       3.2
# ------------------

pca = princomp(X)

# Trace plot of PC1
PC1 = pca$loadings[,1]
PC2 = pca$loadings[,2]
plot(sort(PC1), main="Traceplot of PC1", ylab = "loadings", xlab = "")

# 5 most contributing features
imp_fea = PC1[sort(abs(PC1), index.return=TRUE, decreasing =TRUE)$ix[1:5]]
print(imp_fea)

# Plot the PC score in coord pc1,pc2 and colour point by ViolentCrimePerPop
pc1_scores = pca$scores[,1]
pc2_scores = pca$scores[,2]
pc_data = data.frame(pc1 = pc1_scores, pc2 = pc2_scores, v = data$ViolentCrimesPerPop)
ggplot(data = pc_data, aes(x = pc1, y = pc2, colour = v)) + geom_point()


# ------------------
#       3.3
# ------------------

# Scale and divide the data into training and test
datas = scale.default(data)
n=dim(datas)[1]
set.seed(12345) 
id=sample(1:n, floor(n*0.5)) 
train=data.frame(datas[id,])
test=data.frame(datas[-id,])

# create linear regression model, ViolentCrimePerPop as target, predict using training and test data
model = lm(ViolentCrimesPerPop ~ ., data = train)
pre_train = predict(model,train)
pre_test = predict(model,test)
summary(model) 

# MSE for training and test 
MSE_train = mean((pre_train-train$ViolentCrimesPerPop)^2)
MSE_test = mean((pre_test-test$ViolentCrimesPerPop)^2)
print(MSE_train)
print(MSE_test)


# ------------------
#       3.4
# ------------------

# redefine as matrix and define variables
X_train = as.matrix(train[,1:100])
X_test = as.matrix(test[,1:100])
Y_train = as.matrix(train$ViolentCrimesPerPop)
Y_test = as.matrix(test$ViolentCrimesPerPop)
Fs_train=list()
Fs_test=list()
k=0
theta = list()


# create cost function using MSE
loss_fcn <- function(theta){
  error_train = mean((Y_train-X_train%*%theta)^2)
  error_test = mean((Y_test-X_test%*%theta)^2)
  .GlobalEnv$k= .GlobalEnv$k+1
  .GlobalEnv$Fs_train[[k]]=error_train
  .GlobalEnv$Fs_test[[k]]=error_test
  return(error_train)
}

# run regression with cost function
res_train <- optim(rep(0,100), loss_fcn, method = "BFGS")

# Both train and test error in same plot
i = 1500
Fstest = as.numeric(Fs_test[i:k])
Fstrain = as.numeric(Fs_train[i:k])


gg_data = data.frame(y2 = Fstrain, y1 = Fstest, it = i:k)
g = ggplot(data=gg_data, aes(x=it))  
g = g + geom_line(aes(y=y1), color="black")
g = g + geom_line(aes(y=y2), color="blue")
g = g + labs(x = "Iteration [1500:k]", y = "Test (black) & train (blue) error") 
g

# Optimal Training and Test error
opt_train = sort(unlist(Fs_train), index.return=TRUE)
opt_train$x[1] # 0.2592247
opt_train$ix[1] # 19915

opt_test = sort(as.numeric(Fs_test), index.return=TRUE)
opt_test$x[1] # 0.3769468
opt_test$ix[1] # 2183