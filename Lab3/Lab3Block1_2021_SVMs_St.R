# Lab 3 block 1 of 732A99/TDDE01/732A68 Machine Learning
# Author: jose.m.pena@liu.se
# Made for teaching purposes

library(kernlab)
set.seed(1234567890)

#?data, data()
data(spam)
#original data set spam has rows 1...4601
#sample takes a sample of the specified size from the elements of x using either with or without replacement.
#sample()
#i.e. we shuffle the rows using sample()
foo <- sample(nrow(spam))
spam <- spam[foo,]

#58th col/feature "type" is the target which is spam/nonspam, scale the data except this 58th col
spam[,-58]<-scale(spam[,-58])
#divide inte trainset, validation set, combined train+validation, and test set
train <- spam[1:3000, ]
val <- spam[3001:3800, ]
trainval <- spam[1:3800, ]
test <- spam[3801:4601, ] 

by <- 0.3 
err_val <- NULL
#for the sequence: from 0.3 to 5, by 0.3, i.e. 0.3, 0.6, 0.9...
for(i in seq(by,5,by)){
  #predict type ~ all features. train with traindata. kernel = gaussian kernel.
  
  #kpar - the list of hyper-parameters (kernel parameters). 
  #This is a list which contains the parameters to be used with the kernel function, 
  #here just sigma which is the inverse kernel width for the gaussian kernel
  
  #Cost of constraints violation (default: 1) this is the 'C'-constant of the regularization term in the Lagrange formulation.
  #C is what we change throughout the iterations here. C = 1/(2*n*lambda), i.e. for the regularization
  #alpha_i < 1/(2*n*lambda), this is our C! 
  #I.e. when i increases, we increase C, which means that lambda decreases -> more regularization in feature space,
  #since the inverse of 1/lambda is increasing -> fewer support vectors
  #whereas as lambda decreases -> less regualrization in the input space 
  
  filter <- ksvm(type~., data=train, kernel="rbfdot", kpar=list(sigma=0.05), C=i, scaled=FALSE)
  #predict on validation data, spam or nonspam
  mailtype <- predict(filter,val[,-58])
  #confusion matrix of predictions and true labels
  t <- table(mailtype,val[,58])
  #err_val = err_val + the new error, since we do like c(old ones + new one) 
  #misclassification. sum of diagonal, i.e. nonspam nonspam + spam spam / all in conf. matrix (t)
  err_val <-c(err_val,(t[1,2]+t[2,1])/sum(t))
}
#we can plot this, or do which.min, and we will see that the minimum is for index=13, i.e. 0.3*13 = 3.9, 
#thus this is the optima lvalue for C. 


#train model on traindata. predict on validation set. 
filter0 <- ksvm(type~.,data=train,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_val)*by,scaled=FALSE)
mailtype <- predict(filter0,val[,-58])
t <- table(mailtype,val[,58])
err0 <- (t[1,2]+t[2,1])/sum(t)
err0

#train model on traindata. predict on test set. 
filter1 <- ksvm(type~.,data=train,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_val)*by,scaled=FALSE)
mailtype <- predict(filter1,test[,-58])
t <- table(mailtype,test[,58])
err1 <- (t[1,2]+t[2,1])/sum(t)
err1

#train model on traindata+validation data. predict on val set. 
filter2 <- ksvm(type~.,data=trainval,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_val)*by,scaled=FALSE)
mailtype <- predict(filter2,test[,-58])
t <- table(mailtype,test[,58])
err2 <- (t[1,2]+t[2,1])/sum(t)
err2

#train model on all data. predict on val set. 
filter3 <- ksvm(type~.,data=spam,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_val)*by,scaled=FALSE)
mailtype <- predict(filter3,test[,-58])
t <- table(mailtype,test[,58])
err3 <- (t[1,2]+t[2,1])/sum(t)
err3

errors <- c (err0, err1, err2, err3)


# 3. Implementation of manual SVM predictions and comparison with above SVM.
#yhat(x*) = sign(alpha * K(X,x*) +b)


#alphaindex - returns indexes of the support vectors
#alphaindex(filter3) gives us a list of 1 length. [[1]] gives us the 1 element, i.e. the support vectors! Total 1561 SV's. 
supportVectors<-alphaindex(filter3)[[1]]
#coef - returns the linear coefficients for the support vectors. 1561 coefficients, 1 for each SV's. 
coefs<-coef(filter3)[[1]]
#So alphaindex and coefs in combinatio give us the indexes of the support vectors and their coefficients.
#b - the resulting offset/intercept - in this case the NEGATIVE intercept of the linear combination
#we take -b since b() returns the negative intercept
intercept = -b(filter3)


#Both kernel and kernel2 take the same parameters, x and xnew, and return the same value.
#So can use any of them, but I will use the rfbdot from kernellab package.  
kernel <- function (x, xnew, sigma = 0.05) {
  #euclidean distance: ||x-x*||^2 = sqrt( (x1-x1)^2* + (x2-x2)^2 * + ... (xn-xn)^2 ) 
  # |x-x'|^2 --> with eucilidan distance --> sum((x-xnew)^2), i.e. sqrt and ^2 cancel eachother out. 
  kernelValue = exp(-sigma * (sqrt(sum((x-xnew)^2)))^2 )
  return (kernelValue)
}
#rbfdot - The kernel generating functions provided in kernlab 
#The Gaussian RBF kernel k(x,x') = \exp(-?? \|x - x'\|^2) 
#rbfkernel <- rbfdot(sigma = 0.1)
kernel2 <- rbfdot(sigma = 0.05)


##k och k2 är samma som preds och alpha_kernelvals, bara annat mer clean sätt att skriva på?
#k<-NULL
preds = rep(0,10)
#k <- NULL
for(i in 1:10){ # We produce predictions for just the first 10 points in the dataset.
  alpha_kernelvals = rep(0,length(supportVectors))
  #k2<-NULL
  #supportVectors - index av de som är supportvectors.
  #spam(supportVectors[1]) - den första supportvectorn 
  #i vårt fall är supportVectors[1] = 3
  #spam[3,] - ger oss den supportVector, dvs test pointen, och dess värden för de 58 features
  #DVS samma som att ta spam[supportVectors[1], ]
  #sen även ta bort -58.
  
  for(j in 1:length(supportVectors)){
    #LK mellan vår point x* och våra supportvectors + intercept? 
    ##yhat(x*) = alphahat^t * K(X,x*) + b = alpha1*K1 + alpha2*K2 + .... + intercept
    #so alpha_kernelvals = each support vectors contribution to predictio, so alpha_kernelvals will have 1561 values = nr of SV's.
    #then, prediction will be sum of all these values + intercept.
    alpha_kernelvals[j] = coefs[j] * kernel(unlist(spam[supportVectors[j] , -58]), unlist(spam[i,-58]))
    #k2 <- c(k2, coefs[j] * kernel2(spam[supportVectors[j] , -58], spam[i,-58]))
  }
  
  preds[i] = sum(alpha_kernelvals) + intercept
  #k <- c(k, sum(k2) + intercept)
}

#k
preds

#sign of this is prediction. so negative is spam, positive nto spam right? 
#BUT we specify type="decision", thus we get back the value and not sign(value), so we get a
#numeric value and not a prediction here
SVMmodelPredictions = predict(filter3,spam[1:10,-58], type = "decision")
SVMmodelPredictions = as.numeric(SVMmodelPredictions)
#SVMmodelPredictionsClassification = predict(filter3,spam[1:10,-58]) #would return class label prediction

all.equal(preds, SVMmodelPredictions)

#Cat: Outputs the objects, concatenating the representations. cat performs much less conversion than print.
cat("Are our above predictions from 'manual' calculations the same as using predict with our SVM model?",
    all.equal(preds, SVMmodelPredictions))


#> which(k != SVMmodelPredictions)
#[1]  1  2  3  4  5  6  7  8  9 10
#> all(k==SVMmodelPredictions)
#[1] FALSE

foo <- function(A,B){
  if (!isTRUE(all.equal(A,B))){
   mismatches <- paste(which(A != B), collapse = ",")
    stop("error the A and B does not match at the following columns: ", mismatches )
  } else {
    message("TRUE!")
  }
}

foo(preds, SVMmodelPredictions)