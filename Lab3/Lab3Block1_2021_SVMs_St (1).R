# Lab 3 block 1 of 732A99/TDDE01/732A68 Machine Learning
# Author: jose.m.pena@liu.se
# Made for teaching purposes

library(kernlab)
set.seed(1234567890)

#?data, data()
data(spam)
#det urspurnliga data setet spam är numrerat med raderna 1..4601. 
#sample takes a sample of the specified size from the elements of x using either with or without replacement.
#sample()
#dvs här shufflar vi om raderna i princip, men behåller alla urspurngliga rader. 
foo <- sample(nrow(spam))
spam <- spam[foo,]

#58th col/Featureis "type - spam/nonspam, scale the data except this 58th col
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
  #This is a list which contains the parameters to be used with the kernel function.here just sigma 
  #which is the inverse kernel width for the gaussian kernel
  #	cost of constraints violation (default: 1) this is the 'C'-constant of the regularization term in the Lagrange formulation.
  #C is what we change thoruhgout the iterations here. C = 1/2nlambda dvs för regularizationen alpha_i < 1/2nlambda
  #detta är vårt C. 
  #dvs när i ökar, så ökar C, och detta betyder att lambda minskas.-> mer regularisering i feature space
  #(ty inversen 1/lambda ökar där) -> färre suport vectors. 
  #medan regularization i innput space minskar ty lambda minskar. 
  filter <- ksvm(type~.,data=train,kernel="rbfdot",kpar=list(sigma=0.05),C=i,scaled=FALSE)
  #predict on validation data, spam or nonspam
  mailtype <- predict(filter,val[,-58])
  #confusion matrix of predictions and true labels
  t <- table(mailtype,val[,58])
  #err_al = err_val + den nya typ. eftersom vi kör c(gamla, + en ny)
  #misclassification. summan av diagonalen, dvs nonspam nonspam + spam spam / alla i t. 
  err_val <-c(err_val,(t[1,2]+t[2,1])/sum(t))
  #kan plotta detta, eller which.min, ser då att index 13 är minst ,dvs 0.3*13 = 3.9 är optimala värdet på C. 
}
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

# Questions

# 1. Which filter do we return to the user ? filter0, filter1, filter2 or filter3? Why?

# 2. What is the estimate of the generalization error of the filter returned to the user? err0, err1, err2 or err3? Why?

#VALIDATION SET Has been used previously for selecting hyperparameter!!!!!
#thus models being trained with this data set will be biased, since we tuned hyperparameter C usign validaitron data.


#filter 0 trained on train, predictions on val
#val set was used when determining the hyperparameter C, so we have to some extent already used this
#data set. thus this model also used data which has already been seen by the model.

#filter 1 trained on train, predictions on test
#this filter predicts on unseen data .
#CHOOSE THIS! The ionly model unbiased estimate of the error since the testing data is unknown to the model, 
#and we dont use our validation set to train the model. 

#filter 2 traiend on train+val, predictions on test
#this filter predicts on unseen data.
#the validatio ndata taht is used to train has been used before when selecting hperparaemter. 
#so this data set is already "biased" since we used this data set to minimize the error
#for differnet values of hyperparaemter C. so this data is not really unseen. 

#filter 3 trained on ALL data, rpedictions on test. - i.e. we are predicitng on data 
#the model has seen before and been trained on. so we get a lower error/MSE than we should in practice. 
#since we train on test data also, and then predict on it. 

#choose filter 1. error 0.84 - highest because doesnt use any previously seen data 

#so our model now: using training data and different values for C we train some models that we then
#evaluate using our validation data set and calculate error/MSE.
#then, we select the model with value of C that minimzed the MSE.
#finally we use this model and estimate its error on new unseen data, i.e. the test data.
#This is what we are doing if we choose filter 1!


# 3. Implementation of SVM predictions.

#Once a SVM has been fitted to the training data, a new point is essentially classified
#according to the sign of a linear combination of the kernel function values between the
#support vectors and the new point. --- typ som i assignment 1 med sum av kernels samt multiplikation? typ
#fast LK - är ju mer konstant*kernel + konstakt2*kernel2 + ...
#yhat(x*) = sign(alpha * K(X,x*))


#alphaindex - returns indexes of the support vectors
#alphaindex(filter3) gives us a list of 1 length. [[1]] gives us the 1 element, i.e. the vector! 1561 SV's. 
supportVectors<-alphaindex(filter3)[[1]]
#coef - returns the linear coefficients for the support vectors. 1561 coefficients, 1 for each SV's. 
coefs<-coef(filter3)[[1]]
#b - the resulting offset/intercept - in this case the NEGATIVE intercept of the linear combination
intercept = -b(filter3)



#rbfdot - The kernel generating functions provided in kernlab 
#The Gaussian RBF kernel k(x,x') = \exp(-?? \|x - x'\|^2) 
#rbfkernel <- rbfdot(sigma = 0.1)

#båda dessa kernels tar in samma parametrar x, xnew och returnerar samma värde!!!! 
#dvs kan använda vilken som
kernel <- function (x, xnew, sigma = 0.05) {
  #euclidean distance: ||x-x*||^2 = sqrt( (x1-x1)^2* + (x2-x2)^2 * + ... (xn-xn)^2 ) 
  # |x-x'|^2 --> med eucilidan distance --> sum((x-xnew)^2), dvs sqrt och ^2 tar ut varandra
  kernelValue = exp(-sigma * (sqrt(sum((x-xnew)^2)))^2 )
  return (kernelValue)
}

kernel2 <- rbfdot(sigma = 0.05)

##k och k2 är samma som preds och alpha_kernelvals, bara annat mer clean (?) sätt att skriva på?
#yhat(x*) = alphahat^t * K(X,x*)
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
    ##yhat(x*) = alphahat^t * K(X,x*) = alpha1*K1 + alpha2*K2 + .... + intercept
    alpha_kernelvals[j] = coefs[j] * kernel(spam[supportVectors[j] , -58], spam[i,-58])
    #k2 <- c(k2, coefs[j] * kernel(spam[supportVectors[j] , -58], spam[i,-58]))
  }
  
  preds[i] = sum(alpha_kernelvals) + intercept
  #k <- c(k, sum(k2) + intercept)
}

#k
preds

#sign of this is prediction. so negative is spam, positive nto spam right? 
SVMmodelPredictions = predict(filter3,spam[1:10,-58], type = "decision")
SVMmodelPredictions = as.numeric(SVMmodelPredictions)

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
    message("Yahtzee!")
  }
}

foo(preds, SVMmodelPredictions)