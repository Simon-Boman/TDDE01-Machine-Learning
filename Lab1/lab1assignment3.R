library(dplyr)
        
data=read.csv("pima-indians-diabetes.csv")

n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*1))
data=data[id,]
#testdata=data[-id,]

#1 
#extracting the relevant columns from our data frame
plasma_age_diabetes = data[,c(2,8,9)]

#splitting into cases of diabetes and non diabetes
diabetes = plasma_age_diabetes %>% filter(X1 == 1)
notDiabetes = plasma_age_diabetes %>% filter(X1 == 0)

plot(diabetes[,2], diabetes[,1], col="red", xlab="age", ylab="plasma glucose concentration")
points(notDiabetes[,2], notDiabetes[,1], col="green",)
legend(60,30,legend=c("diabetes","not diabetes"), col=c("red","green"), pch=c("o","o"))


#2
#training our model_glm
model_glm <- glm(plasma_age_diabetes[,3] ~ plasma_age_diabetes[,1] + plasma_age_diabetes[,2],
             family="binomial", data = data[,c(2,8,9)])
predictions = model_glm[["fitted.values"]]



#for (i in 1:length(predictions)) {
 # if (predictions[i] > 0.5) {
  #  predictions[i] = 1
 # }
  #else {
  #  predictions[i] = 0
  #}
#}
#if prediction > 0.5 set it to 1, else 0
predictions = ifelse(predictions > 0.5, 1, 0)

#probabilistic equation of the estimated model_glm
#i.e. how the target depends on the features and the estimated model_glm parameters probabilistically
#so,a way to estimate whether diabetic or not given new input data x1,x2 and given our theta from the model. 
g <- function(plasma, age) {
  theta = model_glm[["coefficients"]]
  variables = c(1,plasma,age)
  g = exp( t(theta)%*%variables ) / (1 + exp(t(theta)%*%variables))
  return(g[1])
}
yHat_newData = g(100,59)

confusionMatrix_r05 = table(plasma_age_diabetes[,3], predictions)
misclassification_r05 = ( 1-sum(diag(confusionMatrix_r05))/nrow(plasma_age_diabetes))

#combine plasma glucose and age data with our predictions, and split it into diabetes and non-diabetes cases like earlier. 
#then create a plot of this
plasma_age_predictedDiabetes <- cbind(plasma_age_diabetes[,1:2], predictions)
predictedDiabetes = plasma_age_predictedDiabetes %>% filter(predictions == 1)
predictedNotDiabetes = plasma_age_predictedDiabetes %>% filter(predictions == 0)
plot(predictedDiabetes[,2], predictedDiabetes[,1], col="red", main="Diabetes predictions using r = 0.5", xlab="age", ylab="plasma glucose concentration")
points(predictedNotDiabetes[,2], predictedNotDiabetes[,1], col="green")
legend(70,200,legend=c("diabetes","not diabetes"), col=c("red","green"), pch=c("o","o"))


#3
#To calculate the decision boundary: t(theta) %*% X = 0
#This gives us: theta0 + theta1x1 + theta2x2 = 0 <=> x2 = -theta0/theta2 -theta1/theta2 * x1
slope = - coef(model_glm)[3] / coef(model_glm)[2]
intercept = - coef(model_glm)[1] / coef(model_glm)[2]
abline(intercept, slope)


#4
#reset our predictions vector to its original values
predictions = model_glm[["fitted.values"]]

predictions = ifelse(predictions > 0.2, 1, 0)

#combine plasma glucose and age data with our new predictions, and split it into diabetes and non-diabetes cases like earlier. 
#then create a plot of this
plasma_age_predictedDiabetes <- cbind(plasma_age_diabetes[,1:2], predictions)
predictedDiabetes = plasma_age_predictedDiabetes %>% filter(predictions == 1)
predictedNotDiabetes = plasma_age_predictedDiabetes %>% filter(predictions == 0)
plot(predictedDiabetes[,2], predictedDiabetes[,1], col="red", main="Diabetes predictions using r = 0.2", xlab="age", ylab="plasma glucose concentration")
points(predictedNotDiabetes[,2], predictedNotDiabetes[,1], col="green")
legend(70,200,legend=c("diabetes","not diabetes"), col=c("red","green"), pch=c("o","o"))

confusionMatrix_r02 = table(plasma_age_diabetes[,3], predictions)
misclassification_r02 = ( 1-sum(diag(confusionMatrix_r02))/nrow(plasma_age_diabetes))
print(misclassification_r02)


#reset our predictions vector to its original values
predictions = model_glm[["fitted.values"]]

predictions = ifelse(predictions > 0.8, 1, 0)

#combine plasma glucose and age data with our new predictions, and split it into diabetes and non-diabetes cases like earlier. 
#then create a plot of this
plasma_age_predictedDiabetes <- cbind(plasma_age_diabetes[,1:2], predictions)
predictedDiabetes = plasma_age_predictedDiabetes %>% filter(predictions == 1)
predictedNotDiabetes = plasma_age_predictedDiabetes %>% filter(predictions == 0)
plot(predictedDiabetes[,2], predictedDiabetes[,1], col="red", 
     main="Diabetes predictions using r = 0.8", xlab="age", ylab="plasma glucose concentration", xlim=c(20, 80), ylim=c(70,200) )
points(predictedNotDiabetes[,2], predictedNotDiabetes[,1], col="green")
legend(70,200,legend=c("diabetes","not diabetes"), col=c("red","green"), pch=c("o","o"))

confusionMatrix_r08 = table(plasma_age_diabetes[,3], predictions)
misclassification_r08 = ( 1-sum(diag(confusionMatrix_r08))/nrow(plasma_age_diabetes))
print(misclassification_r08)


#5 
#Basis function expansion trick
inputs = plasma_age_diabetes[,1:2]
inputs <- inputs %>%  mutate(z1 = (inputs[,1]^4))
inputs <- inputs %>%  mutate(z2 = (inputs[,1]^3*inputs[,2]))
inputs <- inputs %>%  mutate(z3 = (inputs[,1]^2*inputs[,2]^2))
inputs <- inputs %>%  mutate(z4 = (inputs[,1]*inputs[,2]^3))
inputs <- inputs %>%  mutate(z5 = (inputs[,2]^4))

model_glm_expanded <- glm(plasma_age_diabetes[,3] ~.,
                 family="binomial", data = inputs)

predictions = model_glm_expanded[["fitted.values"]]

predictions = ifelse(predictions > 0.5, 1, 0)


#combine plasma glucose and age data with our new predictions, and split it into diabetes and non-diabetes cases like earlier. 
#then create a plot of this
plasma_age_predictedDiabetes <- cbind(inputs[,1:2], predictions)
predictedDiabetes = plasma_age_predictedDiabetes %>% filter(predictions == 1)
predictedNotDiabetes = plasma_age_predictedDiabetes %>% filter(predictions == 0)
plot(predictedDiabetes[,2], predictedDiabetes[,1], col="red", 
     main="Diabetes predictions using basis function expansion, r = 0.5", xlab="age", ylab="plasma glucose concentration",  )
points(predictedNotDiabetes[,2], predictedNotDiabetes[,1], col="green")
legend(50,80,legend=c("diabetes","not diabetes"), col=c("red","green"), pch=c("o","o"))

confusionMatrix_basis_f_exp = table(plasma_age_diabetes[,3], predictions)
misclassification_basis_f_exp = ( 1-sum(diag(confusionMatrix_basis_f_exp))/nrow(plasma_age_diabetes))


theta_expanded = model_glm_expanded[["coefficients"]]

