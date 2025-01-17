library(dplyr)
        
data=read.csv("pima-indians-diabetes.csv")

#1 
#plasma_age_diabetes = data[,c(2,8,9)] #extracting the relevant columns from our data frame

#diabetes = plasma_age_diabetes %>% filter(X1 == 1) #splitting into cases of diabetes and non diabetes
#notDiabetes = plasma_age_diabetes %>% filter(X1 == 0)

#plot(diabetes[,2], diabetes[,1], col="red", xlab="age", ylab="plasma glucose concentration")
#points(notDiabetes[,2], notDiabetes[,1], col="green",)
#legend(60,30,legend=c("diabetes","not diabetes"), col=c("red","green"), pch=c("o","o"))

plasma = data[,2]
age = data[,8]
diabetes = data[,9]
plot(age, plasma, col=ifelse(diabetes==1, "red", "green"), xlab="age", ylab="plasma glucose concentration", xlim=c(20,70))
legend(60,30,legend=c("diabetes","not diabetes"), col=c("red","green"), pch=c("o","o"))

#2
#training our model_glm
model_glm <- glm(diabetes ~ plasma + age, family="binomial", data = data[,c(2,8,9)])
predictions = model_glm[["fitted.values"]]

predictions = ifelse(predictions > 0.5, 1, 0) #threshold r = 0.5

plot(age, plasma, col=ifelse(predictions==1, "red", "green"), main="Diabetes predictions using r = 0.5", xlab="age", ylab="plasma glucose concentration")
legend(70,200,legend=c("diabetes","not diabetes"), col=c("red","green"), pch=c("o","o"))

confusionMatrix_r05 = table(diabetes, predictions)
misclassification_r05 = ( 1-sum(diag(confusionMatrix_r05))/length(diabetes))

#probabilistic equation of the estimated model_glm implemented in code, see lab report. 
#i.e. how the target depends on the features and the estimated model_glm parameters probabilistically
#so,a way to estimate whether diabetic or not given new input data x1,x2 and given our theta from the model. 
g <- function(plasma, age) {
  theta = model_glm[["coefficients"]]
  variables = c(1,plasma,age)
  g = exp( t(theta)%*%variables ) / (1 + exp(t(theta)%*%variables))
  return(g[1])
}
yHat_newData = g(100,59)


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

plot(age, plasma, col=ifelse(predictions==1, "red", "green"), main="Diabetes predictions using r = 0.2", xlab="age", ylab="plasma glucose concentration")
legend(70,200,legend=c("diabetes","not diabetes"), col=c("red","green"), pch=c("o","o"))

confusionMatrix_r02 = table(diabetes, predictions)
misclassification_r02 = ( 1-sum(diag(confusionMatrix_r02))/length(diabetes))
print(misclassification_r02)


#reset our predictions vector to its original values
predictions = model_glm[["fitted.values"]]

predictions = ifelse(predictions > 0.8, 1, 0)

plot(age, plasma, col=ifelse(predictions==1, "red", "green"), main="Diabetes predictions using r = 0.8", xlab="age", ylab="plasma glucose concentration")
legend(70,200,legend=c("diabetes","not diabetes"), col=c("red","green"), pch=c("o","o"))

confusionMatrix_r08 = table(diabetes, predictions)
misclassification_r08 = ( 1-sum(diag(confusionMatrix_r08))/length(diabetes))
print(misclassification_r08)


#5 
#Basis function expansion trick
z1 = plasma^4
z2 = (plasma^3)*age
z3 = (plasma^2)*(age^2)
z4 = plasma*(age^3)
z5 = age^4
dataExpanded = as.data.frame(cbind(plasma, age, z1, z2, z3, z4, z5, diabetes))

model_glm_expanded <- glm(diabetes ~., family="binomial", data = dataExpanded)

predictions = model_glm_expanded[["fitted.values"]]

predictions = ifelse(predictions > 0.5, 1, 0)


plot(age, plasma, col=ifelse(predictions==1, "red", "green"), main="Diabetes predictions using basis function expansion, r = 0.5", xlab="age", ylab="plasma glucose concentration")
legend(70,200,legend=c("diabetes","not diabetes"), col=c("red","green"), pch=c("o","o"))

confusionMatrix_basis_f_exp = table(diabetes, predictions)
misclassification_basis_f_exp = ( 1-sum(diag(confusionMatrix_basis_f_exp))/length(diabetes))


theta_expanded = model_glm_expanded[["coefficients"]]