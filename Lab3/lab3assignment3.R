library(neuralnet)
library(ggplot2)
set.seed(1234567890)

#generate 500 points that are uniformly distributed, min=0 max=10 indicate U[0,10],
generatedVars <- runif(n=500, min=0, max=10)

#apply sin function to each variable
#data2 <-data.frame(generatedVars, sin(generatedVars))
data <-data.frame("x" = generatedVars, "sin" = sin(generatedVars)) #same as above, but name columns here!

#don't need to shuffle or set seed  since data is already random from generation
train <- data[1:25,]
test <- data[26:500,]

# Random initialization of the weights in the interval [-1, 1]
#w1=10, b1=10, w2=10, b2=1, so we get 10+10+10+1
winit <- runif(10+10+10+1, -1, 1)
#hidden = c(10) - one layer with 10 hidden units. c(30,20,10) - would mean 3 layers, with 30, 20 and 10 hidden units each.
#default activation function is logistic/sigmoid, h(x) = 1 / (1 + e^-x)
nn <- neuralnet(sin ~. , data = train, startweights = winit, hidden = c(10))

#Plot of the training data (black), test data (blue), and predictions on test data(red)
#cex changes width of dots. 
plot(train, cex=2, main="logistic/sigmoid (default) activation function") 
points(test, col = "blue", cex=1) #plot test points
#plot x from test data and predicted sin(x)
pred = predict(nn, test)
points(test[,1], pred, col="red", cex=1)
legend(x = "bottomleft", cex=1, legend=c("training data", "test data", "predictions on test data"), col=c("black","blue", "red"),pch=c("o","o", "o"))

MSElogistic = 1/nrow(test) * sum((test[,2]-pred)^2)
MSElogistic

#2
#act.fct: a differentiable function that is used for smoothing the result of the cross product of the covariate
#or neurons and the weights. Additionally the strings, 'logistic' and 'tanh' are possible for the logistic
#function and tangent hyperbolicus.

#LINEAR:  h(x) = x  
linear <- function(x) {
  x
}
nn_linear <- neuralnet(sin ~. , data = train, startweights = winit, hidden = c(10), act.fct=linear)

plot(train, cex=2, main="linear h(x) = x activation function") 
points(test, col = "blue", cex=1) 
pred = predict(nn_linear, test)
points(test[,1], pred, col="red", cex=1)
legend(x = "bottomleft", cex=1, legend=c("training data", "test data", "predictions on test data"), col=c("black","blue", "red"),pch=c("o","o", "o"))

MSElinear = 1/nrow(test) * sum((test[,2]-pred)^2)
MSElinear

#ReLU:  h(x) = max{0,x}
relu <- function(x) {
  ifelse(x>0, x, 0)
  #max(0,x)
}
nn_relu <- neuralnet(sin ~. , data = train, startweights = winit, hidden = c(10), act.fct=relu)

plot(train, cex=2, main="ReLU h(x) = max(0,x) activation function", ylim=c(-1,1.25)) #cex changes width of dots. Plot train points
points(test, col = "blue", cex=1) #plot test points
pred = predict(nn_relu, test)
points(test[,1], pred, col="red", cex=1)
legend(x = "bottomleft", cex=1, legend=c("training data", "test data", "predictions on test data"), col=c("black","blue", "red"),pch=c("o","o", "o"))

MSErelu = 1/nrow(test) * sum((test[,2]-pred)^2)
MSErelu

#softplus:  h(x) = ln(1+exp(x))
softplus <- function(x) {
  log(1 + exp(x))
}
nn_softplus <- neuralnet(sin ~. , data = train, startweights = winit, hidden = c(10), act.fct = softplus)

plot(train, cex=2, main="softplus h(x) = log(1+exp(x)) activation function") #cex changes width of dots. Plot train points
points(test, col = "blue", cex=1) #plot test points
pred = predict(nn_softplus, test)
points(test[,1], pred, col="red", cex=1)
legend(x = "bottomleft", cex=1, legend=c("training data", "test data", "predictions on test data"), col=c("black","blue", "red"),pch=c("o","o", "o"))

MSEsoftplus = 1/nrow(test) * sum((test[,2]-pred)^2)
MSEsoftplus

curve(relu, from=-5, to=5)
curve(softplus, from=-5, to=5)

#3
#generate 500 new points U~[0,50]
generatedVars <- runif(n=500, min=0, max=50)
newTest <-data.frame("x" = generatedVars, "sin" = sin(generatedVars))

#Plot of test data (blue), and predictions on test data(red)
plot(newTest, col = "blue", cex=1,ylim=c(-11,1)) 
newPred = predict(nn, newTest)
points(newTest[,1], newPred, col="red", cex=1)
legend(x = "bottomleft", cex=1, legend=c("test data", "predictions on test data"), col=c("blue", "red"),pch=c("o", "o"))


#4
#gwplot(nn)
#?plot.nn()
#rep: repetition of the neural network. If rep="best", the repetition with the smallest error will be plotted. 
#If not stated all repetitions will be plotted, each in a separate window.
plot(nn, rep="best")

#this vs generalized.weights??
#extracting the weights from our NN model
nn$weights
layer1 = nn$weights[[1]][[1]]
layer2 = nn$weights[[1]][[2]]
w1 = layer1[2,]
b1 = layer1[1,]
w2 = layer2[2:11]
b2 = layer2[1]

#function that prints the output values from the hidden units, 
#and the predicted Sin-value using our models weights
converge <- function(x) {
  sigmoided = (1 / (1 + exp(- x%*%w1+b1)))
  print(sigmoided,) #1 or 0
  sin = (sigmoided)%*%(w2)+b2
  print(sin)
}
  
converge(50)
converge(100)
converge(200)
converge(300)
converge(400)
converge(500)
converge(1000)
converge(10000)


#5
generatedVars <- runif(n=500, min=0, max=10)
train500 <-data.frame("x" = generatedVars, "sin" = sin(generatedVars))

nn2 <- neuralnet(x ~. , data = train500, startweights = winit, hidden = c(10), threshold = 0.1)

plot(train500[,2], train500[,1], xlab="sin", ylab="x", col = "blue", cex=1, xlim=c(-1,1), ylim=c(0,10)) 
#plot sin(x) and predicted x
pred2 = predict(nn2, train500)
points(train500[,2], pred2, col="red", cex=1)
legend(x = "bottomleft", cex=1, legend=c("test data", "predictions on test data"), col=c("blue", "red"),pch=c("o", "o"))

plot(train500[,2], train500[,1], xlab="sin", ylab="x", col = "blue", cex=1, xlim=c(-1,1), ylim=c(0,10)) 
#plot sin(x) and predicted x
points(train500[,2], pred2, col="red", cex=1)
abline(v=-0.5, col="red")
abline(v=0, col="red")
legend(x = "bottomleft", cex=1, legend=c("test data", "predictions on test data"), col=c("blue", "red"),pch=c("o", "o"))


