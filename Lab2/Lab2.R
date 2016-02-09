
## Assignment 1 ## 
# 1.1
mort_rate<-read.csv2("C:\\Users\\Gustav\\Documents\\Computational-Statistics\\Lab2\\mortality_rate.csv",sep=";")
mort_rate$LMR <- log(mort_rate$Rate)

n=dim(mort_rate)[1]
set.seed(123456)
id=sample(1:n, floor(n*0.5))
train=mort_rate[id,]
test=mort_rate[-id,]

# 1.2
Data <- list(X=train$Day, Y=train$LMR, Xtest=test$Day, Ytest=test$LMR)

myMSE <- function(lambda, pars){
  model <- loess(pars$Y~pars$X, data=pars[1:2], enp.target = lambda)
  Pred <- predict(model, newdata=pars$Xtest)
  MSE <- (1/length(pars$Y)) * sum((Pred-pars$Ytest)^2)
  print(MSE)
  return(MSE)
}
# 1.3 
Lambda <-  seq(0.1,40, by=0.1)
modelMSE <- 0
j <- 0
for(i in Lambda){
  j <- j+1
  modelMSE[j] <- myMSE(Lambda[j], Data)
}

library(ggplot2)
MSEdata <- data.frame(MSE=modelMSE, lambda=Lambda)  
ggplot(MSEdata, aes(x=lambda, y=MSE, label=lambda)) + geom_line(col="darkblue")+
  geom_text(data=subset(MSEdata, MSE <= min(MSEdata$MSE))[1,],vjust=-0.6, size=10) + 
  geom_point(data=subset(MSEdata, MSE <= min(MSEdata$MSE))[1,], size=5, col="darkorange")

# 1.4
Optim <- optimize(f=myMSE, interval = c(0.1, 40), tol=0.01, pars=Data)

# 1.5
optim(par=35, fn=myMSE, method = "BFGS", pars=Data)


## Assignment 2 ##

# 2.1
load("C:\\Users\\Gustav\\Documents\\Computational-Statistics\\Lab2\\data.RData")
Data2 <- data

# 2.2
# Log likelihood
#(-n/2) * log(2*pi*sigma2) - (1/2*sigma2)*sum(x-my)^2
# Maximum likelihood estimates
# my
MeanEst <- sum(Data2) / length(Data2)

# sigma2 
(1/length(Data2)) * sum((Data2-MeanEst)^2)


# 2.3


# 2.4
# minus log-like function
loglikef <- function(par,...){
  my <- par[1]
  sigma <- par[2]
  n <- length(data)
  loglike <- -(n*log(1/(sqrt(2*pi*sigma^2))) - (sum((data-my)^2)/(2*sigma^2)))
  return(loglike)
}
loglikef(c(0,1),Data2)

gradientf <- function(par,...){
  n <- length(data)
  my <- par[1]
  sigma <- par[2]
  myML <-  - (2*sum(data-my)) / (2*sigma^2)
  mySigma <- - (n/2)*(2/sigma)+ 2*sigma - (2*sigma^2)^-2 * (-2*sum(data-my)) 
  return(c(myML, mySigma))
}

optim(par = c(0,1), fn=loglikef, method = "CG", data=Data2)
optim(par = c(0,1), fn=loglikef, gr=gradientf,  method = "CG", data=Data2)

optim(par = c(0,1), fn=loglikef , method = "BFGS", data=Data2)
optim(par = c(0,1), fn=loglikef, gr=gradientf,  method = "BFGS", data=Data2)




