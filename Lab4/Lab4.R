library(coda)
library(ggplot2)
library(gridExtra)
f_x <- function(x){
  thaVal <- x^5*exp(-x)
  return(thaVal)
}
x_ta <- 2
set.seed(311015)
for(i in  1:9999){
  Y_point <- rlnorm(1, x_ta[i], 1)
  U_point <- runif(1, 0, 1)
  q_x <- dlnorm(x = x_ta[i], meanlog = Y_point, 1)
  q_y <- dlnorm(x = Y_point, meanlog = x_ta[i], 1)
  alpha <- min(c(1, ((f_x(Y_point) * q_x )  / 
                       (f_x(x_ta[i]) * q_y))))
  if (U_point <= alpha) {
    x_ta[i+1] <- Y_point
  }else{
    x_ta[i+1] <- x_ta[i]
  }
}
x_ta <- data.frame(y=x_ta, x=1:10000)
time_ln <- ggplot(x_ta, aes(y=y, x=x)) + geom_line(size=1.15)
time_ln
set.seed(311015)
x_tb <- 2
for(i in  1:9999){
  Y_point <- rchisq(1, floor(x_tb[i] +1))
  U_point <- runif(1, 0, 1)
  q_x <- dchisq(x_tb[i], floor(Y_point+1))
  q_y <- dchisq(Y_point, floor(x_tb[i]+1))
  alpha <- min(c(1, ((f_x(Y_point) * q_x )  / 
                       (f_x(x_tb[i]) * q_y))))
  if (U_point <= alpha) {
    x_tb[i+1] <- Y_point
  }else{
    x_tb[i+1] <- x_tb[i]
  }
}
x_tb <- data.frame(y=x_tb, x=1:10000)
time_chi <- ggplot(x_tb, aes(y=y, x=x)) + geom_line(size=1) +geom_vline(xintercept=200, col="darkorange", size=1.05)
time_chi
## 1.4 ##
x_xt <- as.data.frame(matrix(seq(10001),nrow=10001,ncol=10))
for(j in 1:10){
  x_t <- j
  t <- 0
  for(i in  1:10000){
    Y_point <- rchisq(1, floor(x_t[i] +1))
    U_point <- runif(1, 0, 1)
    q_x <- rchisq(1, floor(x_t[i]+1))
    q_y <- rchisq(1, floor(Y_point+1))
    alpha <- min(c(1, ((f_x(Y_point) * q_x )  / 
                         (f_x(x_t[i]) * q_y))))
    if (U_point <= alpha) {
      x_t[i+1] <- Y_point
    }else{
      x_t[i+1] <- x_t[i]
    }
  }
  x_xt[, j] <- x_t
}

f=mcmc.list()
for (i in 1:10) f[[i]]=as.mcmc(x_xt[,i])
gelman.diag(f)
Gamm <- data.frame(y=dgamma(0:18, 6, 1))
ggplot(x_tb[210:10000,], aes(y,..density..)) + geom_histogram(binwidth=0.95, fill="red", alpha=0.4) +
  geom_area(data=Gamm,aes(x=0:18, y=y), fill="springgreen2", alpha=0.4)
chemic <- data.frame(load("C:/Users/Gustav/Documents/Computational-Statistics/Lab4/chemical.RData"))
chemic <- data.frame(x=X, y=Y)
ggplot(chemic, aes(x=X, y=Y)) + geom_point() + geom_smooth(method="lm",formula = y ~ x + I(x^2), size=1.1, col="springgreen3") + geom_smooth(method="lm",formula = y ~ log(x), col="red", size=1.1)
sigma2 <- 0.2
y <- Y
mu_update <- as.data.frame(matrix(seq(1000),nrow=1000,ncol=50))
mu_update[,1:50] <- 0
  for(j in 1:1000){
    if(j == 1){
      mu_update[j,1] <- rnorm(1, mean=(y[1]+mu_update[j,2])/2, sd = sqrt(sigma2)/2)
      for(h in 2:49){
        mu_update[j,h] <- rnorm(1, mean=(mu_update[j,h-1]+y[h]+mu_update[j,h+1])/3, sd = sqrt(sigma2)/3)
      }
      mu_update[j,50] <- rnorm(1, mean=(y[50]+mu_update[j,49])/2, sd = sqrt(sigma2)/2)  
    }else{
      mu_update[j,1] <- rnorm(1, mean=(y[1]+mu_update[j-1,2])/2, sd = sqrt(sigma2)/2)
      for(h in 2:49){
        mu_update[j,h] <- rnorm(1, mean=(mu_update[j,h-1]+y[h]+mu_update[j-1,h+1])/3, sd = sqrt(sigma2)/3)
      }
      mu_update[j,50] <- rnorm(1, mean=(y[50]+mu_update[j,49])/2, sd = sqrt(sigma2)/2)
    }    
  }
mu_updateSum <- data.frame(x=colMeans(mu_update))
ggplot(mu_updateSum, aes(y=x, x = 1:50)) + geom_point(size=3, col="red") + geom_line(size=1.25, col="red") +
  geom_point(data=chemic, aes(x=x, y=y), col="springgreen3", size=3) + geom_line(data=chemic, aes(x=x, y = y), col="springgreen3", size=1.25)
mu_50 <- data.frame(y=mu_update[, 50])
ggplot(mu_50, aes(y=y, x = 1:1000))+geom_line() + geom_vline(xintercept = 20, col="darkorange", size=1.05)
## NA
