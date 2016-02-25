library(coda)
library(ggplot2)
library(gridExtra)

f_x <- function(x){
  thaVal <- x^5*exp(-x)
  return(thaVal)
}
x_ta <- 2
set.seed(311015)
for(i in  1:10000){
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
x_ta <- data.frame(y=x_ta, x=1:10001)
ggplot(x_ta, aes(y=y, x=x)) + geom_line(size=1.15)
hist(x_ta$y, freq = FALSE)
ggplot(x_ta, aes(y)) + geom_histogram(binwidth=0.25) + 
  geom_freqpoly(binwidth=0.25, col="darkorange", size=1.5)

time_ln <- ggplot(x_ta, aes(y=y, x=x)) + geom_line(size=1.15)
hist_ln <- ggplot(x_ta, aes(y)) + geom_histogram(binwidth=0.25) + 
  geom_freqpoly(binwidth=0.25, col="darkorange", size=1.5)
grid.arrange(time_ln, hist_ln, ncol=2)

x_tb <- 1
for(i in  1:1000){
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
plot(x_tb, type = "l")
hist(x_tb)



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


## 1.5
# Step 1
mean(x_ta)
# Step 2
mean(x_tb)

## 1.6
6*1
# Expected density for gamma(6,1)
Gamm <- data.frame(y=dgamma(0:18, 6, 1))
ggplot(Gamm, aes(x=0:18, y=y)) +geom_area(fill="purple", alpha=0.5)
ggplot(x_tb, aes(y,..density..)) + geom_histogram(binwidth=0.95, fill="purple", alpha=0.5) +
  geom_area(data=Gamm,aes(x=0:18, y=y), fill="orange", alpha=0.5)

### Assignment 2 ###
chemic <- data.frame(load("C:/Users/Gustav/Documents/Computational-Statistics/Lab4/chemical.RData"))
chemic <- data.frame(x=X, y=Y)
ggplot(chemic, aes(x=X, y=Y)) + geom_point() + geom_smooth(method="lm",formula = y ~ x + I(x^2))
ggplot(chemic, aes(x=X, y=Y)) + geom_point() + geom_smooth(method="lm",formula = y ~ log(x))

cov(chemic)
cor(chemic)
