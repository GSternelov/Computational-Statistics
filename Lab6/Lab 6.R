library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)
## Assignment 1
# 1.1
f_x <- function(x){
  ThaValo <- (x^2 / exp(x)) - 2* exp( (-9* sin(x)) / (x^2+x+1)) 
  return(ThaValo)
}

# 1.2 
crossover <- function(x, y){
    kiddo <- (x+y) / 2
    return(kiddo)
}

# 1.3
Mutate <- function(x){
  mutato <- (x^2) %% 30
  return(mutato)
}

# 1.4
a_func <- function(maxiter, mutprob){
  fx <- f_x(0:30)
  p <- ggplot(data.frame(fx), aes(x=0:30, y=fx)) + geom_line(col="royalblue", size=1.1) + ylim(-2.5, 0.25)
  popuX <- seq(0, 30, 5)
  Values <- f_x(popuX)
  MaxVal <- 0
  for (i in 1:maxiter){
    parents <- sample(popuX, size = 2, replace = FALSE)
    Order <- order(Values)[1]
    victim <- popuX[Order]
    newKiddo <- crossover(parents[1], parents[2])
    probiVal <- runif(1,0,1)
    if(probiVal <= mutprob){
      newKiddo <- Mutate(newKiddo)
    }else {newKiddo <- newKiddo}
    popuX[Order] <- newKiddo
    Values[Order] <- f_x(popuX[Order])
    MaxVal[i] <- max(Values)
  }
  p + geom_point(data=data.frame(y=Values,x=popuX), aes(x=x, y=y), size=3, col="darkorange")
}
# 1.5
set.seed(103115)
a1 <- a_func(10, 0.1) + ggtitle("maxiter=10, mutprob=0.1")
a2 <- a_func(10, 0.5) + ggtitle("maxiter=10, mutprob=0.5")
a3 <- a_func(10, 0.9) + ggtitle("maxiter=10, mutprob=0.9")
b1 <- a_func(100, 0.1) + ggtitle("maxiter=100, mutprob=0.1")
b2 <- a_func(100, 0.5) + ggtitle("maxiter=100, mutprob=0.5")
b3 <- a_func(100, 0.9) + ggtitle("maxiter=100, mutprob=0.9")
grid.arrange(a1,a2,a3, b1,b2,b3, ncol=2)



## Assignment 2
physi <- read.csv("C:/Users/Gustav/Documents/Computational-Statistics/Lab6/physical.csv")

# 2.1
Physi2 <- melt(physi, id=c("X"))
ggplot(Physi2) + geom_line(aes(x=X, y=value, colour=variable),size=1.05) +
  scale_colour_manual(values=c("darkorange","royalblue")) + 
  theme(legend.position=c(0.95,0.85),legend.key.size = unit(1.5, "cm"))

# 2.2






# 2.3

emExp <- function(data, lambdaZ, stopC){
  X <- data$X
  Y <- data$Y
  Z <- data$Z
  Zobs <- Z[!is.na(Z)]
  Zmiss <- Z[is.na(Z)]
  n <- length(c(Zobs, Zmiss))
  r <- length(Zobs)
  # Initial values
  
  # log-like function
  
}


em.norm <- function(Y){
  Yobs <- Y[!is.na(Y)]
  Ymiss <- Y[is.na(Y)]
  n <- length(c(Yobs, Ymiss))
  r <- length(Yobs)
  # Initial values
  mut <- 1
  sit <- 0.1
  # Define log-likelihood function
  ll <- function(y, mu, sigma2, n){
    -0.5*n*log(2*pi*sigma2)-0.5*sum((y-mu)^2)/sigma2
  }
  # Compute the log-likelihood for the initial values
  lltm1 <- ll(Yobs, mut, sit, n)
  repeat{
    # E-step
    EY <- sum(Yobs) + (n-r)*mut
    EY2 <- sum(Yobs^2) + (n-r)*(mut^2 + sit)
    # M-step
    mut1 <- EY / n
    sit1 <- EY2 / n - mut1^2
    # Update parameter values
    mut <- mut1
    sit <- sit1
    # Compute log-likelihood using current estimates
    llt <- ll(Yobs, mut, sit, n)
    # Print current parameter values and likelihood
    cat(mut, sit, llt, "\n")
    # Stop if converged
    if ( abs(lltm1 - llt) < 0.001) break
    lltm1 <- llt
  }
}

