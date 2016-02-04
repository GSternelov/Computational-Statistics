### Assignment 1 ###
x1<-1/3
x2<-1/4
if (x1-x2==1/12){
  print("Teacher said true")
} else{
  print("Teacher lied")
}
x1<-1/3
x2<-1/4
if (round(x1-x2,8)==round(1/12,8)){
  print("Teacher said true")
} else{
  print("Teacher lied")
}
AssigTwo <- function(x, epsilon){
  f_prim <- ((x+epsilon) - x)/ epsilon
  return(f_prim)
}
AssigTwo(x=100000, epsilon=10^-15)
myvar <- function(x, n){
 varX <-  1/(n-1) * (sum(x^2) - (1/n)*(sum(x)^2))
 return(varX)
}
randVec <- rnorm(n = 10000, mean = 10^8, sd = 1)
y <- 0
for(i in 1:10000){
  y[i] <- myvar(randVec[1:i], n=i) - var(randVec[1:i])
}
plot(y=y[2:10000], x=2:10000, pch=21, bg="orange", col="darkorange", ylab="myvar-var", xlab="X",
     main="Difference between myvar and var")
tecat <- read.csv("C:/Users/Gustav/Documents/Machine-Learning/Lab 2/tecator.csv", sep = ";")
# Protein, column 103 is the target variable
Xvar <- data.frame(x1=1, tecat[,-c(1,103)])
A <- t(as.matrix(Xvar)) %*% as.matrix(Xvar)
b <- t(as.matrix(Xvar)) %*% as.matrix(tecat$Protein)
## solve(A,b)
kappa(A)
tecatSc <- data.frame(scale(x = tecat, center = TRUE, scale = TRUE))
XvarXc <- data.frame(x=1, tecatSc[,-c(1,103)])
ASc <- t(as.matrix(XvarXc)) %*% as.matrix(XvarXc)
bSc <- t(as.matrix(XvarXc)) %*% as.matrix(tecatSc$Protein)

# solve(ASc,bSc)
# The condition number is given by
# kappa(ASc)
head(solve(ASc,bSc), 10)
## 
