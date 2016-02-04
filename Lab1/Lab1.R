
### Assignment 1 ###
x1<-1/3
x2<-1/4
if (round(x1-x2,8)==round(1/12,8)){
  print("Teacher said true")
} else{
  print("Teacher lied")
}
# The program yields the wrong answer
# Problem has to do with the magnitude.
# Guess this also happens becaues '==' looks for the exactly "exact" equal number (?).

# Can be solved by specifying the number of decimals.

### Assignment 2 ###
AssigTwo <- function(x, epsilon){
  f_prim <- ((x+epsilon) - x)/ epsilon
  return(f_prim)
}
AssigTwo(x=round(100000, i), epsilon=round(10^-15,i))

# The value obtained is 0.
# The real value is 1.
# The reason behind this is the catastrophic cancellation?
# Bad round-off?

### Assignment 3 ### 
myvar <- function(x, n){
 varX <-  1/(n-1) * (sum(x^2) - (1/n)*(sum(x)^2))
 return(varX)
}
randVec <- rnorm(n = 50000, mean = 10^8, sd = 1)
y <- 0
for(i in 1:50000){
  y[i] <- myvar(randVec[1:i], n=i) - var(randVec[1:i])
}
plot(y=y[2:50000], x=2:50000, ylab="myvar-var", xlab="X")
y <- 0
for(i in 1:100){
  y[i] <- myvar(randVec[1:i], n=i)
}
plot(y=y[2:100], x=2:100, ylab="myvar-var", xlab="X")



### Assignment 4 ###
tecat <- read.csv("C:/Users/Gustav/Documents/Machine-Learning/Lab 2/tecator.csv", sep = ";")
names(tecat)
# Protein, column 103 is the target variable
Xvar <- data.frame(x1=1, tecat[,-c(1,103)])
A <- t(as.matrix(Xvar)) %*% as.matrix(Xvar)
b <- t(as.matrix(Xvar)) %*% as.matrix(tecat$Protein)

# Step 3 and 4
solve(A,b) 

det(A) 
det(solve(A,tol = 10^-18))
det(A)%*%det(solve(A))
norm(A)%*%norm(solve(A))
kappa(A)

# Step 5, scale data and redo step 3 and 4
tecatSc <- data.frame(scale(intercept = tecat, center = TRUE, scale = TRUE))
XvarXc <- data.frame(x=1, tecatSc[,-c(1,103)])
ASc <- t(as.matrix(XvarXc)) %*% as.matrix(XvarXc)
bSc <- t(as.matrix(XvarXc)) %*% as.matrix(tecatSc$Protein)

solve(ASc,bSc) 
det(ASc) 
det(solve(ASc))
norm(ASc)%*%norm(solve(ASc))
kappa(ASc)

