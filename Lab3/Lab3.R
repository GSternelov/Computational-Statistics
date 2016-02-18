
## Assignment 1 ##
library(ggplot2)
library(scales)
require(XLConnect)
wb = loadWorkbook("C:/Users/Gustav/Documents/Computational-Statistics/Lab3/population.xls")
df = readWorksheet(wb, sheet = "Table", header = FALSE)

data1 <- df[, 1:3]
names(data1) <- c("cityCode", "City", "Population")
data1 <- subset(data1, data1$cityCode > 30)

cityFunc <- function(data){
  data$prob <- data$Population / sum(data$Population)
  data$cumProbs <- cumsum(data$prob)
  unifV <- runif(1,0,1)
  dataNew <- subset(data, data$cumProbs >= unifV)
  cityNum <- which(data$cumProbs==min(dataNew$cumProbs))
  return(cityNum)
}

test <- data1
newData <- as.data.frame(matrix(seq(20),nrow=20,ncol=3))
for(i in 1:20){
  set.seed(i)
  cityNum <- cityFunc(test)
  newData[i, ] <- test[cityNum, 1:3]
  test <- test[-cityNum, ]
}

ggplot(data1, aes(Population)) + geom_histogram(binwidth=10000, fill="darkorange", col="darkorange") +
  scale_x_continuous(labels=comma) + theme(panel.grid.minor=element_blank())
ggplot(newData, aes(V3)) + geom_histogram(binwidth=10000, fill="darkorange", col="darkorange") +
  scale_x_continuous(labels=comma) + theme(panel.grid.minor=element_blank()) + labs(x = "Population")


## Assignment 2 ## 
# 10 000 random uniform values
set.seed(910814)
randoms <- runif(10000, 0, 1)
# The uniformed values are transformed by using the inverse CDF method.
# The transformed values follows a DE(0,1) distribution.
DEval <- data.frame(V1=log(2*randoms) - log(2-2*randoms))
set.seed(910814)
NormTest <- data.frame(V1=rnorm(10000, 0, 1))

max(NormTest$V1 / DEval$V1)

DEtest2v2 <- data.frame(V1=DEval$V1 * 0.61)

library(smoothmest)
DEsmooth <- data.frame(V1=rdoublex(10000,mu=0,lambda=1))


ggplot(DEval, aes(V1,..density..)) + geom_histogram(fill="blue", alpha=0.2,binwidth=0.3) + 
  scale_y_continuous(labels = percent_format()) +
  geom_freqpoly(size=1, col="darkblue",binwidth=0.3) +
  geom_histogram(data = DEtest2v2, fill = "green", alpha = 0.2,binwidth=0.3) +
  geom_freqpoly(data = DEtest2v2,size=1, col="darkgreen",binwidth=0.3) +
  geom_histogram(data = NormTest, fill = "red", alpha = 0.2,binwidth=0.3)+
  geom_freqpoly(data = NormTest,size=1, col="darkred",binwidth=0.3) +
  geom_freqpoly(data = DEsmooth,size=1, col="darkgreen",binwidth=0.3)  
  

testVec <- c(0.01, 0.25, 0.5, 0.75, 0.99)
DEtest <- log(2*testVec) - log(2-2*testVec)
1/2 * exp(-1 * abs(DEtest-0))

## 2.2
# c cannot be less than 1

# A uniform value
set.seed(123)
runif(1,0,1)
# The DE(0,1) value
log(2*0.2875775) - log(2-2*0.2875775)
# The normal density value
( 1 /(sqrt(2*pi)) ) * exp((-0.2875775^2)/2)

# tests c equal to 1.639344
0.2875775 <= ( 0.3827823 / (1.639344 * 0.9071787))


c <- 0.07
i <- 0
j <- 1
normVal <- data.frame(V1=0)
while (length(normVal[,1])<2000) {
  i <- i +1
  set.seed(i)
  U <- runif(1,0,1)
  set.seed(i)
  theta <- runif(1, 0, 2*pi)
  #if(i %% 2 == 1){
    normDens <- sqrt(-2*log(U))*cos(theta)
  #}else{
   # normDens <- sqrt(-2*log(U))*sin(theta)
  #}
  #set.seed(i)
  #normDens <- rnorm(1,0,1)
  deDens <- log(2*U) - log(2-2*U)
  if(U <= (normDens / (c*deDens)) ){
    normVal[j,] <- deDens
    j <- j+1
  } else{
    j <- j
  }
}
set.seed(910814)
NormTest2 <- data.frame(V1=rnorm(2000, 0, 1))
set.seed(910814)
randoms2 <- runif(2000, 0, 1)
DEval2 <- data.frame(V1=log(2*randoms2) - log(2-2*randoms2))
ggplot(normVal, aes(V1)) + 
  geom_freqpoly(size=1, col="darkblue",binwidth=0.3) +
  geom_freqpoly(data = NormTest2,size=1, col="darkred",binwidth=0.3) +
  geom_freqpoly(data = DEval2,size=1, col="darkgreen",binwidth=0.3) 


normValTest <- data.frame(V1=0)
for(i in 1:2000){
  set.seed(i)
  U <- runif(1,0,1)
  set.seed(i)
  theta <- runif(1, 0, 2*pi)
  if(i %% 2 == 1){
    normValTest[i,] <- sqrt(-2*log(U))*cos(theta)
  }else{
    normValTest[i,] <- sqrt(-2*log(U))*sin(theta)
  }
}

ggplot(normValTest, aes(V1)) + 
  geom_freqpoly(size=1, col="darkblue",binwidth=0.3) +
  geom_freqpoly(data = NormTest2,size=1, col="darkred",binwidth=0.3)

c <- 0.10
i <- 0
j <- 1
DistVal <- data.frame(V1=0)
while (length(DistVal[,1])<2000) {
  # Generate y
  randU <- runif(1, 0, 1)
  DEy<- log(2*randU) - log(2-2*randU)
  # Generate U
  U <- runif(1, 0, 1)
  # What is fx(Y)?
  # Should I insert Y into the pdf for fx?
  fx_y <- ( 1/sqrt(2*pi) ) * exp(-0.5*DEy^2)
  # Then, if U <= fx_y/c*DEy, take y, else return to first step
  if (U <= (fx_y/(c*DEy)) ) {
    DistVal[j,] <- DEy
    j <- j+1
  }else{
    j <- j
  }  
}
ggplot(DistVal, aes(V1)) + 
  geom_freqpoly(size=1, col="darkblue",binwidth=0.2)  +
  geom_freqpoly(data = NormTest2,size=1, col="darkred",binwidth=0.3)
  

