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
selectData <- data1
newData <- as.data.frame(matrix(seq(20),nrow=20,ncol=3))
for(i in 1:20){
  set.seed(i)
  cityNum <- cityFunc(selectData)
  newData[i, ] <- selectData[cityNum, 1:3]
  selectData <- selectData[-cityNum, ]
}
options(scipen = 999)
Cities <- newData[, 2:3]
names(Cities) <- c("City", "Population")
Cities
library(gridExtra)
All <- ggplot(data1, aes(Population)) + geom_histogram(binwidth=10000, fill="darkorange", col="darkorange") + ggtitle("Population of all swedish cities") + 
  scale_x_continuous(labels=comma) + theme(panel.grid.minor=element_blank())
Sel <- ggplot(newData, aes(V3)) + geom_histogram(binwidth=10000, fill="darkorange", col="darkorange") +
  scale_x_continuous(labels=comma) + theme(panel.grid.minor=element_blank()) + labs(x = "Population") + ggtitle("Population of the selected cities")
grid.arrange(All, Sel, ncol=2)
## Assignment 2 ## 
# 10 000 random uniform values
set.seed(190216)
randoms <- runif(10000, 0, 1)
randIndex <- randoms < 0.5
# The uniformed values are transformed by using the inverse CDF method.
# The transformed values follows a DE(0,1) distribution.
DEval1 <- data.frame(V1=log(2*randoms[randIndex]))
DEVal2 <- data.frame(V1= -log(2-2*randoms[!randIndex]))
DEval <- data.frame(rbind(DEval1, DEVal2))
set.seed(160219)
NormTest <- data.frame(V1=rnorm(10000, 0, 1))
ggplot(DEval, aes(V1,..density..)) + geom_histogram(fill="blue", alpha=0.2,binwidth=0.3) + 
  scale_y_continuous(labels = percent_format()) +
  geom_freqpoly(size=1.5, col="darkblue",binwidth=0.3) +
  geom_freqpoly(data = NormTest,size=1.5, col="darkred",binwidth=0.3)
c <- 1.315489
i <- 0
j <- 1
y <- data.frame(V1=0)
set.seed(311015)
while (length(y[,1])<2000) {
  i <- i+1
  #set.seed(i)
  Utest <- runif(1,0,1)
  if(Utest < 0.5){
    DEy <- log(2*Utest)
  }else{
    DEy <- -log(2-2*Utest)
  }
  fy_y <- 1/2 * exp(-1 * abs(DEy-0))
  fx_y <- dnorm(DEy, 0, 1)
  #set.seed(i+10)
  Uselect <- runif(1,0,1)
  if (Uselect <= fx_y/(c*fy_y)) {
    y[j,] <- DEy
    j <- j+1
  }else{
    j <- j
  }
}
set.seed(311015)
NormTest <- data.frame(V1=rnorm(2000, 0, 1))
ARnm <- ggplot(y, aes(V1,..density..)) + geom_histogram(fill="blue", alpha=0.2,binwidth=0.5) + 
  scale_y_continuous(labels = percent_format()) +
  geom_freqpoly(size=1.5, col="darkblue",binwidth=0.5)
RNnm <- ggplot(y, aes(V1,..density..)) + geom_histogram(fill="blue", alpha=0.2,binwidth=0.5) + 
  scale_y_continuous(labels = percent_format()) +
  geom_freqpoly(size=1, col="darkblue",binwidth=0.5) +
  geom_histogram(data = NormTest, fill = "red", alpha = 0.2,binwidth=0.5)+
  geom_freqpoly(data = NormTest,size=1, col="darkred",binwidth=0.5)
grid.arrange(ARnm,RNnm, ncol=2)

## NA
