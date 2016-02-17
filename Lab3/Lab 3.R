require(XLConnect)
wb = loadWorkbook("C:/Users/Gustav/Documents/Computational-Statistics/Lab3/population.xls")
df = readWorksheet(wb, sheet = "Table", header = FALSE)

data1 <- df[, 1:3]
names(data1) <- c("cityCode", "City", "Population")
data1 <- subset(data1, data1$cityCode > 30)
data1$prob <- data1$Population / sum(data1$Population)


cityFunc <- function(data){
  cumProbs <- cumsum(data$prob)
  unifV <- runif(1,0,1)
  cityNum <- which(abs(cumProbs-unifV)==min(abs(cumProbs-unifV)))
  newData <- data[-cityNum, ]
  return(newData)
}

test <- cityFunc(test)

## Alternatively 
cityFunc2 <- function(data, cumP){
  unifV <- runif(1,0,1)
  cityNum <- which(abs(cumProbs-unifV)==min(abs(cumProbs-unifV)))
  Test <- cityNum/length(data[,1])
  List <- list(num=cityNum, test=Test)
  return(List)
}
MyData <- data1[-cityFunc2(data1), ]

# An example
testData <- data1
i <- 0
testi <- 0
while (length(testData[,1])> 20) {
  i <- i+1
  testData$prob <- testData$Population / sum(testData$Population)
  cumProbs <- cumsum(testData$prob)
  Num <- cityFunc2(testData, cumProbs)
  testi[i] <- Num$num
  testData <- testData[-Num$num, ]
}
plot(y=testi,x=21:2 , type="l")

hist(data1$Population, breaks = 20)
hist(testData$Population, breaks=10)


Val <- cityFunc2(testData)
testData <- testData[-Val, ]



cumsum(data1$prob)
Val <- runif(1, 0, 1)
which(abs(cumsum(data1$prob)-Val)==min(abs(cumsum(data1$prob)-Val)))

findInterval(Val, cumsum(data1$prob))

Valu <- 0
for(i in 1:290){
  Valu[i] <- runif(1, 0, 1)
  Val <- which(abs(cumsum(data1$prob)-Val)==min(abs(cumsum(data1$prob)-Val)))
}
hist(Valu)
plot(x=data1$Population, y=Valu)


