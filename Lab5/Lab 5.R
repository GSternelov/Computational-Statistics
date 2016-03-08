library(ggplot2)
require(XLConnect)
### Assignment 1 ### 
wb = loadWorkbook("C:/Users/Gustav/Documents/Computational-Statistics/Lab5/lottery.xls")
lottery = readWorksheet(wb, sheet = "sheet1", header = TRUE)

# 1.1
ggplot(lottery, aes(y=Draft_No, x=Day_of_year)) + geom_point()
# 1.2
ggplot(lottery, aes(y=Draft_No, x=Day_of_year)) + geom_point() + geom_smooth(method="loess")

# 1.3
library(boot)

stat1<-function(data,n){
  data1=data[n,]
  loessM <- loess(Draft_No ~ Day_of_year, data=data1)
  Xa <- which.min(loessM$fitted)
  Xb <- which.max(loessM$fitted)
  y_Xa <- loessM$fitted[Xa]
  y_Xb <- loessM$fitted[Xb]
  T_val <- (y_Xb - y_Xa)/(data1$Day_of_year[Xb] - data1$Day_of_year[Xa])
  ret <- T_val
  return(ret)
}
set.seed(311015)
res1=boot(lottery,stat1,R=2000)
res1Dat <- data.frame(x=res1$t, index=1:2000)
ggplot(res1Dat, aes(x, ..density..)) + geom_histogram(binwidth=0.01, alpha=.8)

options(scipen=999)
mean(res1$t > 0)

# 1.4

permFunc <- function(data, B){
  T_value=numeric(B)
  n=dim(data)[1]
  for(b in 1:B){
    Gb=sample(data$Day_of_year, n)
    data1 <- data.frame(Draft_No = data$Draft_No, day=Gb)
    loessM <- loess(Draft_No ~ day, data=data1)
    Xa <- which.min(loessM$fitted)
    Xb <- which.max(loessM$fitted)
    y_Xa <- loessM$fitted[Xa]
    y_Xb <- loessM$fitted[Xb]
    T_value[b] <- (y_Xb - y_Xa)/(data1$day[Xb] - data1$day[Xa])
  }
  loessMT <- loess(Draft_No ~ Day_of_year, data=data)
  Xa0 <- which.min(loessMT$fitted)
  Xb0 <- which.max(loessMT$fitted)
  y_Xa0 <- loessMT$fitted[Xa0]
  y_Xb0 <- loessMT$fitted[Xb0]
  T_0 <- (y_Xb0 - y_Xa0)/(Xb0 - Xa0)
  p_val <- mean(T_value > abs(T_0))
  return(p_val)
}
set.seed(311015)
permFunc(lottery, 2000)

## 1.5
lottery$Draft_No <- 0
for(i in 1:366){
  lottery$Draft_No[i] <- max(0, min((0.1*lottery$Day_of_year[i] + rnorm(1, 183, 10)), 366))
}
plot(lottery$Draft_No, type="l")
permFunc(lottery, 200)

alphas <- seq(0.2, 10, 0.1)
P_val <- 0
for(j in 1:length(alphas)){
  lottery$Draft_No <- 0
  for(i in 1:366){
    lottery$Draft_No[i] <- max(0, min(alphas[j]*lottery$Day_of_year[i] + rnorm(1, 183, 10), 366))
  }
  P_val[j] <- permFunc(lottery, 200)
}
hist(P_val, breaks=20)
mean(P_val < 0.05)
plot(P_val, typ="l")

### Assignment 2 ### 

wb2 = loadWorkbook("C:/Users/Gustav/Documents/Computational-Statistics/Lab5/prices1.xls")
prices = readWorksheet(wb2, sheet = "sheet1", header = TRUE)

ggplot(prices, aes(x=Price,..density..)) + geom_histogram(binwidth=100, alpha=0.4) +
  geom_freqpoly(binwidth=100, col="darkblue")

# 2.2
## Function for estimating the mean value
stat3<-function(data,n){
  data1=data[n,]
  res = mean(data1$Price)
  return(res)
}
set.seed(311015)
res3=boot(prices,stat3,R=2000)
hist(res3$t)
plot(res3)
priceBoot <- data.frame(mean = res3$t, index = 1:2000)
ggplot(priceBoot, aes(mean)) + geom_histogram(binwidth=10, alpha=0.7)

# Bias correction
biasCorr <- 2 * mean(prices$Price) - mean(res3$t)
mean(prices$Price)- biasCorr
mean(prices$Price)- mean(res3$t)

# Function for estimating the variance of the price of the mean
varBoot <-function(data,n){
  data1=data[n]
  res = (sum((data1 - mean(data1))^2)) * (1/(length(data1)-1))
  return(res)
}
res4=boot(res3$t,varBoot,R=2000)
mean(res4$t)
Samplevar <- (sum((res3$t - mean(res3$t))^2)) * (1/(length(res3$t)-1))

# 95 % C.I for the mean
CI <- boot.ci(res3, type=c("norm","perc", "bca"))
CIvals <- data.frame(rbind(CI$normal[2:3], CI$perc[4:5],CI$bca[4:5]), 
                     X3=c("norm", "perc", "bca"))
names(CIvals) <- c("Lower", "Upper", "Method")

quantile(res3$t, 0.025) 
quantile(res3$t, 0.975) 

floor(2000* (0.05/2))
floor(2000-(2000*(0.05/2)))

res3_t <- data.frame(t=res3$t)
res3_t <- data.frame(res3_t[order(res3_t$t),] )
res3_t[50,]
res3_t[1950,]
mean(res3_t[1000:1001,])

## 2.3
T_star <- 0
for(j in 1:110){
  T_star[j] <- 110*mean(prices[,1]) - 109 * mean(prices[-j, 1])
}
J_T <- (1/110) * sum(T_star)
varJack <- sum((T_star-J_T)^2) / (110*109)

## 2.4
CIvals$Mean <- (CIvals$Lower+CIvals$Upper)/2
CIvals[2,4] <- quantile(priceBoot$mean, 0.5)

ggplot(priceBoot, aes(mean)) + geom_histogram(binwidth=10, alpha=0.7) +
  geom_vline(data=CIvals, aes(xintercept=Lower, col=Method, linetype=Method),show_guide = TRUE, size=1.1) +
  geom_vline(data=CIvals, aes(xintercept=Upper, col=Method, linetype=Method),show_guide = TRUE, size=1.1) +
  geom_vline(data=CIvals, aes(xintercept=Mean, col=Method, linetype=Method),show_guide = TRUE, size=1.1) +
  scale_colour_manual(name="Confidence \n interval", values=c("springgreen3", "blue", "darkorange")) +
  scale_linetype_manual(name="", values=c("solid", "solid","dashed"), guide=FALSE) +
  geom_text(aes(CIvals$Lower[1],0,label = "Lower", vjust = 1, hjust=1.2))+
  geom_text(aes(CIvals$Upper[1],0,label = "Upper", vjust = 1, hjust=1.2))+
  geom_text(aes(CIvals$Mean[1],0,label = "Mean", vjust = 1, hjust=1.2)) + ylim(-7, 225)


CIvals[1,1] - CIvals[3,1]
CIvals[1,2] - CIvals[3,2]

CI$bca
CI$percent
quantile(priceBoot$mean, 0.99999999)
