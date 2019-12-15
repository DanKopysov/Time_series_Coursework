library(readxl)
install.packages("zoo")
library(zoo)
install.packages("openxlsx")
library(openxlsx)
data = read.xlsx("C:/Users/xiaomi/Downloads/Stock_all_companies2.xlsx", sheet = 1)

data_sanc <- data[ which(data$sanctions == 1),]
data_unsanc <- data[ which(data$sanctions == 0),]
mean(data_sanc$sanctions)

data_sanc$TRADEDATE <- as.Date(data_sanc$TRADEDATE)
data_unsanc$TRADEDATE <- as.Date(data_unsanc$TRADEDATE)

data_news <- xts(x = data_sanc %>% select("TRADEDATE", "LOW"), order.by = df_sanc_for_matr$TRADEDATE)

install.packages("tidyverse")
library(tidyverse)
head(data_sanc)
variable.names(data_sanc)

df_sanc_for_matr <- data_sanc %>% select("TRADEDATE", "SECID", "CLOSE", "OPEN", "HIGH", "LOW", "Amount.of.RBC.news")
xts_sanc <- xts(x = df_sanc_for_matr, order.by = df_sanc_for_matr$TRADEDATE)
class(xts_sanc)

na.approx(xts_sanc)

plot.ts(as.xts(data_news))
coredata(xts_sanc)

plot(data$CLOSE)

library(forecast)
install.packages('forecast')

secid <- levels(factor(data$SECID))

i = 17

dog <- function(i, f){
  test <- zooreg(data[data$SECID == secid[i],]$CLOSE, order.by = as.POSIXct(data[data$SECID == secid[i],]$TRADEDATE, format = '%Y-%m-%d'))
  test2 <- zooreg(as.integer(factor(data[data$SECID == secid[i],]$Sanctions.by.country)), order.by = as.POSIXct(data[data$SECID == secid[i],]$TRADEDATE, format = '%Y-%m-%d'))
  test3 <- zooreg(data[data$SECID == secid[i],]$Amount.of.RBC.news, order.by = as.POSIXct(data[data$SECID == secid[i],]$TRADEDATE, format = '%Y-%m-%d'))
  test4 <- zooreg(data[data$SECID == secid[i],]$RUB.X.high_cleaned, order.by = as.POSIXct(data[data$SECID == secid[i],]$TRADEDATE, format = '%Y-%m-%d'))
  plot(test, main = paste0(secid[i], ' (', data[data$SECID == secid[i],]$SHORTNAME[1], ')'), ylab = 'CLOSE in $', xlab = '')
  points(test[test2 > 1], col = 'red', lwd = 5)
  points(test[test3 > f], col = 'blue', lwd = 2, pch = 17)
  par(new = TRUE)
  plot(test4, type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "", col = 'magenta')
  axis(side=4, at = pretty(range(test4)))
}

dog(14, 5)



x = data_sanc
x = data_unsanc

xlev <- levels(factor(x$SECID))
i = 1
test <- zooreg(x[x$SECID == xlev[i],]$CLOSE, order.by = as.POSIXct(x[x$SECID == xlev[i],]$TRADEDATE, format = '%Y-%m-%d'))
meanclose <- diff(zoo(na.approx(log(test))))

for (i in 2:length(xlev)){
  test <- zooreg(x[x$SECID == xlev[i],]$CLOSE, order.by = as.POSIXct(x[x$SECID == xlev[i],]$TRADEDATE, format = '%Y-%m-%d'))
  a <- diff(zoo(na.approx(log(test))))
  meanclose <- cbind(meanclose, a)
}

meanclose <- as.data.frame(meanclose)
meanclose[is.na(meanclose)] <- 0

tt2 <- zooreg(apply(meanclose, 1, mean), order.by = index(a))
plot(tt2)

tt <- zooreg(apply(meanclose, 1, mean), order.by = index(a))
plot(tt)
lines(tt2, col = 'red')

sd(tt2)/mean(tt2)*100

#######
# VAR #
#######
install.packages('dynlm')
library('dynlm')
news <- zooreg(data[data$SECID == secid[i],]$Amount.of.RBC.news, order.by = as.POSIXct(data[data$SECID == secid[i],]$TRADEDATE, format = '%Y-%m-%d'))

library(reshape2)
tab1 <- dcast(data, TRADEDATE ~ Sanctions.by.country, sum)
tab2 <- apply(tab1[,3:4], 1, sum)
tab2[tab2>0] <- 1 



t1 <- ts(as.vector(tt), start = c(2014,1,8), frequency = 365)
t2 <- ts(as.vector(tt2), start = c(2014,1,8), frequency = 365)
t3 <- ts(as.vector(news), start = c(2014,1,8), frequency = 365)
t4 <- ts(as.vector(tab2)[1:1344], start = c(2014,1,8), frequency = 365)

var1 <- dynlm(t1 ~ L(t1) + L(t2) + t3 + t4)
var2 <- dynlm(t2 ~ L(t2) + L(t1) + t3 + t4)

coeftest(var2, vcov. = sandwich)

install.packages('vars')
library('sandwich')
library(vars)

VAR_data <- na.omit(window(ts.union(t1, t2)))

t3 <- ts(as.vector(news), start = c(2014,1,8), frequency = 365)
boxplot(t3)

t3[t3<2] <- 0
t3[t3>=2] <- 1
t33 <- ts(na.omit(t3)[1:1344], start = c(2014,1,8), frequency = 365)
lt33 <- c(NA,t33[-1344])
l7t33 <- c(rep(NA, 30),t33[-(1345-30):-1345])
lt4 <- c(NA,t4[-1344])
l7t4 <- c(rep(NA, 30),t4[-(1345-30):-1345])

#1step
VARselect(VAR_data, exogen = cbind(t33, lt33, l7t33, t4, lt4, l7t4))

#2step
m1 <- VAR(y = VAR_data, p = 1, exogen = cbind(t33, lt33, l7t33, t4, lt4, l7t4))
summary(m1)
levels(factor(data[data$SECID == secid[15],]$Sanctions.by.country))

summary(as.integer(factor(data[data$SECID == secid[15],]$Sanctions.by.country)))

plot(test2[test2 > 1], type = 'p')

install.packages('sjPlot')

library(sjPlot)
tab_model(m1)
typeof(as.POSIXct(data[data$SECID == secid[1],]$TRADEDATE[1], format = '%Y-%m-%d'))

