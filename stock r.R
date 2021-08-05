#installing libraries
install.packages("imputeTS")
install.packages("devtools")
install_github("SteffenMoritz/")
install.packages("tidyverse")
install.packages("sarima")
install.packages("quantmod")
install.packages("arch")
install.packages("garch")
install.packages("PerformanceAnalytics")
install.packages("rugarch")
install.packages("xts")
install.packages("zoo")
library(tseries)
library(urca)
library(lmtest)
library(strucchange)
library("padr")
library(devtools)
library("imputeTS")
library(ggplot2)
library(plotly)
library(faraway)
library(tidyverse)
library(stats)
library(forecast) 
library(dynlm)
library(lubridate) 
library(strucchange) 
library(sarima)
library(quantmod)
library(charting)
library(xts)
library(PerformanceAnalytics)
library(rugarch)
#importing data
data_1=read.csv("C:/Users/Dell/Desktop/Python projects/stock(2011-2021).csv")
View(data_1)

#data preprocessing
summary(data_1)
data_1[,1]=as.Date(data_1[,1],format="%d-%b-%y")
data<- pad(data_1)
data
data<-na.interpolation(data)
data<-data.frame(Daily=data[,1], Prices=data[,5])
head(data)
time<-ts(dat)
head(time)
#Returns calculation
Returns<-diff(log(data[,2]))
head(Returns)
plot(Returns)

##Analysis

#auto-correlation function
acf(Returns)
pacf(Returns)
tsdisplay(Returns)

#decompose
fit <- decompose(Returns, type='additive') 
autoplot(fit)

#testing stationary
kpss.test(Returns,null='Trend')
adf.test(Returns)

#checking structure change
model1=Fstats(Returns~1,from=0.01)
sctest(model1)
strucchange::breakpoints(Returns~1)

#checking volatility
VolatilitySkewness(Returns)
hist(Returns)
chart.histogram(Returns,methods=c('add.density','add.normal'),colorset=c('blue','orange','red'))

#modelling
auto.arima(Returns)
fit1<-arima(Returns,c(5,0,3))
fit1
plot(fit1)
s<-ugarchspec(mean.model=list(armaOrder=c(5,3)),variance.model=list(model='sGARCH'),distribution.model='norm')
fit2<-ugarchfit(data=Returns, spec=s)
fit2
plot(fit2)
