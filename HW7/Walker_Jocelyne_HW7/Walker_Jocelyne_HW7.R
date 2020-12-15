library(fpp)
library(dplyr)
#
#Read csv file and make it a time series
RS <- read.csv("RSGCSN.csv") %>%
  select(-DATE) %>%
  ts(start= c(1992,1), frequency=12) 
#
#divide data into 2 sets, training and testing
tr <- window(RS, end=c(2011,12))
te <- window(RS, start=c(2012,1))
#
plot(RS)
abline(v=c(2011,12), col="grey")
#
#Question 1
#use the ets() function to fit 
#Holt-Winters Model to sales data
f.HW <- ets(tr, model="AAM", restrict = FALSE)
summary(f.HW)

#use the forecast() function to obtain 68 month ahead
#forecast
fc.HW <- forecast(f.HW, h=68)
#plot forecast and overlay testing data
plot(fc.HW)
points(te, col="red", pch=19)

#reproduce plot, zooming in on forecast
plot(fc.HW, xlim=c(2009,2018), ylim=c(40000,60000))
points(te, col="red", pch=19)

#in-sample and out-of-sample fit statistics
fit <- accuracy(fc.HW, te)
fit

naive.fit <- naive(tr, h = 68)
accuracy(naive.fit)
#
# Question 2
#use the ets() function to fit 
#Holt-Winters Model to sales data, restricting damping
f.HW2 <- ets(tr, model="AAM", damped = FALSE, restrict =FALSE)
summary(f.HW2)

#use the forecast() function to obtain 68 month ahead
#forecast
fc.HW2 <- forecast(f.HW2, h=68)
#plot forecast and overlay testing data
plot(fc.HW2, xlim=c(2009,2018), ylim=c(40000,60000))
points(te, col="red", pch=19)

#in-sample and out-of-sample fit statistics
fit2 <- accuracy(fc.HW2, te)
fit2

#
# Question 3
#optimal ets() model selection
#use the ets() function to fit 
#"ZZZ" to sales data
f.o <- ets(tr, model="ZZZ", restrict =FALSE)
summary(f.o)

#use the forecast() function to obtain 68 month ahead
#forecast
fc.o <- forecast(f.o, h=68)
#plot forecast and overlay testing data
plot(fc.o, xlim=c(2009,2018), ylim=c(40000,60000))
points(te, col="red", pch=19)

#in-sample and out-of-sample fit statistics
fito <- accuracy(fc.o, te)
fito

#
#Question 4
#optimized model using BoxCox transformed data
L <- BoxCox.lambda(tr)
z <- BoxCox(tr,L)
z
fB.O <- ets(tr, model="ZZZ", restrict = FALSE, lambda = L)
summary(fB.O)

#use the forecast() function to obtain 68 month ahead
#forecast
fBc.O <- forecast(fB.O, h=68)
#plot forecast 
plot(fBc.O, xlim=c(2009,2018), ylim=c(40000,60000))
points(te, col="red", pch=19)

#in-sample and out-of-sample fit statistics
fitBcO <- accuracy(fBc.O, te)
fitBcO

#
#Question 5
#optimized BoxCox with Damping
fB.OD <- ets(tr, model="ZZZ", damped = TRUE, restrict = FALSE, lambda = L)
summary(fB.OD)

#use the forecast() function to obtain 68 month ahead
#forecast
fBc.OD <- forecast(fB.OD, h=68)
#plot forecast 
plot(fBc.OD, xlim=c(2009,2018), ylim=c(40000,60000))
points(te, col="red", pch=19)

#in-sample and out-of-sample fit statistics
fitBcOD <- accuracy(fBc.OD, te)
fitBcOD

#Question 6
#
# Discarding Old Data
#
FS <- NULL
for(sy in (1992:2006)){
  td <- window(RS,start=c(sy,1), end=c(2011,12))
  L <- BoxCox.lambda(td)
  fBC <- ets(td, model="ZZZ",  restrict=FALSE, lambda=L)
  RMSE <- accuracy(fBC)[2]
  FS <- rbind(FS,c(sy,RMSE))
}
colnames(FS) <- c("Starting Year","RMSE")
FS
#2003 has the lowest RMSE
#create reduced training set
trr <- window(RS, start=c(2003,1), end=c(2011,12))

#
#Question 7
#fitting a model on the reduced training dataset
#optimized model using BoxCox transformed data
Lrr <- BoxCox.lambda(trr)
zrr <- BoxCox(trr,Lrr)
zrr
f <- ets(trr, model="ZZZ", restrict = FALSE, lambda = Lrr)
summary(f)

#use the forecast() function to obtain 68 month ahead
#forecast
fc <- forecast(f, h=68)
#plot forecast 
plot(fc, xlim=c(2009,2018), ylim=c(40000,60000))
points(te, col="red", pch=19)

#in-sample and out-of-sample fit statistics
fitfc <- accuracy(fc, te)
fitfc

#
#Question 8
#set training set
tr.new <- window(RS, start=c(2003,1), end=c(2017,8))
#select ETS model from Q7 -- (M,A,A)
ff <- ets(tr.new, model="MAA", restrict = FALSE)
summary(ff)

#use the forecast() function to obtain 64 month ahead
#forecast
ffc <- forecast(ff, h=64)
#plot forecast 
plot(ffc)

0.9977177/0.9224311*0.9580372
627.4568/494.1545*541.9784
