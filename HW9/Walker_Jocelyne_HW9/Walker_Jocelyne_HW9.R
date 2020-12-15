# prepare the data
library(fpp)
library(dplyr)
PG <- read.csv("IPG2211N.csv") %>%
  select(-DATE) %>%
  ts(start=c(1972,1), frequency=12)
PG1.tr <- window(PG, end=c(1995,12))
PG1.te <- window(PG, start=c(1996,1), end=c(2000,12))

# Question 1
L <- BoxCox.lambda(PG1.tr)
L
z <- BoxCox(PG1.tr, L)        # Box-Cox series 
tsdisplay(diff(z))            # non-seasonal diff
tsdisplay(diff(z, lag = 12))  # seasonal diff
adf.test(z)

# Question 2
model2 <- auto.arima(PG1.tr, lambda = L)
summary(model2)
tsdiag(model2, gof.lag = 24)
forecast2 <- forecast(model2, h = 60)
plot(forecast2, xlim = c(1990,2001), ylim = c(60,140))
lines(PG1.te, col = "red")
accuracy(forecast2, PG1.te)

# Question 3 - manual model selection
model3a <- Arima(PG1.tr, order=c(1,1,2), seasonal=c(1,1,1), lambda = L)
summary(model3a)
          # didn't help
model3b <- Arima(PG1.tr, order=c(1,1,1), seasonal=c(0,1,1), lambda = L)
summary(model3b)
          # didn't help
model3c <- Arima(PG1.tr, order=c(1,1,2), seasonal=c(0,1,2), lambda = L)
summary(model3c)
          # didn't help
model3d <- Arima(PG1.tr, order=c(1,1,0), seasonal=c(0,1,1), lambda = L)
summary(model3d)
          # didn't help
model3e <- Arima(PG1.tr, order=c(1,0,2), seasonal=c(0,1,1), lambda = L)
summary(model3e)
tsdiag(model3e, gof.lag = 24)
forecast3e <- forecast(model3e, h = 60)
plot(forecast3e, xlim = c(1990,2001), ylim = c(60,140))
lines(PG1.te, col = "red")
accuracy(forecast3e, PG1.te)
          # helped!!!

# Question 4
PG2.tr <- window(PG, end=c(2011,12))
PG2.te <- window(PG, start=c(2012,1))
L2 <- BoxCox.lambda(PG2.tr)
L2
z <- BoxCox(PG2.tr, L2)
tsdisplay(diff(z))
tsdisplay(diff(z, lag = 12))
adf.test(z)

# Question 5
model5 <- auto.arima(PG2.tr, lambda = L2)
summary(model5)
tsdiag(model5, gof.lag = 24)
forecast5 <- forecast(model5, h = 69)
plot(forecast5, xlim = c(2000,2018), ylim = c(60,140))
lines(PG2.te, col = "red")
accuracy(forecast5, PG2.te)

# Question 6
PG3.tr <- window(PG, start=c(2005,1), end=c(2011,12))
L3 <- BoxCox.lambda(PG3.tr)
model6 <- auto.arima(PG3.tr, lambda = L3)
summary(model6)
tsdiag(model6, gof.lag = 24)
forecast6 <- forecast(model6, h = 69)
plot(forecast6, xlim=c(2000,2018), ylim=c(60,140))
lines(PG2.te, col = "red")
accuracy(forecast6, PG2.te)

# Question 7 - manual selection on reduced data set
model7a <- Arima(PG3.tr, order=c(3,0,3), seasonal=c(3,1,3), include.drift = TRUE)
summary(model7a)
    
model7b <- Arima(PG3.tr, order=c(2,0,2), seasonal=c(2,1,2), include.drift = TRUE)
summary(model7b)
tsdiag(model7b, gof.lag = 24)
forecast7b <- forecast(model7b, h = 69)
plot(forecast7b, xlim=c(2000,2018), ylim=c(60,140))
lines(PG2.te, col = "red")
accuracy(forecast7b, PG2.te)
    # reduced AICc and BIC 
    # still high out-of-sample bias

model7c <- Arima(PG3.tr, order=c(1,0,1), seasonal=c(1,1,1), include.drift = TRUE)
summary(model7c)
tsdiag(model7c, gof.lag = 24)
forecast7c <- forecast(model7c, h = 69)
plot(forecast7c, xlim=c(2000,2018), ylim=c(60,140))
lines(PG2.te, col = "red")
accuracy(forecast7c, PG2.te)
    # reduced AICc and BIC 
    # still higher out-of-sample bias

model7d <- Arima(PG3.tr, order=c(2,0,3), seasonal=c(2,1,2), include.drift = TRUE)
summary(model7d)
tsdiag(model7d, gof.lag = 24)
forecast7d <- forecast(model7d, h = 69)
plot(forecast7d, xlim=c(2000,2018), ylim=c(60,140))
lines(PG2.te, col = "red")
accuracy(forecast7d, PG2.te)
    # lowest out-of-sample bias!!!

# Question 8 - future forecast
best.model <- Arima(PG3.tr, order=c(2,0,3), seasonal=c(2,1,2), include.drift = TRUE)
summary(best.model)
tsdiag(best.model, gof.lag = 24)
forecast.best <- forecast(best.model, h = 132)
forecast.best
plot(forecast.best, xlim=c(2005,2022))
lines(PG2.te, col = "red")
