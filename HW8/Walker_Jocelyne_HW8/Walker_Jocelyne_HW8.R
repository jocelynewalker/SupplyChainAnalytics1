library(fpp)
library(dplyr)
load("ChinaElectricity.RData")

CH.tr <-window(CH, end=2006)
CH.te <-window(CH, start=2007, end=2013)

# Question 1
model1 <- ets(CH.tr, model = "ZZZ", restrict = FALSE, lambda = 0)
summary(model1)
tsdiag(model1)
forecast1 <- forecast(model1, h=7)
plot(forecast1)
lines(CH.te, col = "red")
accuracy(forecast1, CH.te)

# Question 2
adf.test(log(CH.tr), alternative = "stationary") # p-value 0.1913
adf.test(diff(log(CH.tr)), alternative = "stationary") # p-value 0.08678
adf.test(diff(diff(log(CH.tr))), alternative = "stationary") # p-value 0.03046

# Question 3
stationary.series <- diff(diff(log(CH.tr)))
tsdisplay(stationary.series)

# Question 4
model2 <- auto.arima(CH.tr, lambda = 0)
summary(model2)
tsdiag(model2)
forecast2 <- forecast(model2, h=7)
plot(forecast2)
lines(CH.te, col = "red")
accuracy(forecast2, CH.te)

# Question 5
full.model <- Arima(CH, order = c(1,1,0), include.drift = TRUE, lambda = 0)
summary(full.model)
tsdiag(full.model)
forecast3 <- forecast(full.model, h=7)
plot(forecast3)
summary(forecast3)

# Question 8
View(CH)
1.2523*5436.6 - .2523*4984.7
5932.503 - 5550.614

1.2523*5932.503 - .2523*5436.6
6474.476-6057.619

1.2523*6474.476 - .2523*5932.503
7066.192 - 6611.216
