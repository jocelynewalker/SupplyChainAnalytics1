library(fpp)
#Volume, Dollar Sales, Price per Oz
#Price per Unit versus Volume 
#1 
log.Vol <- log(Creamy$T.VOL)
log.PPOZ <- log(Creamy$PPOZ)
Creamy.log = data.frame(Creamy, log.VOL=log(Creamy$T.VOL), log.PPOZ=log(Creamy$PPOZ))
fit.Creamy <- lm(log.VOL ~ log.PPOZ, data=Creamy.log)
summary(fit.Creamy)
#2
plot(jitter(Creamy.log$log.VOL)~jitter(Creamy.log$log.PPOZ), xlab="log(Price Per Oz)", 
     ylab="log(Volume)", main="Log-Log Model Fit")
abline(fit.Creamy, col="red", lwd=2)
#3
res.Creamy <- residuals(fit.Creamy)
plot(jitter(res.Creamy)~jitter(PPOZ), ylab="Residuals of log(Volume)", 
     xlab="Price Per Oz", main="Log-Log Model Residuals", data=Creamy.log) 
abline(0,0)
#4
Crunchy.log=data.frame(Crunchy, log.CrunchVOL=log(Crunchy$T.VOL), 
                       log.CrunchPPOZ=log(Crunchy$PPOZ))
fit.Crunchy <- lm(log.CrunchVOL ~ log.CrunchPPOZ, data=Crunchy.log)
summary(fit.Crunchy)
#5
plot(jitter(Crunchy.log$log.CrunchVOL)~jitter(Crunchy.log$log.CrunchPPOZ), 
     xlab="log(Price Per Oz)",
     ylab="log(Volume)", main="Log-Log Model Fit")
abline(fit.Crunchy, col="red", lwd=2)
#6
res.Crunchy <- residuals(fit.Crunchy)
plot(jitter(res.Crunchy)~jitter(Crunchy.log$PPOZ), ylab="Residuals of log(Volume)", 
     xlab="Price Per Oz", main="Log-Log Model Residuals", data=Crunchy.log) 
abline(0,0)
#7
#no R code required - see Word doc
#8
par(mfrow=c(2,2))
plot(Creamy$PPOZ, Creamy$T.VOL, xlab="Price Per Oz", ylab="Volume", main="Creamy Data")
plot(Crunchy$PPOZ, Crunchy$T.VOL, xlab="Price Per Oz", ylab="Volume", main="Crunchy Data")

plot(jitter(Creamy$T.VOL) ~ jitter(Creamy$PPOZ), xlab="Price Per Oz", ylab="Volume", main="Creamy Forecast")
Creamy.data <- data.frame(log.PPOZ = log(seq(0.01,0.52, by=0.01)))
new.Creamy<- forecast(fit.Creamy,newdata = Creamy.data )$mean
x <- seq(0.01, 0.52, by=0.01)
lines(x,exp(new.Creamy), col="Red", lwd=2)

plot(jitter(Crunchy$T.VOL) ~ jitter(Crunchy$PPOZ), xlab="Price Per Oz", ylab="Volume", main="Crunchy Forecast")
Crunchy.data <- data.frame(log.CrunchPPOZ = log(seq(0.01,0.52, by=0.01)))
new.Crunchy<- forecast(fit.Crunchy,newdata = Crunchy.data )$mean
x <- seq(0.01, 0.52, by=0.01)
lines(x,exp(new.Crunchy), col="Red", lwd=2)




