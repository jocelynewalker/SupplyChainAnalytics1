library(fpp)
#1
mult.x <- Soft_Drinks$deal_X*log(Soft_Drinks$pX)
mult.y <- Soft_Drinks$deal_Y*log(Soft_Drinks$pY)
fit <- lm(log(Soft_Drinks$oz_X) ~ Soft_Drinks$deal_X + Soft_Drinks$deal_Y +log(Soft_Drinks$pX) +log(Soft_Drinks$pY) + 
            mult.y + mult.x)
summary(fit)
CV.fit <- CV(fit)
CV.fit
#
#2 see word doc
#
#3
fit.2 <- lm(log(Soft_Drinks$oz_X) ~ Soft_Drinks$deal_X +log(Soft_Drinks$pX) +log(Soft_Drinks$pY) + 
            mult.y + mult.x)
summary(fit.2)
CV.fit.2 <- CV(fit.2)
CV.fit.2
max(-2.233870e+03, -2.235893e+03)
#4
fit.3 <- lm(log(Soft_Drinks$oz_X) ~ Soft_Drinks$deal_X +log(Soft_Drinks$pX) +log(Soft_Drinks$pY) + 
              mult.x)
summary(fit.3)
CV.fit.3 <- CV(fit.3)
CV.fit.3

fit.4 <- lm(log(Soft_Drinks$oz_X) ~ log(Soft_Drinks$pX) +log(Soft_Drinks$pY) + 
              mult.x)
summary(fit.4)
CV.fit.4 <- CV(fit.4)
CV.fit.4
#AICc goes up 
#
#5
#
forward.fit.1 <- lm(log(Soft_Drinks$oz_X) ~ log(Soft_Drinks$pX))
summary(forward.fit.1)
CV.forward.fit.1 <- CV(forward.fit.1)
CV.forward.fit.1
#
#6 see word doc
#
# 7
forward.fit.2 <- lm(log(Soft_Drinks$oz_X) ~ log(Soft_Drinks$pX) + mult.x)
summary(forward.fit.2)
CV.forward.fit.2 <- CV(forward.fit.2)
CV.forward.fit.2
#
#8
#
forward.fit.3 <- lm(log(Soft_Drinks$oz_X) ~ log(Soft_Drinks$pX) + mult.x + log(Soft_Drinks$pY))
summary(forward.fit.3)
CV.forward.fit.3 <- CV(forward.fit.3)
CV.forward.fit.3
#AICc went down
forward.fit.4 <- lm(log(Soft_Drinks$oz_X) ~ Soft_Drinks$deal_X + log(Soft_Drinks$pX) + mult.x + log(Soft_Drinks$pY))
summary(forward.fit.4)
CV.forward.fit.4 <- CV(forward.fit.4)
CV.forward.fit.4
#AICc went down
forward.fit.5 <- lm(log(Soft_Drinks$oz_X) ~ Soft_Drinks$deal_X + log(Soft_Drinks$pX) + mult.x + log(Soft_Drinks$pY) + mult.y)
summary(forward.fit.5)
CV.forward.fit.5 <- CV(forward.fit.5)
CV.forward.fit.5
#AICc went up
#Forward.fit.4 is the best model
