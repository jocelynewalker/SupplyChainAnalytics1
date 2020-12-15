#
# This script requires that you install the "dplyr" package
# before running it.
#
library(glmnet)
library(fpp)
library(dplyr)
#
SD <- read.csv("Soft Drink Sales.csv")

View(SD)

dim(SD)
SD <- na.omit(SD)
dim(SD)
#
#Preprocess data
#
S <- mutate(SD, STORE=as.factor(STORE),
            LpX = log(pX),
            LpY = log(pY),
            DLpX = deal_X*LpX,
            DLpY = deal_Y*LpY,
            LSales = log(Sales_oz_X))
View(S)
#
S.T <- filter(S, WEEK <= 40) %>%
  select(-pX, -pY, -WEEK, -Sales_oz_X)
View(S.T)
#
x <- model.matrix(LSales ~ ., data=S.T)[,-1]
y <- S.T[,"LSales"]
#
set.seed(1)
train <- sample(1:nrow(x),nrow(x)/2)
test <- -train
#
# Create the grid of values for lambda, the tuning parameter 
l.val <- exp(seq(1,6,length=100))
#
# The Lasso Model
fit.L <- glm(x[train,],y[train], alpha=1,lambda=l.val)
plot(fit.L, label=TRUE)
plot(fit.L, xvar="lambda", label=TRUE)


