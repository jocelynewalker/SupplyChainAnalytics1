library(fpp)
library(dplyr)
#
#Questions 1 and 2
load("model mSD1.rda")
load("model mSD2.rda")

#1
summary(mSD1)
summary(mSD2)
CV(mSD1)
CV(mSD2)

#2
#no code required

#
#Questions 3 and 4
SD <- read.csv("Soft Drink Sales.csv")
#
#Preprocess data
#
SD <- SD %>% 
  mutate(STORE=as.factor(STORE),
            LpX = log(pX),
            LpY = log(pY),
            DLpX = deal_X*LpX,
            DLpY = deal_Y*LpY,
            LSales = log(Sales_oz_X))

#
# Function to extract a range of weeks of demand
#   for a given product and store
#
# Inputs:
#  x - data frame with combined data
#  store - store number(1 to 7) to extract
#  prod - product to extract: "lit" or "reg"
#  sw - first week to extract 
#  ex - last week to extract
#
# Output:
#  reduced dataframe with only the store-week-product needed
#
select.wsp <- function(x, store, prod, sw, ew){
  xr <- x %>% 
    filter(STORE == store,
           class == prod,
           WEEK >= sw,
           WEEK <= ew)
  return(xr)
}
#3
XL2 <- SD %>% 
  select.wsp(2,"reg",1,40)
summary(XL2)

XL3 <- SD %>% 
  select.wsp(2,"reg",41,52)
summary(XL3)

#4
XL4 <- SD %>% 
  select.wsp(5,"lit",1,40)
summary(XL4)

XL5 <- SD %>% 
  select.wsp(5,"lit",41,52)
summary(XL5)

#Questions 5 and 6
#
# Evaluate fitted values on the reduced store-product data set
#
# Inputs:
#  x - reduced data set (for a store-product over a week-range)
#  m - regression model object to obtain the fit
#
# Output:
#  data.frame with three columns:
#   Week - Week number
#   Sales - sales of product
#   Fit - fitted value of sales
#  
fitted.sales <- function(x,m){
  Fit <- exp(forecast(m,newdata=x)$mean)
  Sales <- x[,"Sales_oz_X"]
  return(data.frame(Week=x[,"WEEK"], Sales=Sales, Fitted=Fit))
}

#5
#in-sample fitted values
f.SD1 <- fitted.sales(XL2,mSD1)
f.SD1
#in-sample MSE
sqrt(mean((f.SD1[,"Sales"]-f.SD1[,"Fitted"])^2))
#out-of-sample fitted values
f.SD2 <- fitted.sales(XL3,mSD1)
f.SD2
#out-of-sample MSE
sqrt(mean((f.SD2[,"Sales"]-f.SD2[,"Fitted"])^2))

#6 using mSD2
#in-sample fitted values
f.SD3 <- fitted.sales(XL2,mSD2)
f.SD3
#in-sample MSE
sqrt(mean((f.SD3[,"Sales"]-f.SD3[,"Fitted"])^2))
#out-of-sample fitted values
f.SD4 <- fitted.sales(XL3,mSD2)
f.SD4
#out-of-sample MSE
sqrt(mean((f.SD4[,"Sales"]-f.SD4[,"Fitted"])^2))

#Questions 7 and 8
#
#
#
# Code to create forecasting plot for
# store #1 Regular ("reg") product
# using model mSD1
#
# First we select the data for store 1
# product "reg" over weeks 1-40
X <- select.wsp(SD,1,"reg",1,40)
# 
# Next we calculate the fitted sales
# obtained by model mSD1 over weeks 1-40
FS <- fitted.sales(X,mSD1)
Week <- FS[,"Week"]
Sales <- FS[,"Sales"]
Fitted <- FS[,"Fitted"]
#
# Next select the OUT-OF SAMPLE data
# for the same store-product
newX <- select.wsp(SD,1,"reg",41,52)
#
# Calculate the out-of-sample forecasts
Fcst <- fitted.sales(newX,mSD1)
FWeek <- Fcst[,"Week"]
N.Sales <- Fcst[,"Sales"]
F.Sales <- Fcst[,"Fitted"]
#
# Render the Plot
plot(Week,Sales, type="l", xlim=c(0,52), ylim=c(2000,45000),
     main=paste("Store",1,"reg",substitute(mSD1)),
     xlab="Weeks", ylab="Sales (oz)")
points(Week,Fitted, col="red", pch=20)
lines(FWeek,N.Sales, col="grey")
points(FWeek,F.Sales, col="blue", pch=20)
#
#
#
# This function creates a forecasting plot
#  for a store-product-model combination
#
# Inputs:
#  sn - store number (1,2,...,7)
#  pname - product name ("lit" or "reg")
#  model - forecasting model (mSD1 or mSD2)
#
#7
#function to create a forecasting plot 
#for a store-product-model combination
spm.fplot <- function(sn, pname, model){
    X <- select.wsp(SD,sn,pname,1,40)
    FS <- fitted.sales(X, model)
    Week <- FS[,"Week"]
    Sales <- FS[,"Sales"]
    Fitted <- FS[,"Fitted"]
    newX <- select.wsp(SD,sn,pname,41,52)
    Fcst <- fitted.sales(newX,model)
    FWeek <- Fcst[,"Week"]
    N.Sales <- Fcst[,"Sales"]
    F.Sales <- Fcst[,"Fitted"]
    plot(Week,Sales, type="l", xlim=c(0,52), ylim=c(2000,45000),
         main=paste("Store",sn,pname,substitute(model)),
         xlab="Weeks", ylab="Sales (oz)")
    points(Week,Fitted, col="red", pch=20)
    lines(FWeek,N.Sales, col="grey")
    points(FWeek,F.Sales, col="blue", pch=20)
    
}
#plots for Store 1 product "reg" using Models mSD1 and mSD2
spm.fplot(1,"reg",mSD1)
spm.fplot(1,"reg",mSD2)

#8
#plots for Store 5 product "lit" using modelsl mSD1 and mSD2
spm.fplot(5,"lit",mSD1)
spm.fplot(5,"lit",mSD2)
