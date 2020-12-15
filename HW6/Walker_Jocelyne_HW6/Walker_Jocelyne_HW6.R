library(fpp)
library(dplyr)
library(plyr)

#
#Question 1
#
load("model mSD1.rda")
load("model mSD2.rda")
load("model mSD3.rda")
load("model mSD4.rda")
load("model mSD5.rda")

mL <- list(mSD1,mSD2,mSD3,mSD4,mSD5)
mNam <- c("mSD1","mSD2","mSD3","mSD4","mSD5")
cNam <- c("CV", "AIC","AICc", "BIC", "AdjR2")

ICSum <- mL %>% 
  lapply(CV) %>% 
  unlist() %>% 
  matrix(byrow=TRUE, ncol=5, dimnames=list(mNam,cNam))
ICSum

#
#Question 1
summary(mSD1)
summary(mSD2)
summary(mSD3)
summary(mSD4)
summary(mSD5)

#
#Question 2
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
# FUNCTION select.wsp(x,store,prod,sw,ew) 
#   Extracts a range of weeks of demand
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

# Function fitted.sales(x,m)
#   Evaluates fitted values on the reduced store-product data set
#
# Inputs:
#  x - reduced data set (for a store-product over a week-range)
#  m - regression model object to obtain the fit (lm object)
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

#
# FUNCTION fm(m,SN,PN,SW,EW)
#      it calculates Fit Metrics of model m
#
# Inputs:
#  m -  forecasting model (lm-object)
#  SN - store number (1, ...,7)
#  PN - product name ("reg" or "lit")
#  SW - starting week (1 or 41 for in or out of sample)
#  EW - ending week ( 40 or 52 for in or out of sample)
#
# Output:
#  data.frame object with the fit metrics
#     corresponding to 'x'
#

fm <- function(m,SN,PN,SW,EW){
  x <- select.wsp(SD,SN,PN,SW,EW)
  y <- ts(fitted.sales(x,m), start=SW)
  sales <- exp(x$LSales)
  
  #MEAN ERROR (ME)
  error <- sales - y
  me <- mean(error[,"Fitted"])
  
  #MEAN PERCENTAGE ERROR (MPE)
  mpe <- mean(error[,"Fitted"]/sales)*100
  
  #MEAN AVERAGE ERROR (MAE)
  mae <- mean(abs(error[,"Fitted"]))
  
  #MEAN ABSOLUTE PERCENTAGE ERROR (MAPE)
  mape <- mean(abs(error[,"Fitted"])/sales)*100
  
  #ROOT MEAN SQUARED ERROR
  rmse <- sqrt(mean(error[,"Fitted"]^2))
  
  #MEAN ABSOLUTE SCALED ERRORS
  n <- length(y[,"Fitted"])
  f <- y[,"Fitted"]
  mase <- (mean(abs((sales - f) / ((1/(n-1)) * sum(abs(sales[2:n]-sales[1:n-1]))))))
    
  return(data.frame(ME = me, MPE = mpe, 
                    MAE = mae, MAPE = mape,
                    RMSE = rmse, MASE = mase))
}


fm(mSD1,1,"reg",1,40)
fm(mSD1,1,"reg",41,52)

#Questions 3 and 4

#
# FUNCTION is.fm(sn,pn)
#      returns block of in-sample fit metrics for
#      store-product combination
# Inputs:
#  sn - store number (1, ..., 7)
#  pn - product name ("reg" or "lit")
#
# Output:
#  Matrix with fit metrix for all
#    models in the list for the store
#    and product combination
#
is.fm <- function(sn,pn){
  fmNam <- c("ME","MPE","MAE","MAPE","RMSE","MASE")
  FL <- mL %>%
    lapply(fm, SN = sn, PN = pn, SW = 1, EW = 40)
  FL <- data.frame(do.call(rbind.data.frame, FL), row.names = c("mSD1", "mSD2", "mSD3", "mSD4", "mSD5"))
  return(FL)
}

is.fm(1,"reg")
is.fm(1, "lit")

#4
#for each case select the best model as minimizes RMSE
is.fm(1,"reg")
is.fm(1, "lit")
is.fm(2,"reg")
is.fm(2, "lit")
is.fm(3,"reg")
is.fm(3, "lit")
is.fm(4,"reg")
is.fm(4, "lit")
is.fm(5,"reg")
is.fm(5, "lit")
is.fm(6,"reg")
is.fm(6, "lit")
is.fm(7,"reg")
is.fm(7, "lit")

#
#Questions 5 6 and 7
#
#

# FUNCTION os.fm(SN,PN,m)
#  Calculates in-sample and out-of-sample fit metrics
#  for the given stote-product-model combination
#
#Inputs:
#  SN - store number (1, ...,7)
#  PN - Product name ("reg" or "lit")
#  m  - modelname (lm object)
#
# Output:
#  in-sample and out-of sample statistics
#
os.fm <- function(SN,PN,m){
  rN <- c(paste("IS",SN,PN,substitute(m)),
          paste("OS",SN,PN,substitute(m)))
  OSM <- rbind(fm(m,SN,PN,1,40),fm(m,SN,PN,41,52))
  
  rownames(OSM)<- rN
  return(OSM)
}


os.fm(2,"lit",mSD1)
#in sample and out of sample metric comparison 
#for the best model in Q4 for Store 6 "reg"
#store 6 "reg" best model was mSD5
os.fm(6, "reg",mSD5)

#6
#identify s-p combo that has the worst 
#in-sample and out-of-sample bias as a percent of sales
#this "percent of sales" is best measured by MAPE
os.fm(1, "reg",mSD1)
  #in sample MAPE     10.382696
  #out of sample MAPE 8.340499
os.fm(1, "lit",mSD5)
  #in sample MAPE     11.821295
  #out of sample MAPE 8.491686
os.fm(2, "reg",mSD2)
  #in sample MAPE     10.621273
  #out of sample MAPE 9.307557
os.fm(2, "lit",mSD5)
  #in sample MAPE     10.88548
  #out of sample MAPE 13.23056
os.fm(3, "reg",mSD1)
  #in sample MAPE     10.052807
  #out of sample MAPE 9.626766
os.fm(3, "lit",mSD4)
  #in sample MAPE     13.35429
  #out of sample MAPE 12.27961
os.fm(4, "reg",mSD1)
  #in sample MAPE     9.530929
  #out of sample MAPE 12.920785
os.fm(4, "lit",mSD5)
  #in sample MAPE     13.479926
  #out of sample MAPE 9.780137
os.fm(5, "reg",mSD5)
  #in sample MAPE     11.788367
  #out of sample MAPE 8.354866
os.fm(5, "lit",mSD3)
  #in sample MAPE     12.86128
  #out of sample MAPE 12.62963
os.fm(6, "reg",mSD5)
  #in sample MAPE     13.68968
  #out of sample MAPE 21.17974***
os.fm(6, "lit",mSD2)
  #in sample MAPE     13.24490
  #out of sample MAPE 14.27049
os.fm(7, "reg",mSD1)
  #in sample MAPE     10.81157
  #out of sample MAPE 10.92873
os.fm(7, "lit",mSD2)
  #in sample MAPE     13.9829***
  #out of sample MAPE 11.6072

#Question 7
#MASE metric - most potential for improvement
#higher MASE means more potential for improvement 
os.fm(1, "reg",mSD1)
  #in sample MASE     0.10627061
  #out of sample MASE 0.06258951
os.fm(1, "lit",mSD5)
  #in sample MASE     0.11985364
  #out of sample MASE 0.03871842
os.fm(2, "reg",mSD2)
  #in sample MASE     0.09047375
  #out of sample MASE 0.05071711
os.fm(2, "lit",mSD5)
  #in sample MASE     0.08746151
  #out of sample MASE 0.13733111
os.fm(3, "reg",mSD1)
  #in sample MASE     0.09064111
  #out of sample MASE 0.04641449
os.fm(3, "lit",mSD4)
  #in sample MASE     0.08997199
  #out of sample MASE 0.06971128
os.fm(4, "reg",mSD1)
  #in sample MASE     0.3936602***
  #out of sample MASE 0.4189415***
os.fm(4, "lit",mSD5)
  #in sample MASE     0.11958170
  #out of sample MASE 0.07697892
os.fm(5, "reg",mSD5)
  #in sample MASE     0.09144466
  #out of sample MASE 0.08174082
os.fm(5, "lit",mSD3)
  #in sample MASE     0.07574051
  #out of sample MASE 0.21768155
os.fm(6, "reg",mSD5)
  #in sample MASE     0.1318916
  #out of sample MASE 0.2868403
os.fm(6, "lit",mSD2)
  #in sample MASE     0.1097393
  #out of sample MASE 0.1382073
os.fm(7, "reg",mSD1)
  #in sample MASE     0.11552606
  #out of sample MASE 0.05701756
os.fm(7, "lit",mSD2)
  #in sample MASE     0.13932927
  #out of sample MASE 0.07852314

#Question 8

# FUNCTION spm.fplot(sn,pname,model)
#  This function creates a forecasting plot
#  for a store-product-model combination
#
# Inputs:
#  sn - store number (1,2,...,7)
#  pname - product name ("lit" or "reg")
#  model - forecasting model (mSD1 or mSD2)
#
spm.fplot <- function(sn, pname, model){
  X <- select.wsp(SD,sn,pname,1,40)
  FS <- fitted.sales(X,model)
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
#include the forecasting plots corresponding to the
#model-store-product combinations identified in Q6 
#and Q7 above 

#FROM Q6
#Store 7, product Lit, mSD2
spm.fplot(7,"lit",mSD2)
#Store 6, product Reg, mSD5
spm.fplot(6,"reg",mSD5)

#FROM Q7
#Store 4, product Reg, mSD1
spm.fplot(4,"reg",mSD1)
#Store 4, product Reg, mSD1
#same plot
spm.fplot(4,"reg",mSD1)



