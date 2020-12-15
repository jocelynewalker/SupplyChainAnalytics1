library(fpp)
library(dplyr)
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
 
 #  (INSERT NECESSARY CODE HERE)
  
  
  return(data.frame(ME=me,MPE=mpe,
                    MAE=mae, MAPE=mape,
                    RMSE=rmse, MASE=mase))
}


fm(mSD1,1,"reg",1,40)
fm(mSD1,1,"reg",41,52)


#Questions 3 and 4

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
#  Matrix with fit metrix for all
#    models in the list for the store
#    and product combination
#
is.fm <- function(sn,pn){
  fmNam <- c("ME","MPE","MAE","MAPE","RMSE","MASE")
  FL <- mL %>%
    
    #  (INSERT NECESSARY CODE HERE)    
    
  return(FL)
}

is.fm(1,"reg")

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
  OSM <- 

  #  (INSERT NECESSARY CODE HERE) 
  
  rownames(OSM)<- rN
  return(OSM)
}


os.fm(2,"lit",mSD1)


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





