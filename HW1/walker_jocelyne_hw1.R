#1
A = matrix(c(1,7,9,3,2,8,5,3,0),nrow=3,ncol=3)
#2
x <- rnorm(10)
mean(x)
var(x)
y <- rnorm(10)
mean(y)
var(y)
#3
set.seed(2567)
x <- rnorm(10)
mean(x)
var(x)
set.seed(2567)
y <- rnorm(10)
mean(y)
var(y)
#4
e <- exp(1)
y = e^(-x/8)*sin(x)
x = seq(1,20, by=1)
plot(x,y)
#5
B <- matrix(10:25,4,4)
B[c(2,3,4),c(2,4)]
#6
Auto$name[38]
#7
pairs(Auto[,c(1,4,5,6)])
