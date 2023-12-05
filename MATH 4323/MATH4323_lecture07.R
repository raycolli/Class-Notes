RNGkind(sample.kind = "Rounding")
### Example 1: non-linear separating hyperplane.
beta <- c(2,-1,0,3,2)
# 2 - X1 + 0*X1^2 + 3*X2 + 2*X2^2 = 0
# => X1 = 2 + 3*X2 + 2*X2^2
curve(2+3*x + 2*x^2, from=-30, to=10,
      xlab="X2",
      ylab="X1")


### Example 2: non-linear separating hyperplane.
plot(NA, NA, type = "n", xlim = c(-1, 7), ylim = c(-5, 3), asp = 1, xlab = "X1", ylab = "X2")
symbols(c(3), c(-1), circles = c(3), add = TRUE, inches = FALSE)




###

# LINEAR KERNEL: MEASURE OF SIMILARITY
# Example

data <- matrix(c(1,2,1,
                 1,4,1,
                 3,5,6,
                 1,2,1,
                 -1,-2,-5), ncol=3, byrow=T)
data
sum(data[1,]*data[2,])
sum(data[1,]*data[3,])
sum(data[1,]*data[4,])
sum(data[1,]*data[5,])
sum(data[1,]*data[5,])



## SVM for Heart disease data

library(e1071)

#Heart <- read.csv("~/Downloads/Heart.csv")
Heart <- read.csv("C:/Users/wwang60/OneDrive - University Of Houston/Documents/!Teaching/MATH4323/dataset/Heart.csv")
dim(Heart)
head(Heart)
summary(Heart)

Heart <- na.omit(Heart)### delete observations with missing info.
dim(Heart)

summary(Heart)

# A cost argument allows us to specify the cost of
# a violation to the margin. When the cost argument is small, then the mar-
#   gins will be wide and many support vectors will be on the margin or will
# violate the margin. When the cost argument is large, then the margins will
# be narrow and there will be few support vectors on the margin or violating
# the margin.

svm.obj <- svm(as.factor(AHD) ~.,
               data=Heart,
               kernel='linear',
               cost=5)

### AUTOMATICALLY DOES SCALING, unless SPECIFIED OTHERWISE

summary(svm.obj)
svm.obj

predict(svm.obj)
head(data.frame(True=Heart$AHD, 
                Predicted=predict(svm.obj)))
mean(predict(svm.obj) != Heart$AHD)


### If the predictor variables include factors, 
### the formula interface must be used to get a correct model matrix.


#### RADIAL BASIS SVM #slide 34

for (j in 3:1){
  svm.obj <- svm(as.factor(AHD) ~.,
                 data=Heart,
                 kernel='radial',
                 gamma=10^{-j},
                 cost=5)
    print(mean(predict(svm.obj) != Heart$AHD))
}

### TEST ERROR ESTIMATION
set.seed(1)
n <- nrow(Heart)
train <- sample(1:n, 0.7*n)


svm.obj <- svm(as.factor(AHD) ~., data=Heart, cost=5,
               kernel='linear', 
               subset=train)
mean(predict(svm.obj, newdata=Heart[-train,]) != Heart$AHD[-train])

for (j in 3:1){
  svm.obj <- svm(as.factor(AHD) ~., data=Heart, cost=5,
                 kernel='radial', gamma=10^{-j},
                 subset=train)
  print(mean(predict(svm.obj, newdata=Heart[-train,]) != Heart$AHD[-train]))
}

summary(svm.obj)

table(pred=predict(svm.obj, newdata=Heart[-train,]), 
      true=Heart$AHD[-train])


##### Default data in ISLR 
library(ISLR)
head(Default)
?Default
table(Default$default)
333/(333+9667)


###########
### SVM with Multiple Classes
###########

library(e1071)

set.seed(1)
x1 <- rnorm(200)
x2 <- rnorm(200)

x1[1:100] <- x1[1:100] + 2
x2[1:100] <- x2[1:100] + 2

x1[101:150] <- x1[101:150] - 2
x2[101:150] <- x2[101:150] - 2

y=c(rep(1,150),
    rep(2,50))

x <- cbind(x1,x2)

set.seed(1)
x=rbind(x, matrix(rnorm(50*2), ncol=2))
y=c(y, rep(0,50))
x[y==0,2] = x[y==0,2]+2
dat=data.frame(x=x, 
               y=as.factor(y))
plot(x,col=(y+1)) # col=1 black; col=2 red; col=3 green

svmfit=svm(y~., data=dat, 
           kernel="radial", 
           cost=10, gamma=.1)
plot(svmfit, dat)

