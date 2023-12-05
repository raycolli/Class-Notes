library(ISLR)
library(class)
#RNGkind(sample.kind = "Rounding")
##########
## Test/Validation set
##########

Smarket

n <- nrow(Smarket)


train <- 1:1000
test <- c(1:n)[-train]


X.train <- Smarket[train,
                   c("Lag1","Lag2","Lag3","Lag4","Lag5","Volume")]
y.train <- Smarket[train, "Direction"]

X.test <- Smarket[test,
                  c("Lag1","Lag2","Lag3","Lag4","Lag5","Volume")]
y.test <- Smarket[test, "Direction"]

set.seed(1)
knn.pred <- knn(train=X.train,
                test=X.test,
                cl = y.train,
                k=1)
mean(knn.pred != y.test)
table(knn.pred,y.test)
### Now we can thoroughly compare KNN models with DIFFERENT Ks
K.set <- seq(1,401, by=10)
knn.test.err <- numeric(length(K.set))

set.seed(1)
for (j in 1:length(K.set)){
  knn.pred <- knn(train=X.train,
                  test=X.test,
                  cl=y.train,
                  k=K.set[j])
  knn.test.err[j] <- mean(knn.pred != y.test)
}

min(knn.test.err)
which.min(knn.test.err)
K.set[which.min(knn.test.err)]


plot(K.set, knn.test.err, 
     type='b',
     xlab="K",
     ylab="Test error")

# K=291 - winner !!! at about 41% error.



## PICKING BEST VARIABLES !!!

## Keep manually changing the var subset, 
## seeing what test error is yielded.

var.subset <- c("Lag1","Lag2")
X.train <- Smarket[train, var.subset]
y.train <- Smarket[train, "Direction"]

X.test <- Smarket[test, var.subset]
y.test <- Smarket[test, "Direction"]

set.seed(1)
knn.pred <- knn(train=X.train,
                test=X.test,
                cl = y.train,
                k=291)
mean(knn.pred != y.test)



## Selecting BOTH variable subset & K.

possible.subsets <- list()
possible.subsets[[1]] <- c("Lag1","Lag2","Lag3", "Lag4", "Lag5","Volume")
possible.subsets[[2]] <- c("Lag1","Lag2","Lag3", "Lag4","Volume")
possible.subsets[[3]] <- c("Lag1","Lag2","Lag3","Volume")
possible.subsets[[4]] <- c("Lag1","Lag2","Volume")
possible.subsets[[5]] <- c("Lag1","Lag2")


### for loop in R #####
for (ind in 1:length(possible.subsets)){
var.subset <- possible.subsets[[ind]] 
X.train <- Smarket[train, var.subset]
y.train <- Smarket[train, "Direction"]

X.test <- Smarket[test, var.subset]
y.test <- Smarket[test, "Direction"]

K.set <- seq(1,401, by=10)
knn.test.err <- numeric(length(K.set))

set.seed(1)
for (j in 1:length(K.set)){
  knn.pred <- knn(train=X.train,
                  test=X.test,
                  cl=y.train,
                  k=K.set[j])
  knn.test.err[j] <- mean(knn.pred != y.test)
}

if (ind == 1){
plot(K.set, knn.test.err,
     type='b',
     xlab="K",
     ylab="Test error",
     ylim=c(0.35,0.60),
     col=length(var.subset))
}

if (ind > 1){
lines(K.set, knn.test.err,
           type='b',
           xlab="K",
           ylab="Test error",
           col=length(var.subset))
}
}


legend("topright",
       legend = c("Lag1-Lag5, Volume",
                  "Lag1-Lag4, Volume",
                  "Lag1-Lag3, Volume",
                  "Lag1-Lag2, Volume",
                  "Lag1-Lag2"),
       col=c(6:2),
       lty=1)

## BEST: ("Lag1", "Lag2") at K=321





#########
##  OJ data example.
#########


OJ
head(OJ)
OJ$StoreID <- NULL

### VALIDATION SET: 80/20

set.seed(1)

n <- nrow(OJ)
train <- sample(1:n, 0.8*n)
X.train <- OJ[train, -1]
y.train <- OJ[train, "Purchase"]
X.test <- OJ[-train, -1]
y.test <- OJ[-train, "Purchase"]

set.seed(1)
knn.pred <- knn(train=X.train,
                test=X.test,
                cl = y.train,
                k=5)

## ERROR !! one of predictors (Store7) is NON-NUMERIC
str(X.train)


## CODING a NON-NUMERIC VARIABLE via a DUMMY VARIABLE.
OJ$Store7 <- ifelse(OJ$Store7 == "Yes", 1, 0)


set.seed(1)
n <- nrow(OJ)
train <- sample(1:n, 0.8*n)
X.train <- OJ[train, -1]
y.train <- OJ[train, "Purchase"]
X.test <- OJ[-train, -1]
y.test <- OJ[-train, "Purchase"]

# Now that should work.
set.seed(1)
knn.pred <- knn(train=X.train,
                test=X.test,
                cl = y.train,
                k=5)

mean(knn.pred != y.test)




set.seed(12345)
for (j in 1:10){
  n <- nrow(OJ)
  train <- sample(1:n, 0.8*n)
  X.train <- OJ[train, -1]
  y.train <- OJ[train, "Purchase"]
  X.test <- OJ[-train, -1]
  y.test <- OJ[-train, "Purchase"]
  
  set.seed(1)
  knn.pred <- knn(train=X.train,
                  test=X.test,
                  cl = y.train,
                  k=5)
  
  print(mean(knn.pred != y.test))
}




###########
### Leave-One-Out Cross-Validation.
#############

X.train <- OJ[,-1]
y.train <- OJ[,"Purchase"]

set.seed(1)
knn.pred <- knn.cv(train=X.train,
                   cl = y.train,
                   k=5)
print(mean(knn.pred != y.train))


### Running for various K
K.set <- 1:50
knn.test.err <- numeric(length(K.set))

for (j in 1:length(K.set)){
  print(j)
  set.seed(1)
  knn.pred <- knn.cv(train=X.train,
                     cl=y.train,
                     k=K.set[j])
  knn.test.err[j] <- mean(knn.pred != y.train)
}

min(knn.test.err)
which.min(knn.test.err)
K.set[which.min(knn.test.err)]


plot(K.set, knn.test.err, 
     type='b',
     xlab="K",
     ylab="Test error",
     main="LOOCV for OJ data")


###########
### K-fold cross-validation
#############
library(caret)
set.seed(1)
train.control <- trainControl(method  = "CV",10)
knnfit <- train(Purchase ~.,
             method     = "knn",
             tuneGrid   = expand.grid(k = 1:20),#tuneGrid: decide which values the main parameter will take
             trControl  = train.control,
             metric     = "Accuracy",
             data       = OJ)
knnfit
plot(knnfit)
