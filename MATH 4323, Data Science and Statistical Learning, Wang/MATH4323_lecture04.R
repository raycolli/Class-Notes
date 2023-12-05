library(ISLR)
library(class) # knn()
#?knn
?Smarket
head(Smarket)
str(Smarket)
names(Smarket)
dim(Smarket)
summary(Smarket)
cor(Smarket) # Error msg: "Direction" variable is qualitative.
cor(Smarket[,-9])
plot(Smarket$Volume)
boxplot(Smarket$Volume~Smarket$Year)
#boxplot(Volume~Year, data=Smarket)
## EDA:
plot(Lag1 ~ Lag2, 
     data=Smarket,
     col=ifelse(Direction == "Up", "green", "red"))
legend("topright",
       legend=c("Up", "Down"),
       pch=1,
       col=c("green","red"))

#legend(4,0,legend = c("Up", "Down"),
#       pch=1,
#       col=c("green","red"))

#### knn() function

train.X <- Smarket[,c("Lag1","Lag2","Lag3","Lag4","Lag5","Volume")]
train.Direction <- Smarket[,"Direction"] # label of the dataset Smarket

set.seed(12345)# set seed before apply knn() 
               # because if several obs. are tied as nearest neighbors, then R will randomly break the tie. 
               # Therefore, a seed must be set in order to ensure reproducibility of results.
## Do it for K=1, 3, 100
K <- 3
knn.obj <- knn(train=train.X,
               test=train.X,
               cl=train.Direction,
               k=K)

head(knn.obj)

## Printing predictions next to the true class values
head(data.frame(knn.obj, train.Direction))


3==3

## Calculating TRAINING error.
mean(knn.obj != train.Direction) 
sum(knn.obj != train.Direction)/1250
