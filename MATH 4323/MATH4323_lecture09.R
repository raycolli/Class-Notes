RNGkind(sample.kind = "Rounding")

library(ISLR)
library(factoextra)

###
# Gene expression data set NCI60
###

dim(NCI60$data)

cluster1 <- eclust(NCI60$data, 
                   FUNcluster="kmeans",
                   k = 3,
                   nstart = 50,
                   graph=0)
ls(cluster1)


NCI60$labs

k.max <- 10
wss <- numeric(k.max)
for (k in 1:k.max){
  wss[k] <- eclust(NCI60$data, 
                   FUNcluster="kmeans",
                   k = k,
                   nstart = 50,
                   graph=0)$tot.withinss
}


plot(wss,
     type="b",
     pch=19,
     col=2,
     main = "Total Within-Cluster Sum of Squares, for K=1,...,10",
     xlab = "Number of Clusters K",
     ylab = "Total WSS")




#######
## Data-based methods
## INTUITION
## Simulated example
########

set.seed(1)
x <- matrix(rnorm(20*2*3), ncol=2)
head(x)
plot(x, xlab = "X1", ylab = "X2",
     pch=19,
     main="Simulated Data")

# Shifting coordinates to separate observations into 3 clusters
x[21:40,2] <- x[21:40, 2] + 6
x[41:60,1] <- x[41:60, 1] + 6
x <- scale(x) 
plot(x, xlab = "X1", ylab = "X2",
     pch=19,
     main="Simulated Data")



library(factoextra)

## Switch k from 2 to 3 to 4 to 5, etc.
km.res <- eclust(data.frame(x), 
                 FUNcluster = "kmeans", 
                 k=5)
                 #graph=0)
print(km.res$tot.withinss)

fviz_cluster(km.res, geom = "point", ellipse.type = "norm",
             palette = "jco", ggtheme = theme_minimal())
km.res



k.max <- 10
wss <- numeric(k.max)
for (k in 1:k.max){
  wss[k] <- eclust(data.frame(x), 
                   FUNcluster = "kmeans", 
                   k=k,
                   nstart=50,
                   graph=0)$tot.withinss
}

plot(wss,
     type="b",
     pch=19,
     col=2,
     main = "Total Within-Cluster Sum of Squares, for K=1,...,10",
     xlab = "Number of Clusters K",
     ylab = "Total WSS")



##########
## "Gap" statistic
##########

set.seed(1)

data.uniform.x1 <- runif(20*3, min(x[,1]), max(x[,1]))
data.uniform.x2 <- runif(20*3, min(x[,2]), max(x[,2]))
data.uniform <- cbind(data.uniform.x1, 
                      data.uniform.x2)

plot(data.uniform.x1,data.uniform.x2)

### UNIFORM GRID???
data.uniform <- expand.grid(seq(min(x[,1]), max(x[,1]), length.out=15),
                            seq(min(x[,2]), max(x[,2]), length.out=4))
plot(data.uniform)

# HOW EXACTLY TO PUT IT TOGETHER?  5x12? 4*15? 10*6?



# par(mfrow=c(1,2))
# plot(x,
#      xlab = "X1", ylab = "X2",
#      pch=19,
#      main="(Original) Simulated Data")
# plot(data.uniform,
#      xlab = "Uniform.X1", ylab = "Uniform.X2",
#      pch=19,
#      main="Uniformly Distributed Data")
# par(mfrow=c(1,1))



k.max <- 10
wss.uniform <- numeric(k.max)
for (k in 1:k.max){
  wss.uniform[k] <- eclust(data.frame(data.uniform), 
                           FUNcluster = "kmeans", 
                           k=k,
                           nstart=50,
                           graph=0)$tot.withinss
}


plot(log(wss),
     type="b",
     pch=19,
     col=2,
     main = "log(WSS) for Original and Uniform data, K=1,...,10",
     xlab = "Number of Clusters K",
     ylab = "log(WSS)")


lines(log(wss.uniform),
      type="b",
      pch=19,
      col=3)

legend("topright",
       legend=c("Original", "Uniform"),
       col=c(2,3),
       lty=c(1,1))

print(log(wss) - log(wss.uniform))
plot(abs(log(wss) - log(wss.uniform)))



fviz_nbclust(data.frame(x), 
             kmeans, 
             nstart = 50,  
             method = "gap_stat", 
             nboot = 50)


ec.obj <- eclust(data.frame(x), 
                 FUNcluster = "kmeans", 
                 nstart=50,
                 nboot=50,
                 graph=0)
ec.obj$gap_stat



###########
## Back to microarray data
###########
library(ISLR)
library(factoextra)

dim(NCI60$data)

###
# Gene expression data set NCI60
###

## Trying to calculate gap statistic:
fviz_nbclust(NCI60$data, 
             kmeans, 
             nstart = 50,  
             method = "gap_stat", 
             nboot = 50)

### ISSUE: TAKES WAY TOO LONG

## How to deal with that?

## 1. Attempt a guess via "elbow" method

dim(NCI60$data)
k.max <- 10
wss <- numeric(k.max)
for (k in 1:k.max){
  print(k)
  wss[k] <- eclust(NCI60$data, 
                   FUNcluster="kmeans",
                   # stand=T,
                   k = k,
                   nstart = 50,
                   graph=0)$tot.withinss
}


plot(wss,
     type="b",
     pch=19,
     col=2,
     main = "Total Within-Cluster Sum of Squares, for K=1,...,10",
     xlab = "Number of Clusters K",
     ylab = "Total WSS")


### Take a guess: K=2,3,4. Check the plots.
km.obj <- eclust(NCI60$data, 
                 FUNcluster="kmeans", 
                 k=4,
                 nstart = 50)

table(NCI60$labs[km.obj$cluster==1])
table(NCI60$labs[km.obj$cluster==2])
table(NCI60$labs[km.obj$cluster==3])
# table(NCI60$labs[km.obj$cluster==4])

## Bar plot of WSS drop-offs
barplot(abs(diff(wss)))



## 2. PCA:

## Try n.PCs=2,3,4,5 
pca.obj <- prcomp(NCI60$data)
dim(pca.obj$x)
n.PCs <- 2
pca.data <- pca.obj$x[,c(1:n.PCs)]               # Extracting the PCs


k.max <- 10
wss <- numeric(k.max)
for (k in 1:k.max){
  wss[k] <- kmeans(pca.data, 
                   centers = k,
                   nstart = 50)$tot.withinss
}

plot(wss,
     type="b",
     pch=19,
     col=2,
     main = "Total Within-Cluster Sum of Squares, for K=1,...,10",
     xlab = "Number of Clusters K",
     ylab = "Total WSS")

## Bar plot of WSS drop-offs
barplot(abs(diff(wss)))

## Gap statistic

fviz_nbclust(pca.data, kmeans, k.max=10, 
             nstart = 50,  
             method = "gap_stat", 
             nboot = 50)

## Fitting the K-means solution,
## EXTERNAL VALIDATION (via cancer labels)

km.obj <- eclust(pca.data,
                 FUNcluster = "kmeans",
                 nstart=50,
                 nboot=50)
table(NCI60$labs[km.obj$cluster==1])
table(NCI60$labs[km.obj$cluster==2])
table(NCI60$labs[km.obj$cluster==3])
# table(NCI60$labs[km.obj$cluster==4])
# table(NCI60$labs[km.obj$cluster==5])


## 3. SILHOUETTE coefficient: See next set of lecture slides/code.








#######################
### Khan data set
#######################

dim(Khan$xtrain)
Khan$ytrain

## Trying to calculate gap statistic:
fviz_nbclust(Khan$xtrain, 
             kmeans, 
             nstart = 50,  
             method = "gap_stat", 
             nboot = 50)

### ISSUE: TAKES WAY TOO LONG

## How to deal with that?

## 1. Attempt a guess via "elbow" method

dim(Khan$xtrain)
k.max <- 10
wss <- numeric(k.max)
for (k in 1:k.max){
  print(k)
  wss[k] <- eclust(Khan$xtrain, 
                   FUNcluster="kmeans",
                   # stand=T,
                   k = k,
                   nstart = 50,
                   graph=0)$tot.withinss
}


plot(wss,
     type="b",
     pch=19,
     col=2,
     main = "Total Within-Cluster Sum of Squares, for K=1,...,10",
     xlab = "Number of Clusters K",
     ylab = "Total WSS")

### Take a guess: K=2,3,4. Check the plots.
km.obj <- eclust(Khan$xtrain, 
                 FUNcluster="kmeans", 
                 k=4,
                 nstart = 50)

table(Khan$ytrain[km.obj$cluster==1])
table(Khan$ytrain[km.obj$cluster==2])
table(Khan$ytrain[km.obj$cluster==3])
# table(NCI60$labs[km.obj$cluster==4])


## 2. PCA:

## Try n.PCs=2,3,50
pca.obj <- prcomp(Khan$xtrain)
dim(pca.obj$x)
n.PCs <- 2
pca.data <- pca.obj$x[,c(1:n.PCs)]               # Extracting the PCs


k.max <- 10
wss <- numeric(k.max)
for (k in 1:k.max){
  wss[k] <- kmeans(pca.data, 
                   centers = k,
                   nstart = 50)$tot.withinss
}

plot(wss,
     type="b",
     pch=19,
     col=2,
     main = "Total Within-Cluster Sum of Squares, for K=1,...,10",
     xlab = "Number of Clusters K",
     ylab = "Total WSS")

## Gap statistic

fviz_nbclust(pca.data, kmeans, k.max=10, 
             nstart = 50,  
             method = "gap_stat", 
             nboot = 50)

## Fitting the K-means solution,
## EXTERNAL VALIDATION (via cancer labels)

km.obj <- eclust(pca.data,
                 FUNcluster = "kmeans",
                 nstart=50,
                 nboot=50)

table(Khan$ytrain[km.obj$cluster==1])
table(Khan$ytrain[km.obj$cluster==2])
table(Khan$ytrain[km.obj$cluster==3])
table(Khan$ytrain[km.obj$cluster==4])


## 3. SILHOUETTE coefficient: See next set of lecture slides/code.
