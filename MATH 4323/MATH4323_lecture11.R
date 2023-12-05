#########
### Silhouette
#########
RNGkind(sample.kind = "Rounding")

### MICROARRAY EXAMPLE (cont'd)
library(ISLR)
library(factoextra)


## Silhouette coefficient for one clustering solution
k=3
ec.obj <- eclust(NCI60$data, 
                 FUNcluster="kmeans",
                 k = k,
                 nstart = 50)
ec.obj$silinfo


### Getting optimal K with respect to silhouette coefficient
k.max <- 10
silh.coef <- numeric(k.max)
for (k in 2:10){
  print(k)
  silh.coef[k] <- eclust(NCI60$data, 
                         FUNcluster="kmeans",
                         k = k,
                         graph=0,
                         nstart = 50)$silinfo$avg.width
}

plot(silh.coef,
     type="b", pch=19, col=4,
     main="Silhouette Coefficient for K=1,...,10",
     xlab="K")

which.max(silh.coef)




## 7 appears as the optimal # of clusters.

k=7
ec.obj <- eclust(NCI60$data, 
                 FUNcluster="kmeans",
                 k = k,
                 nstart = 50)
ec.obj$silinfo


### Nice visualization

library(cluster)
sil <- silhouette(ec.obj$cluster, 
                  dist(NCI60$data))
fviz_silhouette(sil)
plot(sil, main ="Silhouette plot - K-means")# black and white but has summary

## Let's check their actual cancer labels
NCI60$labs[ec.obj$cluster == 1]
NCI60$labs[ec.obj$cluster == 3]
NCI60$labs[ec.obj$cluster == 5]
NCI60$labs[ec.obj$cluster == 6]

table(NCI60$labs)
## Those appear as very sensible grouping:
##   Cluster 1 contains pretty much all melanomas
##   Cluster 3: mostly K562 type cancers
##   Cluster 5: mostly MCF-repro type cancers
##   Cluster 6: contains pretty much all colons




### Compare with PCA version.

### Via PCA:
pca.obj <- prcomp(NCI60$data)
dim(pca.obj$x)
n.PCs <- 2
pca.data <- pca.obj$x[,c(1:n.PCs)]               # Extracting the PCs

## Gap statistic

fviz_nbclust(pca.data, kmeans, k.max=10, 
             nstart = 50,  
             method = "gap_stat", 
             nboot = 50)



## Fitting the K-means solution,
## EXTERNAL VALIDATION (via cancer labels)

km.obj <- eclust(pca.data,
                 FUNcluster = "kmeans",
                 k=3,
                 nstart=50)

sil <- silhouette(km.obj$cluster, dist(NCI60$data))
sil
mean(sil[,3])
fviz_silhouette(sil)
plot(sil, main ="Silhouette plot - K-means")

