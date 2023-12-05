library(dendextend) # for comparing two dendrograms
library(factoextra)
x <- matrix(c(-0.6,-1,
              0.05,-1.1,
              -1.5,-0.4,
              -1.4,-1.55,
              1.3,-0.3,
              -0.9,-1.2,
              1.4,0,
              0.65,-0.25,
             -0.1,0.9),
           ncol=2,
           byrow=T)

plot(x)

dist(x)
round(dist(x),2)#keeping 2 decimals


plot(hclust(dist(x)), 
     main="Complete Linkage")

plot(hclust(dist(x),method = "single"), 
     main="Single Linkage")

plot(hclust(dist(x),method = "average"))
#############
### Gene Expression example
###
##  Please SEE THE BOOK, SECTION 10.6, mostly 10.6.2 (Lab for Chapter 10)
#############

library(ISLR)

nci.labs <- NCI60$labs
nci.data <- NCI60$data

dim(nci.data)

nci.labs[1:4]
table(nci.labs)

## Normalizing the data
sd.data <- scale(nci.data)


## Plot the dendrogram for complete linkage
data.dist <- dist(sd.data)
plot(hclust(data.dist), 
     labels=nci.labs, main="Complete Linkage", xlab="", sub="",ylab="")


# Clearly cell lines within a single cancer type do tend to cluster together, although the
# clustering is not perfect. We will use complete linkage hierarchical clustering
# for the analysis that follows.


## K=2 appears appropriate
hc.out <- hclust(data.dist)
hc.clusters=cutree(hc.out,2)
table(hc.clusters,nci.labs)

#### compute 2 hierachical clusterings
hc1 <- hclust(data.dist,method = "complete")
hc2 <- hclust(data.dist,method = "ward.D2")


#### create a circular plot
dend_plot <- fviz_dend(hc1)   # first, create full dendogram
dend_data <- attr(dend_plot, "dendrogram") # extract plot info
dend_cuts <- cut(dend_data, h = 150) # cut the tree at a desired height
fviz_dend(dend_cuts$lower[[1]], type = 'circular') # make a circular plot


# Create two dendrograms
# more info.: https://uc-r.github.io/hc_clustering
dend1 <- as.dendrogram (hc1)
dend2 <- as.dendrogram (hc2)

tanglegram(dend1, dend2)


dend_list <- dendlist(dend1, dend2)

tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, # Turn-off dashed lines
           common_subtrees_color_lines = FALSE, # Turn-off line colors
           common_subtrees_color_branches = TRUE, # Color common branches 
           main = paste("entanglement =", round(entanglement(dend_list), 2))
)

## To confirm our guess, we can use SILHOUETTE coefficients:
##   The progression of silhouette coef values as K increases
##  or GAP STATISTIC:
##    The progression of gap statistic values as K increases

library(factoextra)
fviz_nbclust(sd.data, hcut, 
             method="silhouette",
             hc_method = "complete")

## Takes a while, but it's possible.
fviz_nbclust(sd.data, hcut, 
             method="gap",
             nboot=25,
             hc_method = "complete")



## Hierarchical Clustering for Complete, Average and Single Linkage
par(mfrow=c(1,3))
data.dist <- dist(sd.data)
plot(hclust(data.dist), 
     labels=nci.labs, main="Complete Linkage", xlab="", sub="",ylab="")
plot(hclust(data.dist, method="average"), 
     labels=nci.labs, main="Average Linkage", xlab="", sub="",ylab="")
plot(hclust(data.dist, method="single"), 
     labels=nci.labs,  main="Single Linkage", xlab="", sub="",ylab="")
par(mfrow=c(1,1))

# Typically, single linkage will tend
# to yield trailing clusters: very large clusters onto which individual observations attach one-by-one.
# 
# On the other hand, complete and average linkage tend to yield more balanced, 
# attractive clusters. 
# (DOWNSIDE: sensitive to OUTLIERS:
#  https://nlp.stanford.edu/IR-book/html/htmledition/single-link-and-complete-link-clustering-1.html
# )
#
# For this reason, complete and average linkage are generally preferred to single linkage. 


########
## Comparing various linkages:
##   Getting the optimal K for each linkage
##   Comparing the silhouette coefficient of best clustering solutions for each linkage
########

### Let's use silhouette coefficient to pick optimal K
##   (as gap statistic calculation takes a while)
library(factoextra)

## K=2 optimal.
fviz_nbclust(sd.data, hcut, method="silhouette", 
             hc_method = "complete")

## K=2 optimal.
fviz_nbclust(sd.data, hcut, method="silhouette", 
             hc_method = "average")

## K=2 optimal.
fviz_nbclust(sd.data, hcut, method="silhouette", 
             hc_method = "single")


### Comparing the silhouette coefficients of optimal solutions
##  for each linkage type.

hc.out=eclust(sd.data, 
              FUNcluster = "hclust",
              hc_method="complete",
              k=2)
print(hc.out$silinfo$avg.width)

hc.out=eclust(sd.data, 
              FUNcluster = "hclust",
              hc_method="average",
              k=2)
print(hc.out$silinfo$avg.width)

hc.out=eclust(sd.data, 
              FUNcluster = "hclust",
              hc_method="single",
              k=2)
print(hc.out$silinfo$avg.width)



## Average linkage yields best solution.

hc.out <- hclust(data.dist, method="average")
plot(hc.out, 
     labels=nci.labs, main="Average Linkage", xlab="", sub="",ylab="")

## Very clean subdivision of cancer types:
##  All K562 and Leukemia types are in cluster 2,
##  The rest are in cluster 1.
hc.clusters=cutree(hc.out,2)
table(hc.clusters,nci.labs)

