data("USArrests")

#install.packages("tidyverse")

library(tidyverse)
library(GGally)#an extenstion for ggplot2 with some new feature

#### PCA on USArrest data from base R #####

?USArrests
#obtain row names
states=row.names(USArrests)
states

#obtain column names
names(USArrests)

pairs(USArrests) #pairs plot
ggpairs(USArrests)


###### apply() function in R
# calculate column means
apply(USArrests,2,mean) #  1 indicates rows, 2 indicates columns
apply(USArrests,2,median)
apply(USArrests,2,sd)

?lapply
?sapply

### prcomp() for principle component analysis
pr.out=prcomp(USArrests,scale=T)
pr.out
plot(pr.out)
names(pr.out)

pr.out$sdev # st. dev. of each PC
pr.out$center # mean of each column/variable
pr.out$scale # standard deviation of each column/variable
             # try sqrt(apply(USArrests,2,var))
pr.out$rotation # loadings
sum((pr.out$rotation[,1])^2) # loadings are normalized

pr.out$x # principle component score vectors: linear combination of x1-x4

dat.proj = as.matrix(scale(USArrests)) %*% pr.out$rotation
dat.proj == pr.out$x # is this true?
sd(dat.proj[,1])
sd(dat.proj[,2])
sd(dat.proj[,3])
sd(dat.proj[,4])

apply(pr.out$x,2,sd) # standard deviation of each principle component
apply(pr.out$x,2,var) # variance of each principle component
ggpairs(as.tibble(pr.out$x))#pairs plot after the rotation

pr.out$sdev # standard deviation of each principle component
pr.var=pr.out$sdev^2
pr.var # variance explained by each principle component: decreasing order !!
plot(pr.out)
#? how much variance is explained by each principle component?
pve = pr.var/sum(pr.var)


### alternative 10.8 part b
pr.out$rotation
USArrests.sc=scale(USArrests)
USArrests.sc %*% pr.out$rotation

colSums((USArrests.sc %*% pr.out$rotation)^2)
sum(colSums((USArrests.sc %*% pr.out$rotation)^2))
colSums((USArrests.sc %*% pr.out$rotation)^2)/sum(colSums((USArrests.sc %*% pr.out$rotation)^2))


plot(pve, xlab="Principle Component", ylab = "Proportion of Variance Explained", ylim=c(0,1),type = 'b')
plot(cumsum(pve), xlab="Principle Component", ylab = " Cumulative Proportion of Variance Explained", ylim=c(0,1),type = 'b')


##### biplot: displays both principal component scores and the principal componnet loadings
pr.out$rotation=-pr.out$rotation #change the sign to replicate the plot in the textbook
pr.out$x=-pr.out$x               #change the sign to replicate the plot in the textbook
biplot(pr.out,scale = 0) # scale =0 ensure that the arrows are scaled to represent the loadings
# state names represent the scores for the first two principle components
# red arrows represent the first two principal component loading vectors with axes on top and right
pr.out$rotation # the word "Rape" is centered at the point (0.54, 0.17)
