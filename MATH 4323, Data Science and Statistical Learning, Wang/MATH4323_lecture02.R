###
## Show installation of ISLR
###
# install.packages("ISLR")

library(ISLR)

rm(list = ls()) # clear your environment
####################
####################
### Wage example ###
####################
####################

Wage
head(Wage)
summary(Wage)
str(Wage)
dim(Wage)
####
##  Wage vs Age scatterplot + smoothing.
####
plot(wage ~ age, 
     data=Wage,
     col='grey')
plot(Wage$wage ~ Wage$age, col='red',main="practice 1")
lo <- loess(wage ~ age, data=Wage)
x.pred <- seq(min(Wage$age), max(Wage$age), by=1)
lines(x=x.pred, 
      y=predict(lo, newdata=x.pred), 
      col='blue', lwd=2)


#######
## Wage vs Year: scatterplot + smoothing.
######

plot(wage ~ year, 
     data=Wage,
     col='grey')
lo <- loess(wage ~ year, data=Wage)
x.pred <- seq(min(Wage$year), max(Wage$year), by=1)
lines(x=x.pred, 
      y=predict(lo, newdata=x.pred), 
      col='blue', lwd=2)

## Roughly the same if you were to just fit a linear regression line.
abline(lm(wage ~ year, data=Wage), col='red', lwd=2)

summary(lm(wage ~ year, data=Wage))
######
## Wage vs Education: boxplots.
#######

# Basic version.
plot(wage ~ education, data=Wage)

# Version with color.
plot(wage ~ education,    
     data=Wage,
     col=c(2:6))

# Version with 1,2,..,5 enumeration of education categories.
edu.num <- Wage$education
levels(edu.num) <- c(1:5)
plot(wage ~ edu.num,    
     data=Wage,
     col=c(2:6),
     xlab="Education Level",
     ylab="Wage")


#######################
#######################
### Smarket example ###
#######################
#######################

Smarket
head(Smarket)

########
## Yesterday boxplot.
########

# Basic version.
plot(Lag1 ~ Direction, 
     data=Smarket) 

### If you wish all three plots on the same frame:
# par(mfrow=c(1,3))

# Full-on version, with names & colors.
plot(Lag1 ~ Direction, 
     data=Smarket,
     col=c("lightblue","orange"),
     xlab="Today's Direction",
     ylab="Percentage Change in S&P",
     main="Yesterday")


########
## Two/Three days previous boxplots
########

plot(Lag2 ~ Direction, 
     data=Smarket,
     col=c("lightblue","orange"),
     xlab="Today's Direction",
     ylab="Percentage Change in S&P",
     main="Two Days Previous")

plot(Lag3 ~ Direction, 
     data=Smarket,
     col=c("lightblue","orange"),
     xlab="Today's Direction",
     ylab="Percentage Change in S&P",
     main="Three Days Previous")

## Don't forget to go back to one plot per frame.
par(mfrow=c(1,1))
 


########################################
########################################
#### Gene Expression example: PCA   ####
########################################
########################################


# data()  to check which data sets are loaded in your R workspace.

dim(NCI60$data)

PC.NCI60 <- prcomp(NCI60$data, scale=T)
z1 <- PC.NCI60$x[,1]
z2 <- PC.NCI60$x[,2]

## No clusters/colors
plot(z1,z2)


## With clusters (resulting from K-means) and colors.
clust.NCI60 <- kmeans(data.frame(z1,z2), centers=4)
clust.NCI60$cluster
plot(z1,z2,
     col = clust.NCI60$cluster+1,
     pch=16)

