## Income dataset

# Via "Import Dataset" -> "From text (base)", find the location of downloaded file.
setwd("C:/Users/wwang60/OneDrive - University Of Houston/Documents/!Teaching/MATH4323/dataset") # set your working directory
Income1 <- read.csv("Income1.csv")
Income1
str(Income1)

head(Income1)
plot(Income ~ Education, 
     data=Income1,
     col="red",
     pch=1,
     type="b")


## Unfortunately the book doesn't specify the formula
## for true curve, hence I had to use 
## polynomial regression of degree 3 in order to
## try and estimate it.
## It is not exactly the same as in the slides.

lm.obj <- lm(Income ~ poly(Education,3), data=Income1)
lines(x=Income1$Education, y= predict(lm.obj),
      col="blue",
      lwd=2)




