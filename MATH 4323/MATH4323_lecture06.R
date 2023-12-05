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



