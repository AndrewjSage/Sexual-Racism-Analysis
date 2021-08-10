# Z's are uncorrelated
Z1 <- rnorm(1000,0,1)
Z2 <- rnorm(1000,0,1)
Z3 <- rnorm(1000,0,1)
Z4 <- rnorm(1000,0,1)

# X is an explanatory variable that is a function of the Z's
X <- Z2 + Z3

# Two responses, Y1 and Y2 are uncorrelated with Z's. 
Y1 <- Z1 + Z2
Y2 <- Z3 + Z4

# Y's are correlated with X, but not with each other. 
cor(Y1,X)
cor(Y2,X)
cor(Y1, Y2)

