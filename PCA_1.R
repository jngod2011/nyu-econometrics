### Principle Component Regression = PCA + OLS
#
# Simu data
set.seed(123)
y <- 1:7 + rnorm(7, sd = 0.2)
x1 <- 1:7 + rnorm(7, sd = 0.2)
x2 <- 1:7 + rnorm(7, sd = 0.2)
x3 <- 1:7 + rnorm(7, sd = 0.2)
x4 <- 1:7 + rnorm(7, sd = 0.2)
data1 <- matrix(c(x1, x2, x3, x4, y), ncol = 5, byrow = F)
# 5 x 7

res <- lm(y ~ x1 + x2 + x3 + x4)
summary(res)

res <- lm(y ~ x1 + x4)
summary(res)

res <- lm(y ~ x4)
summary(res)

pairs(matrix(c(x1, x2, x3, x4, y), ncol = 5, byrow = F),
      labels = c("x1", "x2", "x3", "x4", "y"))

# Simu for data2:
y <- 1:7 + rnorm(7, sd = 0.2)
x1 <- 1:7 + rnorm(7, sd = 0.2)
x2 <- 1:7 + rnorm(7, sd = 0.2)
x3 <- 1:7 + rnorm(7, sd = 0.2)
x4 <- 1:7 + rnorm(7, sd = 0.2)
data2 <- matrix(c(x1, x2, x3, x4, y), ncol = 5, byrow = F)

res <- lm(y ~ x1 + x2 + x3 + x4)
summary(res)

pairs(matrix(c(x1, x2, x3, x4, y), ncol = 5, byrow = F),
      labels = c("x1", "x2", "x3", "x4", "y"))

# run OLS using mean of four x vars as dependent var
xmn1 <- (data1[,1] + data1[,2] + data1[,3] + data1[,4])/4
xmn2 <- (data2[,1] + data2[,2] + data2[,3] + data2[,4])/4
rm1 <- lm(data1[,5] ~ xmn1)
rm2 <- lm(data2[,5] ~ xmn2)
summary(rm1)
summary(rm2)

# Almost all variance explained in first component:
princomp(data1[,1:4])
# Loading structure (of first component)
princomp(data1[,1:4])$loadings[,1]

# Similarly for data2...
princomp(data2[,1:4])

# Save the LS coefficients for predictions:

coef1 <- summary(lm(data1[,5] ~ data1[,1] + data1[,2]+ data1[,3] +
                      data1[,4]))$coefficients[,1]
coef2 <- summary(rm2)$coefficients[,1]

# Note: our full model, mean= PCR, and single variable
# Simulate over 7,000 
nsim <- 7000
error <- 0.2
y <- rep(1:7, 1000) + rnorm(nsim, sd = error)
x1 <- rep(1:7, 1000) + rnorm(nsim, sd = error)
x2 <- rep(1:7, 1000) + rnorm(nsim, sd = error)
x3 <- rep(1:7, 1000) + rnorm(nsim, sd = error)
x4 <- rep(1:7, 1000) + rnorm(nsim, sd = error)

#Test our predicted coefficients

yhat1 <- coef1[1] + matrix(c(x1,x2,x3,x4), ncol=4, byrow=F) %*% as.matrix(coef1[2:5]) # Model 1, multivar LS

test_xmean <- (x1 +x2  +x3 +x4)/4
yhat2 <- coef2[1] + coef2[2] * test_xmean # Model 2, principle component regression
yhat3 <- coef2[1] + coef2[2] * x3 # Model 3, single variable regression

barplot(c(sum((y-yhat1)^2)/nsim, sum((y-yhat2)^2)/nsim, sum((y-yhat3)^2)/nsim),
        col = heat.colors(3), names.arg = c("M LS","PC R","Var x3"), cex.names = 0.9 ,main = "Ave sq pred error, SSR")


### PCA 2

pca_regression <- eigen(cor(spectra))
loadings <- pca_regression[1]


     

        