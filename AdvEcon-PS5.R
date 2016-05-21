###  Adv Econ  --  PS5 --  Richard Godfrey  --  Weds 16th Dec

### Qn 1
# (a) Generate 2000 draws of a latent variable y∗ = k + 3x + u, where u ∼ N[0, 3] and the regressor x ∼ uniform[0, 1]. Choose k such that you generate approximately 30% of y∗ to be negative.

library(mixtools)
set.seed(123)
n <- 2000
u <- matrix(nrow=n,ncol=1)
#u <- rmvnorm(n, mu=0, sigma=3)
u <- rnorm(n, mean=0, sd=3)
x <- runif(n, min = 0, max = 1)
k <- -0.5
ystar <- k + 3*x + u
curve(dunif(x, 0, 1), from=-1, to=2)
plot(x, ystar)
# (b) Estimate the model using all 2,000 observations using OLS.
model <- ystar ~ x
ols <- lm(model)
summary(ols)
plot(x, ols$fittedvalues)

# (c) If Y*<0, set Y=0. Estimate the Tobit model and compare the parameter
# estimates to the OLS estimates.
y <- matrix(nrow=n,ncol=1)
for (i in 1:n){ 
  if (ystar[i]<0) y[i]<-0 else y[i]<-ystar[i]
}
plot(x, y, col='red')
truncated <- y ~ x
library(AER)
trunc <- tobit(y ~ x)
summary(tobit)

# censored
# ystar = -0.35 + 2.88 x
# y = 0.92 + 1.98 x

# (d) Generate a truncated subsample by excluding observations that correspond
# to Y*< 0. Compare the mean and variance to that of the entire sample.

data <- cbind(x, ystar)
data.trunc <- subset(data, ystar>=0)
summary(data.trunc)  # mean ystar truncated 2.91 
summary(data)  # mean ystar 1.08
plot(data.trunc$x, data.trunc$ystar)
mean(data.trunc$ystar); var(data.trunc$ystar)
mean(data$ystar); var(data$ystar)

data.tr <- matrix(nrow=n, ncol=2)
data.tr <- data[which(ystar > 0),]
mean(data.tr); var(data.tr)

# (e) Using the truncated subsample of y > 0, estimate the model by OLS.
# Evaluate your results in the light of the theoretical properties of OLS.

trunc.ols <- lm(ystar ~ x, data=data.tr)
summary(trunc.ols)

# (f) Use a truncated regression model to estimate the parameters for the data
# set where y>0. Compare with the least-squares results from part e).
y.t <- data.tr$ystar
lliken <- function(param) {
  n <- length(y.t)
  mu <- param[1]
  sigmas <- (param[2]^0.5)
  logl1<-dnorm(y.t, mean=mu,sd=sigmas,log=TRUE)
  return(logl1)
}
mle1 <- maxLik(logLik=lliken, grad=NULL, hess=NULL, start=c(mu=1,sigmas=1))
summary(mle1)
mle1$type


### Qn 2 
#        see paper
# Poisson regression model where y* has density y*ey /y*! y* 0,1,2... .i
# Due to a coding error we only fully observe y* when y* ≥ 2. When y* = 0 or 1 we
# only observe that y* ≤ 1. Suppose this is coded as y* = 1. Define the observed
# data as: y = y∗ for yi* ≥ 2, and y = 1 for yi* = 0 or 1.

# a) Write down the density f (y) of the observed y.

# b) Obtain E[y].

# c) Now introduce regressors with E[y*|x] = exp(x) and define the indicator
# variable d = 1 for y* ≥ 2 and d = 0 for y∗ = 0 or 1. Give the exact formula
# for this example of the objective function of an estimator that provides a
# consistent estimator of  using data on yi, di, and xi

