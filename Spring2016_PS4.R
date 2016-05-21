
### ASE2 - PS4 - April 2016
### Richard Godfrey

# Qn 1, 2, 3 - by hand

# Qn 4 
# Consider example 21.3 in Greene, where he reviews the estimation
# by Dickey and Fuller of a unit root for the output in the US
# We will re-estimate the model with an augmented sample in this problem. The model is:
# yt =μ+ ·t+ yt 1 + (yt 1  yt 2)+✏t

library("quantmod")
library("xts")
library("tseries")
library("dynlm")

getSymbols("INDPRO", src="FRED")
INDPRO <- to.quarterly(INDPRO) 
INDPRO <- to.quarterly(INDPRO)[,4]
LINDPRO <- log(INDPRO)
L1INDPRO <- lag(LINDPRO,1)
L2INDPRO <- lag(LINDPRO,2)
L3INDPRO <- lag(LINDPRO,3)
L4INDPRO <- lag(LINDPRO,4)
L5INDPRO <- lag(LINDPRO,5)
L6INDPRO <- lag(LINDPRO,6)
L7INDPRO <- lag(LINDPRO,7)
L8INDPRO <- lag(LINDPRO,8)
L9INDPRO <- lag(LINDPRO,9)
L10INDPRO <- lag(LINDPRO,10)
L11INDPRO <- lag(LINDPRO,11)
L12INDPRO <- lag(LINDPRO,12)
L13INDPRO <- lag(LINDPRO,13)
L14INDPRO <- lag(LINDPRO,14)
L15INDPRO <- lag(LINDPRO,15)

lagg <- merge(LINDPRO, L1INDPRO, L2INDPRO, L3INDPRO, L4INDPRO, L5INDPRO, L6INDPRO, L7INDPRO, 
            L8INDPRO, L9INDPRO, L10INDPRO, L11INDPRO, L12INDPRO, L13INDPRO, L14INDPRO, L15INDPRO)

diff <- matrix(NA, nrow=dim(lagg)[1],ncol=dim(lagg)[2])
for (j in 1:dim(lagg)[2])
{
  diff[,j] <- diff(lagg[,j])
}
names(lagg) <-c("l0","l1","l2","l3","l4","l5","l6","l7","l8","l9","l10","l11","l12","l13","l14","l15")
colnames(diff) <-c("d0","d1","d2","d3","d4","d5","d6","d7","d8","d9","d10","d11","d12","d13","d14","d15")

trend <-c(1:length(LINDPRO))

data <- merge(lagg,diff,trend)

# 2 . Determine the optimal lag length in the augmented regression.

y <- "l0"
x <- c("trend", "l1","d1","d2","d3","d4","d5","d6","d7","d8","d9","d10","d11","d12","d13","d14","d15")
AIC <- NULL
BIC <- NULL
for(pick in 1:15)
{
  modell<-as.formula(paste(y,paste(x[1:(2+pick)],collapse="+"),sep="~"))
  fit <- lm(modell)
  AIC[pick]<-AIC(fit)
  BIC[pick]<-BIC(fit)
}

plot(AIC, type="l")
lines(BIC,col="black")
points(which.min(AIC), min(AIC), col="gold")
points(which.min(BIC), min(BIC), col="gold")
which.min(AIC)
which.min(BIC)
# best is indicated

# 3. Compute the relevant DF statistics for this regression.

adf.test(l0)
modell<-as.formula(paste(y,paste(x[1:(2+pick)],collapse="+"),sep="~"))
fit<-lm(modell)

# 4. Is there enough evidence to say that the output has a unit root?

# H0: non stationarity is not rejected becuase DF < -3.41 critical value at 5%
# the series may be non-stationary

adf.test(na.omit(l0~l1))

# the difference in stationary; reject the null of non-stationarity.


######################
### Qn 5
######################

# # (V) Download data for ALCOA from January 2000 until today from
# finance.google.com. Are the dialy log-returns of ALCOA predictable? Test this
# hypothesis: 
# 1. Using the first 5 lags of the autocorrelation function. 
# 2. Using the first 10 lags of the autocorrelation function. Draw your conclusion
# using a 5% significance level.

getSymbols("AA")
Alcoa <- log(AA$AA.Close)
plot(Alcoa,type="l", ylab="log close px")
returns <- 100*diff(Alcoa)
plot(returns,type="l", ylab="returns")
# returns are stationary
adf.test(returns[-1])

returns <- returns[-1]
acf(returns, lag.max=5)
pacf(returns, lag.max=5)

# try an ARMA(5,1)
summary(arma(returns, order=c(5,1)))

acf(returns, lag.max=10)
pacf(returns, lag.max=10)

# no decay in the PACF: test for GARCH or ARIMA i.e. ARMA is not viable.

# Qn 6 -- cancel 

# (VI) Solve application 2 in chapter 21 of Greene’s textbook.
# Carry out an ADF test for a unit root in the rate of inflation using the subset of
# the data in Appendix Table F5.2 since 1974.1. (This is the first quarter after the oil
# shock of 1973.) -- cancel

# Qn 7 - by hand


