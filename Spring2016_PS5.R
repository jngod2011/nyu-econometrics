### ASE2 - PS5
### Richard Godfrey - Due 12 May 2016

rm(list=ls())

# Qn 1 - by hand

#########
# Qn 2
########

# (II) We have data for stocks in the file paribas.xlsx. We will estimate
# several GARCH models using these returns. 
# 1. Calculate the daily returns for this share. Take a look at your data. Are 
# there any obvious data errors? Correct if you find any.
# 2. Estimate a GARCH(1,1). Calculate and plot the annualized volatility.
# 3. Test for residual autocorrelation and residual heteroskedasticity.
# 4. Test the hypothesis that this is correctly specified vs having one more lag
# in ARCH or GARCH.

library(quantmod)
library(rugarch)
library(tseries)
library(lmtest)
#BNP<-read.csv(...)

getSymbols("BNP.PA")
BNP <- dailyReturn(BNP.PA)
adf.test(BNP)
# -15.67

## 2 GARCH
spec <- ugarchspec(mean.model = list(armaOrder=c(0,0), var.model=list(model="SGARCH"), garchOrder=c(1,1)))
fit <- ugarchfit(spec=spec, data=BNP, fit.control=list(stationarity=1))
BNP$vol<- sqrt(252)*sigma(fit)
plot(BNP$vol, main="Ann. vol")
### 3 residuals test
show(fit)
# Ljung-Box test, weighted ARCH LM test
# 4 spec test
# eliminate autocorr problem by addition of conditional variance
spec2 <- ugarchspec((mean.model = list(armaOrder=c(0,0), var.model=list(model="SGARCH"), garchOrder=c(2,1)))) 
fit2 <- ugarchfit(spec=spec2, data=BNP, fit.control=list(stationarity=1))

spec3 <- ugarchspec(mean.model = list(armaOrder=c(0,0), var.model=list(model="SGARCH"), garchOrder=c(1,2)))
fit3 <- ugarchfit(spec=spec3, data=BNP, fit.control=list(stationarity=1))

infocriteria(fit)
infocriteria(fit2)
infocriteria(fit3)

# model 1 appears best

########
# Qn 3
########
# (III) Co-integration theory can be used to detect arbitrage opportunities in
# trading. Con- sider the monthly 1-year and 10-year Treasury constant maturity
# rates from October 1968 to October 2015. 
# 1. Are the two series cointegrated? 
# 2. Are the two series threshold-cointegrated? Use the interest spread st :=
# r10,t − r1,t as the threshold variable, where ri,t is the i−year Treasury
# constant maturity rate. 
# 3. If they are threshold-cointegrated, build a multivariate model for the two series.

getSymbols("GS10", src="FRED")
getSymbols("DGS1", src="FRED")
plot(GS10)
plot(DGS1)
# yes, cointgrated series
threshold = GS10 - DGS1
plot(threshold)
library("aTSA") # install alternative TSA with ECM model
alpha = 0.1; beta=0.1 # parameterise
x = matrix(c(GS10,DGS1),nrow=2)
y = alpha*x[,1] + beta*x[,2] 
ecm(y,x)


#######
# Qn 4
######
# (IV) In this problem we work with Clark (1987) and his model of unobserved components:
#   yt =nt + xt
# nt =gt−1 +nt−1 +vt, vt ∼N 0,σv2 
# g t = g t − 1 + w t , w t ∼ N   0 , σ w2  
# xt =φ1xt−1 + φ2xt−2 + et, ∼ N  0, σe2 
# t 0 1 t−1 Here, ε is a standard Gaussian white noise.
# at =σtεt (1) + β σ2 (2)
# 1
# 1 t−1
# 1. Write down the model in state-space representation.
# 2. Estimate this model using macroeconomic data from the FRED database: https:
#   //research.stlouisfed.org/fred2/.

library("quantmod")
library("xts")
library("KFAS")

fredseries = c("GDPC1","CNP16OV")
getSymbols(fredseries, src="FRED")

CNP16OV_mod= CNP16OV/(drop(coredata(CNP16OV)["199209"]))
PcOutput_g = diff(log(GDPC1/CNP16OV_mod), lag=1)*100
plot(PcOutput_g, ylim=1)

# define state space model
zz = matrix(c(1,1,0,0),1,4)
RR = diag(1,4)
RR[3,3]=0
QQ=diag(1,4)
ss0=c(0,0,0,0)

obj_model = function(params){
  phi1 = params[1]
  phi2 = params[2]
  TT = matrix(c(1,0,0,1,0,phi1,phi2,0,0,1,0,0,0,0,0,1),4,4)
  mymodel=SSModel(PcOutput_g ~ -1 + SSMcustom(Z=zz,T=TT, R=RR, Q=QQ, a1=ss0, P1=diag(0,4)))
}

update_model = function(params, model) {
  phi1 = params[1]
  phi2 = params[2]
  TT = matrix(c(1,0,0,1,0,phi1,phi2,0,0,1,0,0,0,0,0,1),4,4)
  model["Q"] = TT
  model
} # rule to change params in the model
pars0 = c(0.1,0.2)
model0=obj_model(pars0)
fitted_model = fitSSM(model0, pars0, update_model)
# function fitSSM specifics SS representations

######
# Qn 5
#######
# (V) Consider Hamilton’s growth model (1989) described by the relation:
# 1. Estimate the model for real GDP using the sample period 1952:II - 1984:IV.
# 2. Extend the sample to 1952:II - 1995:III. Are the parameters obtained reasonable? Explain.
# 3. The model now is replaced by:

library(MSwM)
PcOutput_g = PC.OUTPUT.babyboom = PcOutput_g[,"1952-04-01":"1984-12-01"]
lm4=lm(PcOutput_g~lag(PcOutput_g,1) + lag(PcOutput_g,2) + lag(PcOutput_g,3) + lag(PcOutput_g,4))
Markov_Fit = msmFit(lm4, k=4, sw=c(T,F,F,F,F,F)) # only constants have breaks

PcOutput_g = PC.OUTPUT.genY = PcOutput_g[,"1952-04-01":"1995-09-01"]
lm4=lm(PcOutput_g~lag(PcOutput_g,1) + lag(PcOutput_g,2) + lag(PcOutput_g,3) + lag(PcOutput_g,4))
Markov_Fit = msmFit(lm4, k=4, sw=c(T,F,F,F,F,F)) 
# the parameters are not great, there is non-stationary data evident from plot


