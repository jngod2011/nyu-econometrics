### ASE II PS1 -- Richard Godfrey 
### Due 18 Feb 2016

### Qn 1 & 2 on paper
### Qn 3

# Use the data included in klein.xls to estimate the following linear model
# using GMM: Ct =α+β1Pt +β2Pt−1 +β3Wpg Your code should be able to estimate
# using the optimal weighting matrix, and provide estimators for the residual
# variance and the significance tests. Test also for the validity of the moment
# conditions.

# import and clean data
rm(list=ls())
setwd("~/Google Drive/NYU Courses/R-II");
klein <- read.csv("klein_data.csv", header=T)
klein$P.lag <- c(NA,klein$P[-length(klein$P)])
klein$Wp.G <- klein$Wp *  klein$G
names(klein$T) <- t
klein <- klein[-1,]
# attach(klein)

# gmm
library(gmm);
my.model <- gmm(g=C ~ P + P.lag + Wp.G, x= ~ I + K + GNP + Wg + T, wmatrix="optimal", data=klein)
summary(my.model)

# Your code should be able to estimate using the optimal weighting matrix 
#   W = S^-1  , S = var covar matrix = ( s^2 0 0 ... 0 s^2 0 ... 0 0 s^2 )
# ie. wmatrix="optimal" by default

# and provide estimators for the residual variance and the significance tests
# ie. residual variance is var * n / (n - k)
var(my.model$residuals)*length(my.model$residuals)/(length(my.model$residuals)-4)

#     estimators for the significance tests
# The weighted sum of squares with the restrictions imposed, nqR must be larger
# than the weighted sum of squares obtained without the restrictions, nq. The
# difference is  [nq-r - nq] --->d  chi-sq(J)

# Test also for the validity of the moment conditions.
# ie. the J-test or Hansen-Sargan test for over-identifying restrictions
# considers whether R-K moments are in line with K identifying parameter conditions
# only valid if W = S^-1
# n.Qn (theta) = n.[g(theta)'.W.g(theta)] --->d  chi-sq(R-K) 
summary(my.model)
# J-test 4.70 (p-val 0.09) and 2 DoF


### Qn 4

#Observe that you can choose to estimate the model with some subset of the
#assets and include or not some instrumental variables in xt (if you choose not
#to put any special instrument in it, it is like you are testing only against
#the constant as instrument). We will call the assets we put in the moment
#conditions test assets. The variables in xt will be instruments. 1. Create a
#computer routine to estimate by GMM the parameters of the model. In all your
#calculations, you are allowed (and I recommend you ) to specify your functions
#so that you just set β = 1 and optimize in one variable. The optimal value of β
#will not be important. Use the two-step procedure described in Greene. 2.
#Estimate the GMM with two test assets: vwr and tbill, and no additional instru-
#ments except the constant. Make sure you report the estimated parameter for
#each step (1 and 2), the estimated standard error of the last step. Include in
#your table as well the J stat and p-values.

returns <- read.csv("Returns.csv", header=T)
returns <- returns[,1:13]

returns$c.ret <- rowSums(returns[c('gcnq','gcsq')])/c(NA,rowSums(returns[c('gcnq','gcsq')])[-dim(returns)[1]])
asset.names <-c('vwr','tbill')  # v-weighted mkt return, tbill return  
BBeta <- 1
mtx <- as.matrix(returns[-1,c(asset.names,"c.ret")])  # test asset matrix
q <- dim(mtx)[1]
g=function(gam,mtx) 1-BBeta*(1+mtx[,c(1,length(asset.names))])*mtx[,length(asset.names)+1]^-gam

### use the 2S-GMM as described in Greene.
# [1] ie. weighting matrix W=I
# estimate S using intial eqn and compute
I <- diag(1,nrow=q,ncol=q)
gmm.model <- gmm(g, x=mtx, weightsMatrix=I, t0=300,optfct="optimize",lower=0,upper=1000)
# [2] W = S^-1 i.e. default weighting matrix
gmm.model <- gmm(g, x=mtx,t0=300,optfct="optimize",lower=0,upper=1000)
#  Params using fn g(theta,x)  x=vwr, tbill   start val t0=300   optfct=[theta is 1x1]  lowerupper bounds
summary(gmm.model) 

# ------------------------------
# Estimate       J-test
#  611.5     58.1 (p-val 0.00)
# ------------------------------

