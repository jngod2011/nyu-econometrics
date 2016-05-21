
### Assignment One - Adv Econometrics - Richard Godfrey - 25 Sept 2015

# rm(list=ls()) remove  the list objects in the specified environment
library(maxLik)
# load("DebTrivedi.rda"); 
attach(DebTrivedi); DT <- DebTrivedi; 
dim(DT)
y <- ofp

### (a) Write down the likelihood fn for n independent realizations of Y
### Dataset is 4406 obs of 19 vars (dep and indep)

llikp1 <- function(param) {
  n <- length(y)
  lambd <- param[1]
  logl1<-dpois(y,lambda=lambd,log=TRUE)
  return(logl1)
}
mle1 <- maxLik(logLik=llikp1, grad=NULL, hess=NULL, start=c(lambd=1))
summary(mle1)
mle1$type

llikp2 <- function(param) {
  n <- length(y)
  lambd <- param[1]
  logl2.0 <- log(lambd)*sum(y-n*lambd) - n*lambd - sum(log(factorial(y)))
  logl2.1 <- -lambd + y*log(lambd) - log(factorial(y))
  return(logl2.1)
}
#lambdahash <- 1/n * sum(y) # mle 
mle2 <- maxLik(logLik=llikp2, grad=NULL, hess=NULL, start=c(lambd=1))
summary(mle2)

# delete # lambdaD <- lambda^-1 * sum( x-n)
# delete # pois <- glm(y ~ A + B + x, family = poisson(link=sqrt),data = worm.counts)

# form a matrix X for independent and dummy variables
# form a matrix Y for dependent variable

ofp<-DT[,1]
n<-length(ofp)
const<-rep(1,times=n)
hosp<-DT[,6]
poor<-as.numeric(DT[,7]=='poor')
excellent<-as.numeric(DT[,7]=='excellent')
numchron<-DT[,8]
gender<-as.numeric(DT[,13]=='male')
school<-DT[,15]
privins<-as.numeric(DT[,18]=='yes')

x<-as.matrix(cbind(const,hosp,poor,excellent,numchron,gender,school,privins))
y<-as.matrix(ofp)

# summary of the independent variables: 29.6% hosp, 12.57% poor, 7.8% exc, numchron range 0-8 
# and mean 1.54, 40.4% male, mean school 10.3, 77.6% priv ins.

llikp3<-function(param) {
  loglambd<-x%*%param
  lambd<-exp(loglambd)
  logl<-sum(y*loglambd-lambd)
  return(logl)
}

start.estimators<-c(1,1,1,1,1,1,1,1)
mle3<-maxLik(logLik=llikp3, grad=NULL, hess=NULL, start=start.estimators)
summary(mle3)
glm3 <- glm(ofp~hosp+health+numchron+gender+school+privins, family="poisson")
summary(glm3)  ## estimators match GLM  :-)  


### (b) Write down the gradient and the Hessian

llGradient <- matrix(ncol=8,nrow=1)
grad.lik <-function(param){
  
  loglambd <- x%*%param
  lambd <- exp(loglambd) 
  residual <- y - lambd
  llGradient[1]<-sum(residual*x[,1])
  llGradient[2]<-sum(residual*x[,2])
  llGradient[3]<-sum(residual*x[,3])
  llGradient[4]<-sum(residual*x[,4])
  llGradient[5]<-sum(residual*x[,5])
  llGradient[6]<-sum(residual*x[,6])
  llGradient[7]<-sum(residual*x[,7])
  llGradient[8]<-sum(residual*x[,8])
  return(llGradient)
}


mle4<-maxLik(logLik=llikp3, grad=grad.lik, hess=NULL, start=start.estimators)
summary(mle4)

### forming the Hessian

llHessian<- matrix(ncol=8,nrow=8)

hess.lik <-function(param){
  loglambd <- x%*%t(as.matrix(param))
  lambd <- exp(loglambd)
  for (i in 1:8){
    for (j in 1:8){
      moment <- x[,i]%*%t(x[,j])
      print(dim(moment))
      llHessian[i,j] <- -sum(moment%*%lambd)
    } 
  }
}

# n x k    kxk  k x n

### (c) (i) Use Maxlik with grad and hess options

mle5<-maxLik(logLik=llikp3, grad=NULL, hess=hess.lik, start=start.estimators)
summary(mle5)
mle6<-maxLik(logLik=llikp3, grad=grad.lik, hess=hess.lik, start=start.estimators)
summary(mle6)


#s<-"AdvEcon-PS1-Soln.R" 
#stitch(s, system.file("misc", "knitr-template.Rhtml", package = "knitr"))


