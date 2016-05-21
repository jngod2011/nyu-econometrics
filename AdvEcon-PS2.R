###   Adv Econ -- Problem Set 2 -- Richard Godfrey -- Due Wed 21 Oct

# 1. Use GMM to estimate a Poisson regression for the Deb-Trivedi data set that
# we used in assignment #1. The dependent variable is ofp (# of office visits)
# and the rhs variables are hosp, numchron, gender, health, school and privins.

load("DebTrivedi.rda")
library(gmm)
DT <- DebTrivedi; 
#attach(DT);
dim(DT)
#y <- ofp

# setup dummy variables
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

x.data <- as.matrix(cbind(const,hosp,poor,excellent,numchron,gender,school,privins))
y <- as.matrix(ofp)
full.data <- cbind(as.matrix(ofp),as.matrix(x.data)[,1:8])

# define regression functions
regression1 <- ofp ~ const+hosp+numchron+gender+poor+excellent+school+privins
regression2 <- y ~ x.data
  
# define the logistic function
logistic <- function(theta, data){
  return(1/(1+exp(-data %*% theta)))  
}

# define the moments
moments <- function(theta, data) {
  print(theta)
  y.moments <- as.numeric(data[,1])
  # y.moments <- as.numeric(y) 
  x.moments <- data.matrix(data[,2:9])  # added back in constant col 1
  # x.moments <- as.matrix(x.data[,2:8])  # col 1 is a constant, drop
  #residual <- logistic(theta, x.moments)
 # r<-(D[,1]-exp(D[,2:9]%*%m))
  residual <- exp(x.moments %*% theta)  # [n x 1]
  m <- x.moments * as.vector((y.moments - residual))
  #print(table(dim(y.moments)),dim(x.moments),dim(residuals))
  #print(dim(cbind(m)))
  return(cbind(m))
}


H<-function(b,D){
  m<-matrix(c(b[1],b[2],b[3],b[4],b[5],b[6],b[7],b[8]));
  r<-(D[,1]-exp(D[,2:9]%*%m));
  v2<-D[,2]*r;
  v3<-D[,3]*r;
  v4<-D[,4]*r;
  v5<-D[,5]*r;
  v6<-D[,6]*r;
  v7<-D[,7]*r;
  v8<-D[,8]*r;
  v9<-D[,9]*r;
  cbind(v2,v3,v4,v5,v6,v7,v8,v9)
}

# data is defined as full.data [1,2:9] 
head(full.data)
data_lm_ols <- lm(ofp ~ hosp + poor + excellent + numchron + gender + school + privins) # no constant
summary(data_lm_ols)

init <- data_lm_ols$coef
logistic_gmm1 <- gmm(g=H, x=full.data, t0=init, type="iterative", crit = 1e-25, 
                     wmatrix ="optimal", method="Nelder-Mead", control=list(maxit=20000)) #reitol=1e-25
summary(logistic_gmm1)

logistic_gmm2 <- gmm(moments, x=full.data, t0=init)
summary(logistic_gmm2)


# 2. Compare the GMM results to those from the R glm command with a
# family=’poisson”.

regression3 <- ofp ~ hosp + poor + excellent + numchron + gender + school + privins # no constant
glm3 <- glm(regression3, family="poisson")
summary(glm3)  ## estimators match MLE

#s<-"AdvEcon-PS2.R" 
#stitch(s, system.file("misc", "knitr-template.Rhtml", package = "knitr"))
#script.dir <- dirname(sys.frame(1)$ofile)
