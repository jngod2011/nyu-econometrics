### Adv Econometrics PS4    Due 30 Nov 2015  Richard Godfrey

setwd("~/Google Drive/NYU Courses/R");
qreg <- read.csv("qreg0902.csv")
head(qreg); dim(qreg); class(qreg)
N <- nrow(qreg)
qreg <- data.frame(const=1,qreg)

# a) OLS test
model <- lmed ~ ltotal
ols <- lm(model, data=qreg)
summary(ols)

# b) test for heteroskedasticity
# Durbin-Wu-Hausman (exogeneity test via a wald test), White exogeneity.
# Bruesch-Pagan heteroskedasticity test
library(lmtest)
bptest(model, data=qreg) # heteroskedastic errors as p-value < 0.01 and BP = 77.66

bptest(model, ~ltotal^2, data=qreg) # 77.66
bptest(model, ~ltotal^2 + ed, data=qreg) # 80.94
bptest(model, ~ltotal^2 + ed^2 + lmed^2 + lmed*ltotal, data=qreg) # 315
bptest(ols, ~ltotal*ltotal + lmed*lmed + lmed*ltotal, data=qreg) # 315

library(bstats); white.test(ols) # extinct package
# c) Obtain median estimates and compare these to the OLS estimates.
library(quantreg)
qr50 <- rq(model, data=qreg, tau=0.5)
summary(qr50)
# d) Obtain quantile regression estimates for q = 0.25 and q = 0.75.
qr25 <- rq(model, data=qreg, tau=0.25)
qr75 <- rq(model, data=qreg, tau=0.75)
anova(qr25,qr50,qr75)
# e) Plot lmed against ltotal along with the quantile regression estimated lines for q=0.1, q=0.5
# and q=0.9.
quantiles <- c(0.1,0.5,0.9)
quantreg.all <- rq(model, tau=seq(0.05, 0.95, by=0.05), data=qreg)
quantreg.all
summary(quantreg.all)
fit1 <-summary(rq(model, tau=seq(0.05, 0.95, by=0.05), data=qreg))
fit2 <-summary(rq(model, tau=c(0.1,0.5,0.9), data=qreg))
plot.rqs(quantreg.all)
pdf("Vietcoef.pdf",width=6.5,height=6.5)
plot(fit1,mfrow = c(2,1)) # best plot with CI.
lines(model, add=TRUE)
dev.off()
latex(fit1, caption="Viet", transpose=TRUE)
### the intercept has become a centercept.

# (e) Figure 1. Scatterplot and Quantile Regression Fit. Superimposed on the
# plot are the {.05,.1,.25,.75,.90,.95} quantile regression lines in gray, the
# median fit in solid black, and the least squares estimate of the conditional
# mean function as the dashed (red) line
attach(qreg)
plot(ltotal,lmed,cex=.25,type="n",xlab="Household Income", ylab="Med Expenditure") 
points(ltotal,lmed,cex=.5,col="blue")
abline(rq(lmed~ltotal,tau=.5),col="black")
abline(lm(lmed~ltotal),lty=2,col="red") #the dreaded ols line 
taus <- c(.05,.1,.25,.75,.90,.95)
for( i in 1:length(taus)){abline(rq(lmed~ltotal,tau=taus[i]),col="gray")}

# f) Test the null hypothesis that the slope coefficients obtained for q=0.1 and q=0.9 are equal.

q1 <- summary(rq(model, tau=0.1, data=qreg))
q9 <- summary(rq(model, tau=0.9, data=qreg))
anova.rq(q1,q9) ### error $ operator is invalid
anova(rq(model,tau=0.1), rq(model,tau=0.9))   ###  Df 1 F val 99.3 P val (that >F) 0.000 
### reject the null hypothesis that the slopes of q1 and q9 are equal! 
anova(rq(model,tau=0.1,data=qreg), rq(model,tau=0.9,data=qreg))

anova(rq(model,tau=0.5), lm(model))   ### error in F test on OLS vs q0.5

### z <- (slope1-slope2 )/(sq rt(SE.1^2 + SE.2^2)) else test in this form.

# The whites.htest() function implements White's test for heteroskedasticity for
# vector autoregressions (VAR). It requires a varest object as input. However,
# from your description it seems that your model is not a VAR (vector
# autoregression) but a simple linear model.
# 
# Hence, the model should be estimated by lm() as previously suggested in the
# comments. Then you can use the bptest() function from the lmtest package to
# carry out White's test. The latter requires that you set up the terms in the
# auxiliary model yourself. It should look like this:
# 
# m <- lm(A ~ B + C, data = dataset) bptest(m, ~ A*B + I(A^2) + I(B^2), data =
# dataset) You can also look at
# 
# help("CigarettesB", package = "AER") for a worked example.


# c) 

qqplot(qreg$ltotal,log(qreg$lmed), col='red')
qqnorm(log(qreg$lmed), col='green')
qqnorm(log(qreg$ltotal), col='green')
qqline(log(qreg$lmed), col='red',lwd='red')

#  The World’s survey data of 1997 Vietnam Living Standards consists of 5004 households that have positive medical expenditures. The variables in the data set (qreg0902.csv) are as follows:
#  Age – age of the head of household
#  Ed – years of education for the head of household
#  Farm – an indicator of whether the household lives on a farm.
#  Urban – an indicator of whether the household is urban or rural
#  hhsize – number of persons living in the household.
#  ltotal – natural log of total household expenditures
#  lmed – natural of total medical expenditures
#  lfood – natural log of expnditures on food.
# a) Using lntotal as a proxy for household income obtain OLS estimates for the following model:
#   lmedi = a + b*ltotali + ui
# b) Test for heteroskedasticity.
# c) Obtain median estimates and compare these to the OLS estimates.
# d) Obtain quantile regression estimates for q = 0.25 and q = 0.75.
# e) Plot lmed against ltotal along with the quantile regression estimated lines for q=0.1, q=0.5
# and q=0.9.
# f) Test the null hypothesis that the slope coefficients obtained for q=0.1 and q=0.9 are equal.
