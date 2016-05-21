### PS2 - ASE - Richard Godfrey     Due 03 March

crime_edit <- read.delim("crime_edit.csv", header=T, sep=",")

# ### Use the data in crime.txt for the years 1972 and 1978 for a two-year panel
# data analysis. The model is a simple distributed lag model: log(crimerateit) =
# θ0 + θ1d78t + β1clrprci,t−1 + β2clrprci,t−2 + ci + uit The variable clrprc is
# the clear-up percentage (the percentage of crimes solved). The data are stored
# for two years, with the needed lags given as variables for each year. 1. First
# estimate this equation using a pooled OLS analysis. Comment on the de- terrent
# effect of the clear-up percentage, including interpreting the size of the
# coefficients. Test for serial correlation in the composite error vit assuming
# strict exogeneity. 2. Estimate the equation by fixed effects, and compare the
# estimates with the pooled OLS estimates. Is there any reason to test for
# serial correlation? Optional: obtain heteroscedasticity - robust standard
# errors for the FE estimates. 3. Estimate the model using random effects.
# Indicate how to test for random effects and show the result of your test. 4.
# Using FE analysis, test the hypothesis H0 : β1 = β2. What do you conclude? If
# the hypothesis is not rejected, what would be a more parsimonious model?
# Estimate this model. 

library(plm)
class(crime_edit)
model <- log(crime) ~ d78 + clrprc1 + clrprc2

## Pooled Effects
reg1.pool <- plm(model, data=crime_edit, model="pooling", index=c("district","year"))
summary(reg1.pool)
require(car)

# Comment on the deterrent effect of the clear-up percentage, including interpreting the size of the
# coefficients: 
# -> Crimes solved par the growth rate of crime by 2% per year each.
# -> DW test for SC

require(lmtest)
dwtest(model, data=crime_edit)  # DW test on (u_t, u_t-1)
linearHypothesis(reg1.pool, c("clrprc1","clrprc2"), test="F")
# tests for the joint signifcance of lags 1 and 2.

## Fixed Effects
reg1.fe <- plm(model, data=crime_edit, model="within", index= c("district","year"))
summary(reg1.fe)

# test for individ effects
pFtest(reg1.fe,reg1.pool)
# test for indiv FE present in the Pool
plmtest(reg1.pool, effect = "individual")
# no need to run a SC test as the model is correctly specificed

## Random 
reg1.rand <- plm(model, data=crime_edit, model="random", index= c("district","year"))
summary(reg1.rand)

### B-P LM test to determine RE or Pooled OLS.
# Ho null: variance of unobserved heterogeneity is zero.
# H1 alt: variance_alpha is not zero
# Acceptance of null => more efficient estimates via OLS

plmtest(reg1.pool, type="bp")

### Hausman test to determine FE or RE
# H0: corr[X_it, a_i] = 0
# H1: corr[X_it, a_i] != 0

phtest(reg1.fe,reg1.rand)

### Wald test on hypothesis H0: β1 = β2
# 
model2 <- log(crime) ~ d78 + clrprc1 + clrprc2
reg2.fe <- plm(model2, data=crime_edit, model="within", index= c("district","year"))
#waldtest(reg1.fe, reg2.fe)
#anova(reg1.fe, reg2.fe)


### Qn 3

# ML 

# Posterior density 

# Compute posterior mean in 3D plot 

#install.packages("tcltk")
#install.packages("TeachingDemos")
#library(tcltk)
#library(TeachingDemos)
## E(theta)=x/(x+y)
# x<- y <- seq(1, 10, len=100)
#z <- outer(x,y, FUN=function(x,y) x/(x+y))
#filled.contour(x,y,z, main="3D plot")
#filled.contour(x,y,z, color.palette = heat.colors)
#filled.contour(x,y,z, color.palette = colorRampPalette(c("red","white","blue")) )
#persp(x,y,z, shade=0.75, col="yello")
#rotate.persp(x,y,z)
#view <- persp(x,y,z, shade=0.75, col="red")


