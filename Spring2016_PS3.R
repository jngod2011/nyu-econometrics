#####  Richard Godfrey -- ASE II, Problem Set 3
#####  Due 4 April 2016

################
# Qn 2
################
library(reshape)
library(aod)
library(mfx)

# Initiate data
raw <- read.csv("IMF_2015_data.csv")
raw <- weo[1:552,]
unique(weo$Subject.Descriptor)
raw <- subset(raw, select=c(Country, Subject.Descriptor, Units, X2013))
raw1 <- melt(raw, id=c("Country", "Units", "Subject.Descriptor"))
summary(raw1)
data <- cast(raw1, Country ~ Subject.Descriptor)
data <- rename(data, c("Gross domestic product, constant prices" = "GDP", "Output gap in percent of potential GDP" = "GAP", "Gross national savings" = "GNS", "Six-month London interbank offered rate (LIBOR)" = "LIBOR", "Volume of exports of goods and services" = "EXP", "General government revenue" = "GovR", "General government net lending/borrowing" = "GovL", "General government structural balance" = "GovB"))
rm(raw, raw1)

# Configure European dummy var 
euro <- read.csv("Euro.csv", header = TRUE)
data$euro <- ifelse(data$Country%in%euro$Country,1,0)
View(data)
ls(data)

# 1. Estimate a probit model
data = data/100
data$GovB2 <- ifelse(data$GovB>=0,1,0)
attach(data)
probit1 <- glm(GovB2 ~ EXP + GDP + GovL + GovR + LIBOR + GAP + GNS, family=binomial(link="probit"), x=TRUE)
summary (probit1)

# Insufficient observations in LIBOR and GAP
summary(is.na(LIBOR))
summary(is.na(GAP))

# Exclude LIBOR and GAP
probit2 <- glm(GovB2 ~ EXP + GDP + GovL + GovR + GNS, family=binomial(link="probit"), x=TRUE)
summary(probit2)

# 2. Analyse the model
wald.test (b = coef(probit2), Sigma = vcov(probit2), Terms=1:6)
#Pass the test for overall significance, but only GDP is significant on its own

#Test without exports
wald.test (b = coef(probit2), Sigma = vcov(probit2), L=cbind(1,0,1,1,1,1))
# testing joint signifcance for all the betas : wald test
# null H: everything is zero and so reject
probit3 <- glm (GovB2 ~ GDP + GovL + GovR + GNS, family=binomial(link="probit"), x=TRUE)
summary(probit3)
wald.test (b = coef(probit3), Sigma = vcov(probit3), Terms=1:5)

# 3, 4. Estimate the marginal effect of GDP decrease on Pr(deteriorated structural balance) and
# standard errors
probit3M <- probitmfx (formula = GovB2 ~ GDP + GovL + GovR + GNS, data=data, atmean=FALSE)
probit3M$mfxest
# derivatives evaluated at the average: atmean=FALSE

probit3M2 <- probitmfx (formula = GovB2 ~ GDP + GovL + GovR + GNS, data=data, atmean=TRUE)
probit3M2$mfxest

# 5. Estimate the marginal effect of being a European country on the probability of a deterioration 
# in the structural balance
probitEuroM <- probitmfx(formula = GovB2 ~ GDP + GovL + GovR + GNS + euro, data=data, atmean=FALSE)
probitEuroM$mfxest
detach(data)

#####################
# Qn 5
#####################

library(erer)
library(mfx)

# Initialise
smoke = read.table ("smoke.txt")
names(smoke) [1] <- "faminc"
names(smoke) [2] <- "cigtax"
names(smoke) [3] <- "cigprice"
names(smoke) [4] <- "bwght"
names(smoke) [5] <- "fatheduc"
names(smoke) [6] <- "motheduc"
names(smoke) [7] <- "parity"
names(smoke) [8] <- "male"
names(smoke) [9] <- "white"
names(smoke) [10] <- "cigs"
names(smoke) [11] <- "lbwght"
names(smoke) [12] <- "bwghtlbs"
names(smoke) [13] <- "packs"
names(smoke) [14] <- "lfaminc"
attach(smoke)
summary(smoke)

# Create variables smokes 
smokes <- ifelse (cigs > 0, 1, 0)
smokes <- as.matrix(smokes)
dim(smokes)
hist(smokes)

# 1. Estimate a probit model relating smokes to motheduc, and log(faminc).
#Marginal effect
probit <- probitmfx(formula = smokes ~ lfaminc + motheduc, data=smoke, atmean=TRUE)
(probit)
# evaluated at the average in the sample, what is the estimated difference in the probability 
# of smoking for a woman with 16, 12 years of education.

diff <- probit$mfxest[4,1] - probit$mfxest[8,1]
(diff)

# 2. Is faminc, motheduc exogenous in the smoking equation? 

# The mothers education clearly does not depend on whether the woman smokes, thus exogenous.
# Faminc is not clear as high cigarette use leads to lower disposable income: need to check.

# 3. Test the null hypothesis that log(faminc) is exogenous.
ols1 <- lm(lfaminc ~ motheduc, data=smoke)
resid1 <- ols1$residuals
resid1 <- as.matrix(resid1)
summary(resid1)

# Probit model to test (consider delta_1, alpha_1 and theta_1)
probit2 <- glm(smokes ~ lfaminc + resid1, family=binomial(link="probit"), x=TRUE)
summary(probit2)
# p-value rejects null hypothesis of exogeneity. 
# therefore lfaminc is endogenous
