#define an unhedged distribution, here it's normal, to demo the point. Remember if using the hyperbolic code below to use the return output, not the risk factors
eq<-data.frame(rtn=rnorm(n = 1000000,mean = 0.07,sd = 0.2)) #normal distribution approximation of rtn distn

PUTstrike<-(-0.2) #this is the strike of the long put in a collar. -0.2 would be equivalent to an 80% strike (or 20% OTM) If the put leg is implimented as a put spread I'd personally ignore the short put leg for this analysis
CALLstrike<-0.1 #this is the strike of the short call. 0.1 is equivalent to a 110% strike (or 10% OTM)

eq$trimmed<-ifelse(eq$rtn<(PUTstrike),PUTstrike,ifelse(eq$rtn>CALLstrike,CALLstrike,eq$rtn))  #this is how the return distribution changes if we use a 10%up/20%down zero cost collar
title1<-paste("this is our starting distribution | mean=median=",round(mean(eq$rtn),digits = 3)) #in the chart display in RStudion you'll need to use the left arrow to see this chart
hist(eq$rtn,main=title1,breaks=100)
abline(v = mean(eq$rtn),col="red")
title2<-paste("with the collar | mean(red)=",round(mean(eq$trimmed),digits = 3),"median(blue)=",round(median(eq$trimmed),digits = 3))
hist(eq$trimmed,main=title2,breaks=100)
abline(v = mean(eq$trimmed),col="blue")
abline(v= mean(eq$rtn),col="red") #this is the old mean, it may be overdrawn by the new mean
#abline(v = median(eq$trimmed),col="blue")


print("################################")
print("Summary stats for equity distn")
print(summary(eq$rtn))
print("################################")


print("################################")
print("Summary stats for equity + zero cost collar")
print(summary(eq$trimmed))
print("################################")

costOFhedge<-mean(eq$trimmed)-mean(eq$rtn)

print("")
print("The hedge impacts the expected return by")
print(paste(round(costOFhedge*100,2),"%"))


#script for producing hyperbolic rtn distn

# install.packages("GeneralizedHyperbolic")
# require(GeneralizedHyperbolic)
# Eqparams<-c(0.089900,0.032569,9.946657,-2.789094)  #from calibration paper mu, delta, alpha, beta
# data <- rhyperb(1000, param = Eqparams)  #actually did 100'000
# hist(data) #does it look right
# quantile(data,.995) #are the risk factor percentiles about right?
# quantile(data,.005)
# write.csv(data,"UKL_Eq_100k.csv") #write to file
#remember these are risk factors, not returns. The returns are given by EXP(r)-1
# rtn_data<-exp(data)-1
#You may want to consider adding a small parallel shift to make the mean (or should it be the median?) equal to the expected return we're after. But even if you don't do


