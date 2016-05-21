### Adv Econometrics - PS3 - Due Wed 18 Nov 2015 - Richard Godfrey 

### Using the NIST data set for Lowess discussed in the lecture notes, try to
### resolve the difference between the NIST results and R by adjusting the
### default values that R uses. The spreadsheet example may be of some use.

?lowess # predecessor (with different defaults)
?loess # new ('cell' and 'surface' additions for large datasets)

NIST<-read.delim("NIST-lowess.txt")
NISTa <-read.delim("NIST_actual.txt")
plot(NIST$x,NIST$y)
lowess <- lowess(NIST$x,NIST$y, f=0.33); NIST$Lw_x<-lowess$x; NIST$Lw_y <- lowess$y
# lines(x=NIST$LW_x, y=NIST$LW_y, col='blue') why not work??
lines(lowess(NIST$x,NIST$y, f=0.33), col='blue', lwd=0.5)
# NIST model
NIST$Act_y <- NISTa$Y.fit; lines(NIST$Act_y, col='blue')
Loe_fit1 <- loess(y~x, data=NIST[,1:2], span=1/3, degree=2) # perfect with 1/3, nearly with 0.33
NIST$Loe1_y<-predict(Loe_fit1) 
Loe_fit2 <- loess(y~x, data=NIST[,1:2], span=1/3, degree=1) # poor
NIST$Loe2_y<-predict(Loe_fit2) 
Loe_fit3 <- loess(y~x, data=NIST[,1:2], span=1/3, degree=2, family="symmetric", surface="direct")
NIST$Leo3_y<-predict(Loe_fit3)  # first and last small displacement
Loe_fit4 <- loess(y~x, data=NIST[,1:2], span=0.5, degree=2)
NIST$Leo4_y<-predict(Loe_fit4)  # smoother
Loe_fit5 <- loess(y~x, data=NIST[,1:2], span=1, degree=2)
NIST$Leo5_y<-predict(Loe_fit5)  # smoother

lines(predict(Loe_fit1), col='red', lwd=0.5)
lines(predict(Loe_fit5), col='black', lwd=1)
#Le_fit4 <- loess(y~x, data=NIST[,1:2], span=0.4, degree=1) 
#lines(Le_fit4, col='green', lwd=1)


### Addendum from Loess author
# https://stat.ethz.ch/pipermail/bioconductor/2003-September/002337.html
#
# When there is only one predictor variable and no prior weights, 'lowess' 
# and 'loess' are in principle exactly equivalent. However the default 
# settings of the two programs are very different. Here is an example in 
# which I force 'lowess' and 'loess' to do precisely the same  numerical 
# calculation:
   
y <- rnorm(1000)
x <- 1:1000
out.lowess <- lowess(x,y,f=0.3,iter=3,delta=0)
out.lowess$y[1:5]
# [1] 0.1816632 0.1799619 0.1782683 0.1765826 0.1749048
out.loess <- loess(y~x,span=0.3,degree=1,family="symmetric",iterations=4,surface="direct")
fitted(out.loess)[1:5]
# [1] 0.1816632 0.1799619 0.1782683 0.1765826 0.1749048

# Things to note here:
# 1. 'f' is the 'span' argument for 'lowess'
# 2. 'loess' does quadratic (degree=2) local regression by default instead of 
# linear (degree=1)
# 3. Unless you specify family="symmetric", loess will fit the curve by least 
# squares, i.e., won't do any robustness iterations at all.
# 4. lowess and loess count iterations in differently: 'iter' in lowess means 
# the number of robustness iterations; 'iterations' in loess means the total 
# number of iterations including the least squares fit, i.e., iterations=iter+1
# 
# The only aspect in which it is not possible to make 'loess' and 'lowess' 
# agree exactly is in their treatment of large data sets. When x and y are 
# very long, say 10s of thousands of observations, it is impractical and 
# unnecessary to do the local regression calculation exactly, rather it is 
# usual to interpolate between observations which are very close together. 
# This interpolation is control by the 'delta' argument to 'lowess' and the 
# 'cell' and 'surface' arguments to 'loess'.
# 
# When there are a large number of observations, 'lowess' groups together 
# those x-values which are closer than a certain distance apart. Although 
# grouping observations based on distance is in-principle the best approach, 
# this is impractical for 'loess' because 'loess' is designed to accept many 
# x-variables. So 'loess' instead groups observations together based on the 
# number of observations on a cell rather than distances. Because of this 
# small difference, 'lowess' and 'loess' will almost always give slightly 
# different numerical results for large data sets. The difference is not 
# generally important.
