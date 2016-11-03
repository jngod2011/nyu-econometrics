
#### Breakeven Inflation 10s30s U.S. model

linear<- model ~ st_infl_exp + CPI_curve_2s10s + infl_risk_prem + liq_premium + mkt_stress + FRB_op

# 1. Constant
# 2. 1y CPI swap rate, ST inflation expectation
# 3. 2s10s CPI swap curve
# 4. 15F5 - 10F5 CPI swap rate, inflation risk premium
# 5. 10s30s OTR Asw spread box, relative liquidity of 10y,30y sector
# 6. VIX, market stress
# 7. Dummy variable, Fed Operation Twist [21 Sept 11 to 31 Dec 2012]

# Comments. Daily Mar2010 to Jul2016. Variables in bps except for VIX.
# Variable  Beta  t-stat  R2
# Const      15.7   4.4  0.84
# 1y CPI    -0.03  -2.2
# 2s10s CPI  0.06   2
# Infl RP    0.18   2
# 10s30sASWspd-0.73-9
# VIX        0.14   2
# Dummy OT  -10.17 -8

# Comment on Breakeven rate. Real yields:= nominals less breakevens. 
# UST 30y real (TIPS) 0.71  nominal 2.47 breakeven 1.76calc
# IRS 30y real  0.90graph  nominal 1.89
# UST 10y real (TIPS) 0.11 nominal 1.74 breakeven 1.63calc
# IRS 10y  real [x] nominal 1.55 breakeven[x]
# UST 2y real (TIPS) [x] nominal 0.84 breakeven [x]
# IRS 2y real rate [x] nominal 1.05 breakeven [x]

file <- "dataBE1030s.csv"
data <- read.csv(file=file, header=T)
regress <- lm(formula = linear, data = data)



