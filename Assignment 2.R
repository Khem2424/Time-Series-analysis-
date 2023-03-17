
#Question number 1
##clearing environment
rm(list=ls())
library(tseries)
library(urca)
library(forecast)
library(dynlm)
library("quantmod")
# READ IN DATA 
df <- read.csv("historicstats.csv", header = TRUE)
options("jtools-digits"=4) 
options(scipen = 999)
dfts = ts(df,frequency=1,start=c(1892))
plot(dfts[,2:7])
##Transforming all variables in to log
lngdp = log(dfts[,2])
lnwage = log(dfts[,3])
lnunemp=log(dfts[,4])
lntum=log(dfts[,5])
lnemp=log(dfts[,6])
lncpl=log(dfts[,7])

##Checking for order of Integration
par(mar = c(1, 1, 1, 1))
source(file="intord.R")
## For log gdp
intord(lngdp)
##From the above graph, we can see the Standard deviation has not decreased by 
#more than half and also looking at the ADF, we can conclude that log(gdp per capita) 
#has the order of integration of 0. 

#For lod wage
intord(lnwage)
#From the above graph, we can see the Standard deviation has decreased by more 
#than half and also looking at the ADF, we can conclude that log(wage) has the order of integration of 1. 

#For log unemployemt
intord(lnunemp)
#From the above graph, we can see the Standard deviation has decreased by more 
#than half and also looking at the ADF, we can conclude that log(unemployment) has the order of integration of 1. 

##For log(Trade Union Membership)
intord(lntum)
#From the above graph, we can see the Standard deviation has decreased by more
#than half and also looking at the ADF, we can conclude that log(trade union membership) has the order of integration of 2

##For log(employment)
intord(lnemp)
#From the above graph, we can see the Standard deviation has decreased by more 
#than half and also looking at the ADF, we can conclude that log(employment) has the order of integration of 1. 

##For log(capital per labour)
intord(lncpl)
#From the above graph, we can see the Standard deviation has decreased by more
##than half and also looking at the ADF, we can conclude that log(per capita labor) has the order of integration of 1. 


##Checking if variable have a trend stationary or difference stationary 
##For lntum 
df0n=ur.df(lntum,type="none",selectlags="BIC")
summary(df0n)
#From the first test, as value of test-statistics is -1.8003 which is greater than -1.28, we fail
#to reject null hypothesis. Thus the variable has a unit root i.e it is non-stationary
df0d=ur.df(lntum,type="drift",selectlags="BIC")
summary(df0d) 
#After adding drift, we again fail to reject the joint test. Thus, the variable do not a drift as well
df0t=ur.df(lntum,type="trend",selectlags="BIC")
summary(df0t) 
#After adding trend, we again fail to reject the null hypothesis. Thus,we conclude that, the variable
#is non-stationary and does not have a trend. Thus the variable is difference stationary. 

##Now calculating the difference and checking for stationary
#As ln(tum) have order of integration 2 calculating the difference twice 
d.lntum=diff(diff(lntum))
intord(d.lntum)
#comparing the adf test and standard deviation, we can see now the differenced variable is stationary 


##for lngdp
df0n1=ur.df(lngdp,type="none",selectlags="BIC")
summary(df0n1) 
#As the value of t-statistic -1.52 is greater than the -2.58, we fail to reject the null hypothesis
#and conclude that the variable is non-stationary 
df0d1=ur.df(lngdp,type="drift",selectlags="BIC")
summary(df0d1) 
# here we got the contradicting results to our first test, we reject the null hypothesis and conclude that
#the variable is stationary and it has drift 
df0t1=ur.df(lngdp,type="trend",selectlags="BIC")
summary(df0t1) 
#After above last test, we can look in the result and conclude that variable is stationary, it has drift and 
#their is deterministic trend as well


##For lnwage
df0n2=ur.df(lnwage,type="none",selectlags="BIC")
summary(df0n2) 
#As the value of t-statistic 1.8227 is greater than the -2.58, we fail to reject the null hypothesis
#and conclude that the variable is non-stationary 
df0d2=ur.df(lnwage,type="drift",selectlags="BIC")
summary(df0d2)
# now looking at this joint test, the test-statistic of phi1 is 3.0709 is less than 6.52(1%)
#thus we fail to reject the null hypothesis and conclude that variable has no drift
df0t2=ur.df(lnwage,type="trend",selectlags="BIC")
summary(df0t2) 
#After adding trend, we again fail to reject the null hypothesis. Thus,we conclude that, the variable
#is non-stationary, their is no drift and does not have a deterministic trend. Thus the variable is difference stationary. 
intord(diff(lnwage))
#no when we calculate the difference, we can see the the variable is stationary


##For lnunemp
df0n3=ur.df(lnunemp,type="none",selectlags="BIC")
summary(df0n3) 
#As the value of t-statistic -0.5914 is greater than the -2.58, we fail to reject the null hypothesis
#and conclude that the variable is non-stationary 
df0d3=ur.df(lnunemp,type="drift",selectlags="BIC")
summary(df0d3) 
# now looking at this joint test, the test-statistic of phi1 is 3.7089 is less than 6.52(1%)
#thus we fail to reject the null hypothesis and conclude that variable has no drift and is non-stationary
df0t3=ur.df(lnunemp,type="trend",selectlags="BIC")
summary(df0t3) 
#After adding trend, we again fail to reject the null hypothesis. Thus,we conclude that, the variable
#is non-stationary, their is no drift and does not have a deterministic trend. Thus the variable is difference stationary
intord(diff(lnunemp))
#when we calculate the difference of the variable we can see now the variable is stationary


##For lnemp
df0n4=ur.df(lnemp,type="none",selectlags="BIC")
summary(df0n4) 
#As the value of t-statistic 1.9882 is greater than the -2.58, we fail to reject the null hypothesis
#and conclude that the variable is non-stationary 
df0d4=ur.df(lnemp,type="drift",selectlags="BIC")
summary(df0d4) 
# now looking at this joint test, the test-statistic of phi1 is 3.9393 is less than 6.52(1%)
#thus we fail to reject the null hypothesis and conclude that variable has no drift and is non-stationary
df0t4=ur.df(lnemp,type="trend",selectlags="BIC")
summary(df0t4)
#After adding trend, we again fail to reject the null hypothesis. Thus,we conclude that, the variable
#is non-stationary, their is no drift and does not have a deterministic trend. Thus the variable is difference stationary
intord(diff(lnemp))
#when we calculate the difference of the variable we can see now the variable is stationary

##For lncpl
df0n5=ur.df(lncpl,type="none",selectlags="BIC")
summary(df0n5)
#As the value of t-statistic -0.0155 is greater than the -2.58, we fail to reject the null hypothesis
#and conclude that the variable is non-stationary 
df0d5=ur.df(lncpl,type="drift",selectlags="BIC")
summary(df0d5)
# now looking at this joint test, the test-statistic of phi1 is 2.3852 is less than 6.52(1%)
#thus we fail to reject the null hypothesis and conclude that variable has no drift and is non-stationary
df0t5=ur.df(lncpl,type="trend",selectlags="BIC")
summary(df0t5) 
#After adding trend, we again fail to reject the null hypothesis. Thus,we conclude that, the variable
#is non-stationary, their is no drift and does not have a deterministic trend. Thus the variable is difference stationary
intord(diff(lncpl))
#when we calculate the difference of the variable we can see now the variable is stationary



##Question Number 2

##clearing environment
rm(list=ls())
library(tseries)
library(urca)
library(forecast)
library(dynlm)
library("quantmod")
# READ IN DATA 
df <- read.csv("data2.csv", header = TRUE)
options("jtools-digits"=4) 
options(scipen = 999)
dfts = ts(df[,2:4],frequency=4,start=c(1970,1)) 
plot(dfts[,1:3])
#Here I have taken 3 variables: Unemployment rate, CPI and federal debt, quaterly data which are 
#not seasonally adjusted, from 1970-01-01 to 2022-01-01

## Now Checking for order of integration
par(mar = c(1, 1, 1, 1))
source(file="intord.R")
unemp=dfts[,1]
cpi = dfts[,2]
fdebt=dfts[,3]

#Checking if variables are stationary 
#For unemployment 
intord(unemp)
#looking at the adf test and the standard deviation, we can conclude that the variable is stationary
#And it has order of integration of 0

#For cpi
intord(cpi)
#looking at the adf test and the standard deviation, we can conclude that variable is not stationary
# and it has order of integration 2

# For fedral debt
intord(fdebt)
#Looking at the adf test and the standard deviation, we can conclude that variable is non-stationary 
#and it has order on integration 2

##Dynamic Specification 
#Coverting the variables in to stationary
dfdebt=diff(diff(fdebt))
dcpi=diff(diff(cpi))
intord(dfdebt)
intord(dcpi)
d.fdebt = ts(dfdebt,frequency=4,start=c(1970,3))
d.cpi = ts(dcpi,frequency=4,start=c(1970,3))

#Creating Dummy variable
library(forecast)
seas = seasonaldummy(dfdebt)
seas


##Running dynamic regression

r1 <- dynlm(d.fdebt~L(d.fdebt,1:12)+L(unemp,0:12)+L(d.cpi,0:12)+seas, start=c(1973,3))
summary(r1)
AIC(r1)
BIC(r1)

#New model with lags which are significant at 5 % level
r2<-dynlm(d.fdebt~L(d.fdebt,1:12)+L(unemp,0:3)+L(d.cpi,0:6)+L(d.cpi,8)+seas, start=c(1973,3))
summary(r2)
AIC(r2)
BIC(r2)

##Model 3 with all the lags which are significant at 10 % level 
r3<-dynlm(d.fdebt~L(d.fdebt,1:12)+L(unemp,0:3)+L(d.cpi,0:8)+seas, start=c(1973,3))
summary(r3)
AIC(r3)
BIC(r3)

#MOdel 4 #taking lags which are significant at only 1%
r4<-dynlm(d.fdebt~L(d.fdebt,1:11)+L(unemp,0:3)+L(d.cpi,0:2)+seas, start=c(1973,2))
summary(r4)
AIC(r4)
BIC(r4)

#model 5 #taking varibales which are significant at 10%
r5<-dynlm(d.fdebt~L(d.fdebt,1:12)+L(unemp,0:4)+L(unemp,8)+L(unemp,12)+L(d.cpi,0:11)+seas, start=c(1973,3))
summary(r5)
AIC(r5)
BIC(r5)

##Model 6 ##taking significant lags of model 5
r6<-dynlm(d.fdebt~L(d.fdebt,1:12)+L(unemp,0:3)+L(d.cpi,0:8)+seas, start=c(1973,3))
summary(r6)
AIC(r6)
BIC(r6)

##Question C
#Checking the evidence of seasonality
#R2 is our base model and the best model for us as it has the lowest AIC and BIC,
#now looking at the summary of r2, we can see that seasQ1, seasQ2 and intercept (seasQ4)
#are significant at 5% level. 
#Q4 (intercept) has the estimate of 215,693.153, which shows that federal debt is increased
#by 215,693.153 in fourth quater. Similarly, seasQ1 has the estimate of -370,243.332.
#therefore, the effect of Q1infederal debt is 215,693.153-370243.332=154550.15, which
#indicates, federal debt decreases by 154550.15 in quater 1. Again, Q3 has the estimate 
#of -290746.94. Therefore, the effect of Q3 is -290746.94+215693.153=75,052.84 on federal debt. 
#here, Q2 is insignificant.

#seasonal Dummies
#looking at the summary of regression 2, for federal debt seasonal lag4 and lag12 are significant
# for unemployment non lag are significant and for consumer price index only lag 8 is significant. 


# Question D
#Checking serial correlation 
#For regression 2
library(lmtest)
bgtest(r2, order = 1)
bgtest(r2, order = 2)
bgtest(r2, order = 3)
bgtest(r2, order = 4)
bgtest(r2, order = 5)
bgtest(r2, order = 6)

#R1
bgtest(r1, order = 1)
bgtest(r1, order = 2)
bgtest(r1, order = 3)
bgtest(r1, order = 4)
bgtest(r1, order = 5)
bgtest(r1, order = 6)


#For regression 3
library(lmtest)
bgtest(r3, order = 1)
bgtest(r3, order = 2)
bgtest(r3, order = 3)
bgtest(r3, order = 4)
bgtest(r3, order = 5)
bgtest(r3, order = 6)


#For regression 4
library(lmtest)
bgtest(r4, order = 1)
bgtest(r4, order = 2)
bgtest(r4, order = 3)
bgtest(r4, order = 4)
bgtest(r4, order = 5)
bgtest(r4, order = 6)


#For regression 6
library(lmtest)
bgtest(r6, order = 1)
bgtest(r6, order = 2)
bgtest(r6, order = 3)
bgtest(r6, order = 4)
bgtest(r6, order = 5)
bgtest(r6, order = 6)
##Here, R2 is the best model and after checking for serial test, we can see there is
#serial test in the R2.

#Question no E
##Looking for Granger Causality 
# checking for variable d.cpi
reg7<-dynlm(d.fdebt~L(d.fdebt,1:12)+L(unemp,0:3)+seas, start=c(1973,3))
summary(reg7)
AIC(reg7)
BIC(reg7)
# anova test (F test)
anova(reg7,r2,test="F")
##Here looking at the P-value, it is greater than 5%, thus we fail to reject the null
#hypothesis and conclude that d.cpi do not granger causes d.fdebt 


##Checking for unemp 
reg8<-dynlm(d.fdebt~L(d.fdebt,1:12)+L(d.cpi,0:6)+L(d.cpi,8)+seas, start=c(1973,3))
summary(reg8)
AIC(reg8)
BIC(reg8)
# anova test (F test)
anova(reg8,r2,test="F")
##Here, comparing the P-value with the 5% level, we can see that P value is less than 5%
# Thus, we reject the null hypothesis and conclude that umemp granger causes d.fdebt.


##Conclusion
#After comparing AIC BIC of all the model, we can conclude that 
#model 2 is our best model. Looking at the lags of federal debt, on average lags have a negative 
#impact in the federal debt. On an average unemployment have positive impact on federal debt. And 
#CPI and its lag have negative impact federal debt. Also, Looking at the summary I can say that my model
# is statistically significant.