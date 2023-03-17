##Question no.2
library("quantmod")
library(tseries)
library(urca)
data= read.csv("q2data.csv")
options("jtools-digits"=4) 
options(scipen = 999)
#question a 
#For inflation in china
summary(data$chinainfl)
sd(data$chinainfl)
length(data$chinainfl)
#For interest rate Uk government bonds
summary(data$ukint)
sd(data$ukint)
length(data$ukint)
# For umemoloyemnt in United states
summary(data$usunemp)
sd(data$usunemp)
length(data$usunemp)
#question b
reg1=lm(ukint~usunemp, data=data)
reg2=lm(ukint~chinainfl, data=data)
reg3=lm(usunemp~chinainfl, data=data)
reg4=lm(ukint~usunemp+chinainfl, data=data)
summary(reg1)
summary(reg2)
summary(reg3)
summary(reg4)
##Question c
#Checking for stationary 
par(mar = c(1, 1, 1, 1))
source(file="intord.R")
intord(data$ukint[!is.na(data$ukint)])
intord(data$chinainfl[!is.na(data$chinainfl)])
intord(data$usunemp[!is.na(data$usunemp)])
##Coverting the variable in to stationary 
#For uk governement bonds interest 
dukint=diff(data$ukint)
intord(dukint[!is.na(dukint)])
#For china inflation
dchinainfl=diff(data$chinainfl)
intord(dchinainfl[!is.na(dchinainfl)])
#for Unemployment rate
dusunemp=diff(data$usunemp)
intord(dusunemp[!is.na(dusunemp)])

##Re-estimating the regression
reg5=lm(dukint~dusunemp)
reg6=lm(dukint~dchinainfl)
reg7=lm(dusunemp~dchinainfl)
reg8=lm(dukint~dusunemp+dchinainfl)
summary(reg5)
summary(reg6)
summary(reg7)
summary(reg8)
