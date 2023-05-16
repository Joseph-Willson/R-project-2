######################## Task 1 ######################
pack2<-c("lmtest", "fGarch", "vars", "FinTS", "moments", "rugarch", "sandwich", "rmgarch",
         "urca", "xts") 
#install.packages(pack2)
lapply(pack2, require, character.only = TRUE) 
library(urca)
library(car)
library(GGally)
library(tseries)
library(vars)
library(rugarch)
library(rmgarch)
library(ggcorrplot)
library(corpcor)
library(strucchange)
library(ppcor)
library(readxl)
library(ggplot2)
library(tseries)
library (lmtest)
library (rugarch)
library (rmgarch)
library(PerformanceAnalytics)
#load the dataset
pack<-c("readxl")
dataset<- read_excel("/Users/francistran/Downloads/macro.xlsx",sheet = "macro", 
                  col_names = TRUE)

#1.1 
#we print the name of the variable
View(dataset)
colnames(dataset)
#we rename our column to remove the & caracter
names(dataset)[3] = "SP500"

#we compute our log time series
logMicrosoft = log (dataset$MICROSOFT)
logSP500 = log (dataset$SP500)
logCPI = log (dataset$CPI)
logINDPRO = log (dataset$INDPRO)
logM1SUPPLY = log (dataset$M1SUPPLY)
logCCREDIT = log (dataset$CCREDIT)
logBMINUSA = log (dataset$BMINUSA)
logUSTB3M = log (dataset$USTB3M)
logUSTB6M = log (dataset$USTB6M)
logUSTB10Y = log (dataset$USTB10Y)

#we plot our log time series
par(mfrow=c(2,5))
plot(dataset$Date, logMicrosoft, type = "l", lty = 1)
plot(dataset$Date, logSP500, type = "l", lty = 1)
plot(dataset$Date, logCPI, type = "l", lty = 1)
plot(dataset$Date, logINDPRO, type = "l", lty = 1)
plot(dataset$Date, logM1SUPPLY, type = "l", lty = 1)
plot(dataset$Date, logCCREDIT , type = "l", lty = 1)
plot(dataset$Date, logBMINUSA, type = "l", lty = 1)
plot(dataset$Date, logUSTB3M, type = "l", lty = 1)
plot(dataset$Date, logUSTB6M , type = "l", lty = 1)
plot(dataset$Date, logUSTB10Y , type = "l", lty = 1)

#1.2 
#we compute our log returns 
log_returns_SP500 <-diff(logSP500, lag=1)
log_returns_Microsoft <- diff(logMicrosoft, lag=1)

#we plot the log returns
par(mfrow=c(1,1))
plot(dataset$Date[2:385], log_returns_Microsoft, type = "l", col = "black", xlab = "Date", ylab = "Log_Returns")
lines(dataset$Date[2:385], log_returns_SP500, type = "l", col = "red", xlab = "Date", ylab = "Log_Returns")
legend("topright", c("Microsoft", "SP500"), col = c("black", "red"), lty = 1)
title("Microsoft and SP500 Log returns ")


#we test the correlation of our series by plotting them and computing the coefficient of correlation
par(cex.axis = 1.5, cex.lab = 1.5, lwd = 2) 
plot(log_returns_SP500, log_returns_Microsoft, xlab ="SP500",
     ylab ="Microsoft",
     pch = 20, cex = 1, col="red") 

Corr<-cor(log_returns_Microsoft,log_returns_SP500) 
Corr

#1.3 
#We compute the box plot of the 2 log returns series
par(mfrow=c(1,2))
boxplot(log_returns_Microsoft)
boxplot(log_returns_SP500)

#1.4
#Repeat Task 1.2 and 1.3 for 3 months and 10 months

#we compute the log returns
log_returns_CPI <-diff(logCPI, lag=1)
log_returns_INDPRO <- diff(logINDPRO, lag=1)
log_returns_M1SUPPLY <-diff(logM1SUPPLY, lag=1)
log_returns_CCREDIT <- diff(logCCREDIT, lag=1)
log_returns_BMINUSA <-diff(logBMINUSA, lag=1)
log_returns_USTB3M <- diff(logUSTB3M, lag=1)
log_returns_USTB6M <-diff(logUSTB6M, lag=1)
log_returns_USTB10Y <-diff(logUSTB10Y, lag=1)



par(mfrow=c(1,1))
plot(dataset$Date[2:385], log_returns_USTB3M, type = "l", col = "black", xlab = "Date", ylab = "Log_Returns")
lines(dataset$Date[2:385], log_returns_USTB10Y, type = "l", col = "red", xlab = "Date", ylab = "Log_Returns")
legend("topright", c("USTB3M", "USTB10Y"), col = c("black", "red"), lty = 1)
title("USTB3M and USTB10Y log returns ")


#test the correlation
par(cex.axis = 1.5, cex.lab = 1.5, lwd = 2) 
plot(log_returns_USTB3M, log_returns_USTB10Y, xlab ="SP500",
     ylab ="Microsoft",
     pch = 20, cex = 1, col="red") # Here we create a scatter plot of the two series 

corre <- cor(log_returns_USTB3M, log_returns_USTB10Y)

#we compute the boxplot
par(mfrow=c(1,2))
boxplot(log_returns_USTB3M)
boxplot(log_returns_USTB10Y)

#1.5
#Microsoft
mean(log_returns_Microsoft)
sd(log_returns_Microsoft)
kurtosis(log_returns_Microsoft)
skewness(log_returns_Microsoft)
median(log_returns_Microsoft)
quantile(log_returns_Microsoft,0.25)
quantile(log_returns_Microsoft,0.75)
min(log_returns_Microsoft)
max(log_returns_Microsoft)

#SP500
mean(log_returns_SP500)
sd(log_returns_SP500)
kurtosis(log_returns_SP500)
skewness(log_returns_SP500)
median(log_returns_SP500)
quantile(log_returns_SP500,0.25)
quantile(log_returns_SP500,0.75)
min(log_returns_SP500)
max(log_returns_SP500)

#USTB3M
mean(log_returns_USTB3M)
sd(log_returns_USTB3M)
kurtosis(log_returns_USTB3M)
skewness(log_returns_USTB3M)
median(log_returns_USTB3M)
quantile(log_returns_USTB3M,0.25)
quantile(log_returns_USTB3M,0.75)
min(log_returns_USTB3M)
max(log_returns_USTB3M)

#log_returns_USTB10Y
mean(log_returns_USTB10Y)
sd(log_returns_USTB10Y)
kurtosis(log_returns_USTB10Y)
skewness(log_returns_USTB10Y)
median(log_returns_USTB10Y)
quantile(log_returns_USTB10Y,0.25)
quantile(log_returns_USTB10Y,0.75)
min(log_returns_USTB10Y)
max(log_returns_USTB10Y)

######################## Task 2 ######################
#2.1) ACF & PACF Plots for log series & returns series
#ACF of log series 

par(mfrow = c(2, 5))
acf(logMicrosoft, main = 'ACF MICROSOFT ')
acf(logSP500, main = 'ACF SP500 ')
acf(logCPI, main = 'ACF CPI ')
acf(logINDPRO, main = 'ACF INDPRO ')
acf(logM1SUPPLY, main = 'ACF M1SUPPLY ')
acf(logCCREDIT, main = 'ACF CCREDIT ')
acf(logBMINUSA, main = 'ACF BMINUSA ')
acf(logUSTB3M, main = 'ACF USTB3M ')
acf(logUSTB6M, main = 'ACF USTB6M ')
acf(logUSTB10Y, main = 'ACF USTB10Y ')

#PACF of the log series
par(mfrow = c(2, 5))
pacf(logMicrosoft, main = 'PACF MICROSOFT ')
pacf(logSP500, main = 'PACF SP500 ')
pacf(logCPI, main = 'PACF CPI ')
pacf(logM1SUPPLY, main = 'PACF M1SUPPLY ')
pacf(logCCREDIT, main = 'PACF CCREDIT ')
pacf(logBMINUSA, main = 'PACF BMINUSA ')
pacf(logUSTB3M, main = 'PACF USTB3M ')
pacf(logUSTB6M, main = 'PACF USTB6M ')
pacf(logUSTB10Y, main = 'PACF USTB10Y ')

#ACF of the log returns
par(mfrow = c(2, 5))
acf(log_returns_Microsoft_1, main = 'ACF MICROSOFT ')
acf(log_returns_SP500_1, main = 'ACF SP500 ')
acf(log_returns_CPI, main = 'ACF CPI ')
acf(log_returns_INDPRO, main = 'ACF INDPRO ')
acf(log_returns_M1SUPPLY, main = 'ACF M1SUPPLY ')
acf(log_returns_CCREDIT, main = 'ACF CCREDIT ')
acf(log_returns_BMINUSA, main = 'ACF BMINUSA ')
acf(log_returns_USTB3M, main = 'ACF USTB3M ')
acf(log_returns_USTB6M, main = 'ACF USTB6M ')
acf(log_returns_USTB10Y, main = 'ACF USTB10Y ')

#PACF of the log returns
par(mfrow = c(2, 5))
pacf(log_returns_Microsoft_1, main = 'PACF MICROSOFT ')
pacf(log_returns_SP500_1, main = 'PACF SP500 ')
pacf(log_returns_CPI, main = 'PACF CPI ')
pacf(log_returns_INDPRO, main = 'PACF INDPRO ')
pacf(log_returns_M1SUPPLY, main = 'PACF M1SUPPLY ')
pacf(log_returns_CCREDIT, main = 'PACF CCREDIT ')
pacf(log_returns_BMINUSA, main = 'PACF BMINUSA ')
pacf(log_returns_USTB3M, main = 'PACF USTB3M ')
pacf(log_returns_USTB6M, main = 'PACF USTB6M ')
pacf(log_returns_USTB10Y, main = 'PACF USTB10Y ')

#2.2
#Stationarity
#log
adf.test(logMicrosoft)
adf.test(logSP500)
adf.test(logCPI)
adf.test(logINDPRO)
adf.test(logM1SUPPLY)
adf.test(logCCREDIT)
adf.test(logBMINUSA)
adf.test(logUSTB3M)
adf.test(logUSTB6M)
adf.test(logUSTB10Y)

# log returns
adf.test(log_returns_Microsoft_1)
adf.test(log_returns_SP500_1)
adf.test(log_returns_CPI)
adf.test(log_returns_INDPRO)
adf.test(log_returns_M1SUPPLY)
adf.test(log_returns_CCREDIT)
adf.test(log_returns_BMINUSA)
adf.test(log_returns_USTB3M)
adf.test(log_returns_USTB6M)
adf.test(log_returns_USTB10Y)

#serially correlated
#log
Box.test(logMicrosoft, type = "Ljung-Box")
Box.test(logSP500, type = "Ljung-Box")
Box.test(logCPI, type = "Ljung-Box")
Box.test(logINDPRO, type = "Ljung-Box")
Box.test(logM1SUPPLY, type = "Ljung-Box")
Box.test(logCCREDIT, type = "Ljung-Box")
Box.test(logBMINUSA, type = "Ljung-Box")
Box.test(logUSTB3M, type = "Ljung-Box")
Box.test(logUSTB6M, type = "Ljung-Box")
Box.test(logUSTB10Y, type = "Ljung-Box")

#log returns 
Box.test(log_returns_Microsoft, type = "Ljung-Box")
Box.test(log_returns_SP500, type = "Ljung-Box")
Box.test(log_returns_CPI, type = "Ljung-Box")
Box.test(log_returns_INDPRO, type = "Ljung-Box")
Box.test(log_returns_M1SUPPLY, type = "Ljung-Box")
Box.test(log_returns_CCREDIT, type = "Ljung-Box")
Box.test(log_returns_BMINUSA, type = "Ljung-Box")
Box.test(log_returns_USTB3M, type = "Ljung-Box")
Box.test(log_returns_USTB6M, type = "Ljung-Box")
Box.test(log_returns_USTB10Y, type = "Ljung-Box")

#homoskedasticity
#log
ArchTest(logMicrosoft)
ArchTest(logSP500)
ArchTest(logCPI)
ArchTest(logINDPRO)
ArchTest(logM1SUPPLY)
ArchTest(logCCREDIT)
ArchTest(logBMINUSA)
ArchTest(logUSTB3M)
ArchTest(logUSTB6M)
ArchTest(logUSTB10Y)

#log returns
ArchTest(log_returns_Microsoft)
ArchTest(log_returns_SP500)
ArchTest(log_returns_CPI)
ArchTest(log_returns_INDPRO)
ArchTest(log_returns_M1SUPPLY)
ArchTest(log_returns_CCREDIT)
ArchTest(log_returns_BMINUSA)
ArchTest(log_returns_USTB3M)
ArchTest(log_returns_USTB6M)
ArchTest(log_returns_USTB10Y)

#normally distributed
#log
shapiro.test(logMicrosoft)
shapiro.test(logSP500)
shapiro.test(logCPI)
shapiro.test(logINDPRO)
shapiro.test(logM1SUPPLY)
shapiro.test(logCCREDIT)
shapiro.test(logBMINUSA)
shapiro.test(logUSTB3M)
shapiro.test(logUSTB6M)
shapiro.test(logUSTB10Y)

#log returns
shapiro.test(log_returns_Microsoft)
shapiro.test(log_returns_SP500)
shapiro.test(log_returns_CPI)
shapiro.test(log_returns_INDPRO)
shapiro.test(log_returns_M1SUPPLY)
shapiro.test(log_returns_CCREDIT)
shapiro.test(log_returns_BMINUSA)
shapiro.test(log_returns_USTB3M)
shapiro.test(log_returns_USTB6M)
shapiro.test(log_returns_USTB10Y)

#task 2.3
#First bullet point : test of multicollinearity
fit1<- lm(log_returns_Microsoft~log_returns_SP500 + log_returns_CPI+log_returns_INDPRO+log_returns_M1SUPPLY+
            log_returns_CCREDIT+log_returns_BMINUSA+log_returns_USTB3M+log_returns_USTB6M+log_returns_USTB10Y)
fit1
par(mfrow=c(2,2))
plot(fit1)
vif (fit1)

#Second bullet point 
#Series of residuals obtained from the OLS estimation
ErrorTerms<-fit1$residuals 

# 1) E(Res)=0
par(mfrow=c(1,2)) 
plot(ErrorTerms)
hist(ErrorTerms) 
mean(ErrorTerms)
sd(ErrorTerms)

#2) Var(Res)=sigma^2 (constant & finite)
#White's test
bptest(fit1, data=Returns) 
#ArchTest
ArchTest(ErrorTerms, lags=60, demean = TRUE) 
coeftest(fit1, vcov = vcovHC(fit1))

#3) Residuals no autocorrelated
plot(ErrorTerms)
bgtest(fit1, order=5) # Breusch-Godfrey Test
Box.test(ErrorTerms,lag =5) # Ljung-Box test

#4) X(t) and e(t) non-correlated
Check <- lm(log_returns_SP500~ErrorTerms) 
summary(Check)
cor(ErrorTerms,log_returns_SP500)

# 5) Residuals are normally distributed
jarque.test(ErrorTerms) # Bera-Jarque test

# Stability Tests 
# Chow Test: We test the null hypothesis of NO STRUCTURAL BREAKS starting from obs. 1500
# KNOWN STRUCTURAL BREAK
sctest(fit1, type = "Chow", point = 150) 

# CUSUM test: We test the null hypothesis of NO STRUCTURAL BREAKS
# UNKNOWN STRUCTURAL BREAK
par(mfrow=c(1,1))
SB <- efp(log_returns_Microsoft~log_returns_SP500 + log_returns_CPI+log_returns_INDPRO+log_returns_M1SUPPLY+
            log_returns_CCREDIT+log_returns_BMINUSA+log_returns_USTB3M+log_returns_USTB6M+log_returns_USTB10Y, data=dataset, type =  "OLS-CUSUM") # CUSUM test
plot(SB)


#2.4
# EGARCH model for GM series & MSFT series
egarch.spec = ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)), 
                         mean.model=list(armaOrder=c(0,0)))
Microsoft_egarch.fit = ugarchfit(egarch.spec,log_returns_Microsoft_1) 
coef(Microsoft_egarch.fit) 
Microsoft_egarch.fit

#2.5) GARCH-DCC
# GARCH(1,1) model for each of both series

garch1.spec = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                         variance.model = list(garchOrder = c(1,1),
                                               model = "sGARCH"),
                         distribution.model = "norm" )


dcc.garch11.spec = dccspec(uspec = multispec( replicate(2,garch1.spec) ),dccOrder = c(1,1),distribution = "mvnorm")
Microsoft_egarch.fit = dccfit(dcc.garch11.spec,data=data.frame(log_returns_Microsoft_1,log_returns_SP500_1))

#we plot the conditional correlation
data <- data.frame(x = log_returns_Microsoft, y = log_returns_SP500)
corr_matrix <- cor(data)
ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower",
           lab = TRUE, lab_size = 3, method = "circle")

######################## Task 3 ######################
#3.1 
#we plot our times series
Yield_3M= dataset$USTB3M
Yield_6M= dataset$USTB6M
Yield_10Y = dataset$USTB10Y

par(mfrow=c(1,1))
plot(dataset$Date, logUSTB3M, type = "l", col="blue", xlab="date", ylab="Log_series")
lines(dataset$Date, logUSTB6M, type = "l", col="red", xlab="date", ylab="Log_series")
legend("topright", c("L_USTB3M","L_USTB6M"), col= c("blue","red"), lty = 1)
title("Difference USTB3M, USTB6M")

#3.2
#we regress the six-month yields on the three-month yields
slope <- cov(logUSTB6M, logUSTB3M)/ var(logUSTB6M)
intercept <- mean(logUSTB3M) - (slope * mean(logUSTB6M))
slope
intercept

L_USTB3M_p <- intercept + slope*logUSTB6M
Res <- logUSTB3M - L_USTB3M_p
plot(Res)

#we check the correlation
Corr<-cor(logUSTB6M, logUSTB3M)
Corr

# OLS TEST : we compute alpha and beta to estimate our error term 
OLS<-lm(logUSTB3M~logUSTB6M)
summary(OLS)

#we want to minimize the residuals
matrix_coef <- summary(OLS)$coefficients
alpha_hat <- matrix_coef[1,1]
beta_hat <- matrix_coef[2,1]
L_USTB3M_hat <- fitted(OLS)
Res_OLS <- logUSTB3M - L_USTB3M_hat
summary(OLS, confint = TRUE, ci.width = .95)

# Unit Root test
unitroot <- ur.df(Res, type = "trend", selectlags = c("BIC"))
summary(unitroot)

#3.3
#we compute our ECM
Coint_Eq = lm(logUSTB6M~logUSTB3M)
summary(Coint_Eq)
Resids <-  Coint_Eq$residuals
DL_L_USTB6M=diff(logUSTB6M)
DL_L_USTB3M=diff(logUSTB3M)
ResidsAdj <-Res[1:length(DL_L_USTB3M)]
ECM <- lm(DL_L_USTB6M~DL_L_USTB3M+ ResidsAdj)
summary(ECM)


#3.4) Regress the three-month yields on the 6-month yields
Coint_Eq = lm(logUSTB6M~logUSTB3M)
summary(Coint_Eq)
Resids <-  Coint_Eq$residuals
DL_L_USTB6M=diff(logUSTB6M)
DL_L_USTB3M=diff(logUSTB3M)
ResidsAdj <-Res[1:length(DL_L_USTB6M)]
ECM <- lm(DL_L_USTB3M~DL_L_USTB6M+ ResidsAdj)
summary(ECM)


######################## Task 4 ######################
#4.1
#we compute our impulse responses and FEVD
VARData <- data.frame(log_returns_USTB3M, log_returns_USTB6M,log_returns_USTB10Y)
VARselect(VARData, lag.max=5) 
VAR_Model <- VAR(VARData, p = 1, type = "const")
summary(VAR_Model)

#4.2

ir1 <-irf(VAR_Model, impulse = "log_returns_USTB3M", response = c("log_returns_USTB6M", "log_returns_USTB10Y"), n.ahead = 12, ortho = TRUE)
vd1 <-fevd(VAR_Model, n.ahead = 12)
plot(ir1)
plot(vd1)

#4.3
#we change the variable in impulse and in response to see if the variable ordering has an impact on our results
ir2 <-irf(VAR_Model, impulse = "log_returns_USTB6M", response = c("log_returns_USTB3M", "log_returns_USTB10Y"), n.ahead = 12, ortho = TRUE)
vd2 <-fevd(VAR_Model, n.ahead = 12)
plot(ir2)
plot(vd2)

ir3 <-irf(VAR_Model, impulse = "log_returns_USTB10Y", response = c("log_returns_USTB3M", "log_returns_USTB6M"), n.ahead = 12, ortho = TRUE)
vd3 <-fevd(VAR_Model, n.ahead = 12)
plot(ir3)
plot(vd3)

#4.4
#we compute our Johansen test
jotest=ca.jo(data.frame(Yield_3M,Yield_6M,Yield_10M), type="trace", K=3, ecdet="none", spec="longrun")
summary(jotest)

jotest2=ca.jo(data.frame(Yield_3M,Yield_6M,Yield_10M), type="eigen", K=3, ecdet="none", spec="longrun")
summary(jotest2)
