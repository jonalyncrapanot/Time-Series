attach(exod)

summary(Tradedata$`Balance of Trade (BOT-G)`)

data <- ts(Tradedata$`Balance of Trade (BOT-G)`, start = 1991, frequency = 12)
plot.ts(data)

exo <- ts(exod$Average, start = 1991, frequency = 12)

d.train <- subset(data, end=length(data)-(length(data)*0.30))
d.test <- subset(data, start=length(data)-(length(data)*0.30))
length(d.train)
length(d.test)
###Assumptions
##Packages
library(tseries)
library(VGAM)
library(forecast)
library(ggplot2)
library(MASS)
library(funtimes)
library(sarima)
library(caret)
library(astsa)

##Stationary
adf.test(data, alternative="stationary", k=1)

#Differencing with lag 12
newdata <- diff(data, lag = 12)
adf.test(newdata, alternative="stationary", k=12)

plot.ts(newdata, xlab = "Year", ylab = "Balance of Trade")

##Linearity
library(funtimes)
notrend_test(newdata, test = c("t", "MK", "WAVK"))

##Dependence
Box.test(newdata, lag = 12, type = "Ljung-Box")
plot(newdata)

##Split the data
train <- subset(newdata, end=length(newdata)-(length(newdata)*0.30))
test <- subset(newdata, start=length(newdata)-(length(newdata)*0.30))
View(newdata)
View(train)
View(test)
length(newdata)
length(train)
length(test)
###ARMA
##ACF and PACF
ggAcf(train)
pacf(train)

ggAcf(test)
pacf(test)

#Hyperparameter tuning

final.aic <- Inf
final.order <- c(0,0,0)
for (p in 1:12) for (q in 1:12) {
  current.aic <- AIC(arima(train, order=c(p, 0, q)))
  if (current.aic < final.aic) {
    final.aic <- current.aic
    final.order <- c(p, 0, q)
    arma.train <- arima(train, order=final.order)
  }
}
final.order
final.aic
BIC(arma.train)
arma.train

#Forecast
f.arma.train <- forecast(arma.train, h=114) 

f.plot <- f.arma.train %>% autoplot() + autolayer(test)
f.plot
autoplot(train, series="Training data") +
  autolayer(fitted(arma.train, h=12), series="12-step fitted values")

arma.test <- Arima(test, model = arma.train)
accuracy(arma.test)
accuracy(f.arma.train, test)

##Diagnostic Checking
#Residual Independence (Train set)
residual <- residuals(arma.train)
plot(residual)
Box.test(residual, type = "Ljung-Box")

#Residual is independent

shapiro.test(residual)

#Normal

#Residual Independence (Test set)
residual2 <- residuals(arma.test)
plot(residual2)
Box.test(residual2, type = "Ljung-Box")

#Residual is independent

shapiro.test(residual2)

#Not Normal

###ARIMA
#Hyperparameter tuning

final.aic2 <- Inf
final.order2 <- c(0,0,0)
for (p in 1:12) for (d in 1:12) for (q in 1:12) {
  current.aic2 <- AIC(arima(d.train, order=c(p, d, q)))
  if (current.aic2 < final.aic2) {
    final.aic2 <- current.aic2
    final.order2 <- c(p, d, q)
    arima.train <- arima(d.train, order=final.order2)
  }
}
final.order2
final.aic2
BIC(arima.train)
arima.train <- arima(d.train, order=final.order2)

#Forecast
f.arima.train <- forecast(arima.train, h=118) 

f.plot2 <- f.arima.train %>% autoplot() + autolayer(test)
f.plot2

autoplot(d.train, series="Training data") +
  autolayer(fitted(arma.train, h=12), series="12-step fitted values")

arima.test <- Arima(test, model = arima.train)
accuracy(arima.test)
accuracy(f.arima.train, d.test)

##Diagnostic Checking
#Residual Independence (Train set)
residual3 <- residuals(arima.train)
plot(residual3)
Box.test(residual3, type = "Ljung-Box")

#Residual is independent

shapiro.test(residual3)

#Normal

#Residual Independence (Test set)
residual4 <- residuals(arima.test)
plot(residual4)
Box.test(residual4, type = "Ljung-Box")

#Not independent

shapiro.test(residual4)

#Not Normal

###SARIMA
#Hyperparameter tuning
final.aic3 <- Inf
final.order3 <- c(0,0,0)
final.order4 <- c(0,0,0)
for (p in 1:12) for (d in 1:12) for (q in 1:12) for (P in 1:12) for (D in 1:12) for (Q in 1:12)  {
  current.aic3 <- AIC(arima(d.train, order=c(p, d, q), seasonal = list(order = c(P,D,Q), period = 12)))
  if (current.aic3 < final.aic3) {
    final.aic3 <- current.aic3
    final.order3 <- c(p, d, q)
    final.order4 <- c(P, D, Q)
    sarima.train <- arima(d.train, order=final.order3, seasonal = list(order = final.order4, period = 12))
    }
}
final.order3
final.order4
final.aic3
BIC(arima.train)
arima.train <- arima(d.train, order=final.order2)

sarima <- sarima.for(d.train, 12, p, d, q, P, D, Q, S=12)

grid_search(func, params = NULL, n.iter = 1)

#Fit
plot(as.ts(newdata))
lines(fitted(sarima), col="red")

#Plotting the series with fitted values
sarima_fit <- newdata - residuals(sarima)
ts.plot(newdata)
points(sarima_fit, type = "l", col = 2, lty = 2)

#Predict
futurVal2 <- forecast(sarima, h=5, level=c(99)) #confidence level 99%
plot(forecast(futurVal2))
futurVal2

#Check the AIC and BIC
AIC(sarima)
BIC(sarima)

##Diagnostic checking

#Residual independence
residual3 <- residuals(sarima)
plot(residual3)
Box.test(residual3, type = "Ljung-Box")
# Not Independent

#Residual normality
shapiro.test(residual3)
acf(residual3)
pacf(residual3)
#Not normal




