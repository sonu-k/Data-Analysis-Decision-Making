library(ggplot2)
library(forecast)
library(plotly)
library(ggfortify)
library(tseries)
library(gridExtra)
library(docstring)
library(readr)
library(here)
library(data.table)
library(scales)
library(fpp)
library(fpp2)

data_master = copy(dadm_data)
data_master

attach(dadm_data) #database attached to R search path
data_master

sp_500 = ts(data_master$Open, start=c(1995, 1),end = c(2017,3), freq=12) #convert to time series
sp_500
plot(sp_500) #plotting the data

#Stationarity test
Box.test(sp_500, lag = 20, type = 'Ljung-Box') #we use this to test stationarity. H0-no lack of fit. H1-lack of fit.Accept alternate hypothesis
adf.test(sp_500) #accept null hypothesis i.e the data is not stationary.

sp_500_training = ts(data_master$Open, start=c(1995, 1),end = c(2014,12), freq=12) #training data

#stationarity test on training data
Box.test(sp_500_training, lag = 20, type = 'Ljung-Box')
adf.test(sp_500_training)  #training data is not stationary      

#plot_decomp(sp_500_training)

decomp = decompose(sp_500_training) #estimating the three components of the time series
plot(decomp) #seasonality, trend, errors

seasonalAdjusted = sp_500_training - decomp$seasonal #removing seasonal part of the data
#seasonalAdjusted=seasadj(sp_500_training) - alternate method
plot(seasonalAdjusted)

acf = acf(sp_500_training) #acf is exponentially decaying
#plot(acf)
pacf(sp_500_training) #partial acf. Correlation of Residuals with next lag values 
#acf and pacf are used to determine appropriate values of p and q


tsdiff = diff(sp_500_training) #differencing the data to make it stationary
plot(tsdiff)

Box.test(tsdiff,lag=20,type = 'Ljung-Box') #Accept null hypothesis
adf.test(tsdiff)#rejected null hypothesis => data is now stationary. No more differencing required

acf(tsdiff) 
pacf(tsdiff)

#alternatively using ndiffs function to determine the number of times the data needs to be differenced
ndiffs(sp_500_training) #data needs to be differenced one time

fit <- Arima(sp_500_training, order = c(0,1,1),include.drift = TRUE) #fitting arima model
summary(fit)

# RESIDUAL DIAGNOSTICS
ggtsdiag(fit) +
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.minor = element_blank(),
        axis.line.y = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray"))

residFit <- ggplot(data=fit, aes(residuals(fit))) +
  geom_histogram(aes(y =..density..),  
                 binwidth = 5,
                 col="turquoise4", fill="white") +
  geom_density(col=1) +
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.minor = element_blank(),
        axis.line   = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) +
  ggtitle("Plot of SP 500 ARIMA Model Residuals")

#Forecasting with arima model and displaying accuracy
sp_500_all  = forecast(fit,h =27)
sp_500_test = window(sp_500,2015,c(2017,03))
plot(sp_500_all) #plotting forecasts
round(accuracy(f = sp_500_all, x = sp_500_test, test = NULL,d =NULL,D = NULL), 3)

#Forecasting with naive model and displaying accuracy
aa = snaive(sp_500_training,h=27)
aa1 = forecast(aa,h=27)
plot(aa1) #forecast plot
round(accuracy(f = aa1, x = sp_500_test, test = NULL,d =NULL,D = NULL), 3)

bb = meanf(sp_500_training,h=27)
bb1 = forecast(bb,h=27)
plot(bb1) #forecast plot
round(accuracy(f = bb1, x = sp_500_test, test = NULL,d =NULL,D = NULL), 3)

cc = naive(sp_500_training,h=27)
cc1 = forecast(cc,h=27)
plot(cc1) #forecast plot
round(accuracy(f = cc1, x = sp_500_test, test = NULL,d =NULL,D = NULL), 3)

#Arima gives the best accuracy with minimal errors
#Arima performs better with higher horizon values (h values)




