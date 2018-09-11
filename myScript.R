#cat('\014')
rm(list=ls())

# LOAD YOUR PACKAGES
library(ggplot2)
library(forecast)
library(plotly)
library(ggfortify)
library(tseries)
library(gridExtra)
library(docstring)
library(readr)
library(prophet)
library(here)
source('helper_functions.R')

#Loading in the data
myData <- read.csv("sp500.csv")
colnames(myData)[1]<- "year"
View(myData)


#Create time series object
sp_500 <- ts(myData$sp_500, start=c(1995, 1), freq=12)
View(sp_500)
plot_time_series(sp_500, "sp 500")

#Training set 
spTraining <- ts(sp_500, start=c(1995, 1), end=c(2014, 12), freq=12)
plot_time_series(spTraining, "sp 500 training")
#Test for stationarity
adf.test(spTraining)
Box.test(spTraining, lag = 20, type = 'Ljung-Box')

#Decompose 
plot_decomp(spTraining, 'sp 500')

#Diagnosing ACF and PACF
plot_acf_pacf(spTraining, "sp 500 training")

#finding difference across its consecutive values. 
tsDiff <- diff(spTraining)
plot_time_series(tsDiff, 'first difference')

#Test whether it is stationary 
adf.test(tsDiff)
Box.test(tsDiff, lag = 20, type = 'Ljung-Box')


#Stationary confirmation
plot_acf_pacf(tsDiff, 'First Difference Time Series Object')

#Fitting the model 
fit <- Arima(spTraining, order = c(0,1,1), include.drift = TRUE)
summary(fit)

# RESIDUAL DIAGNOSTICS
ggtsdiag_custom(fit, 'ARIMA(0,1,1)') + 
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.minor = element_blank(),
        axis.line.y = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) 

residFit <- ggplot(data=fit, aes(residuals(fit))) + 
  geom_histogram(aes(y =..density..),  
                 binwidth = 5,
                 col="turquoise4", fill="white") +
  geom_density(col="turquoise4") +
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.minor = element_blank(),
        axis.line   = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) +
  ggtitle("Plot of SP 500 ARIMA Model Residuals") 


fit_arima <- forecast(fit, h = 12)
sp500_test <- window(sp_500, 2015, c(2015, 12))
plot(fit_arima)
plot(sp500_test)
accuracy(fit_arima, sp500_test)
