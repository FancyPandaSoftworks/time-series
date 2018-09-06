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
library(stringr)
source('helper_functions.R')

#Loading in the data
myData <- read.csv("sp500.csv")
colnames(myData)[1]<- "year"
View(myData)
myData<- myData[1:248,]
month <- data.frame( month = myData$month)
month$month<- str_pad(month$month, 2, pad = "0")

prophetData <-data.frame(ds = paste(myData$year, month$month))
prophetData$ds <- gsub(" ", "-", prophetData$ds)
prophetData$ds <- paste(prophetData$ds, "-01")
prophetData$ds <- gsub(" ", "", prophetData$ds)
prophetData$y <- myData$sp_500
View(prophetData)

m <- prophet(prophetData)
future <- make_future_dataframe(m, periods = 122)
tail(future)

forecasting <- predict(m, future)
plot(m, forecasting)
prophet_plot_components(m, forecasting)
dyplot.prophet(m, forecasting)

#Actual data
forecastingComparison <- ts(forecasting[1:248,c(12,13,16)],start=c(1995, 1), freq=12)
forecastingComparison <- cbind(forecastingComparison, myData$sp_500[1:248])
plot_time_series(forecastingComparison, "forecasting comparison")


#Predicted data
monthComparison <- ts(forecasting[c(279, 309, 340, 370), c(16)], start= c(1995,1), freq=12)
monthComparison <- cbind(monthComparison, myData$sp_500[249:252])
#monthComparison<- cbind(monthComparison, abs(monthComparison[,1]-monthComparison[,2]))
plot_time_series(monthComparison, "Month comparison")

#Combining actual data with predicted data
actualPredicted <- ts(forecasting[c(1:248,279,309,340,370),c(12,13,16)], start = c(1995,1), freq = 12)
actualPredicted <- cbind(actualPredicted, myData$sp_500)
plot_time_series(actualPredicted, "Actual data and predicted data")



#With arima predicitons 
predictionWithArima <- ts(forecasting[c(241:248,279,309,340,370),c(12,13,16)], start = c(2015,1), freq = 12)
predictionWithArima <- cbind(predictionWithArima, myData$sp_500[241:252])
predictionWithArima <- cbind(predictionWithArima, fit_arima$lower)


View(predictionWithArima)
colnames(predictionWithArima)[1]<- "Prophet lower bound"
colnames(predictionWithArima)[2]<- "Prophet upper bound"
colnames(predictionWithArima)[3]<- "Prophet average"
colnames(predictionWithArima)[4]<- "Actual line"
colnames(predictionWithArima)[5]<- "Arima 80%"
colnames(predictionWithArima)[6]<- "Arima 95%"
plot_time_series(predictionWithArima, "Actual data and predicted data")


