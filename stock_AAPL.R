################# Stock Market Price Prediction using R #################

##Importing Required Packages
library(quantmod)
library(tseries)
library(timeSeries)
library(forecast)


##Importing Dataset from Finance Websites...(Default yahoo)
getSymbols('AAPL', from = '2019-01-01', to = '2021-01-01')
View(AAPL)
#class(AAPL)


chartSeries(AAPL, subset = 'last 6 months', type = 'auto')
addBBands()

##Assigning columns of dataset  
Open_prices = AAPL[,1]
High_prices = AAPL[,2]
Low_prices = AAPL[,3]
Close_prices = AAPL[, 4]
Volume_prices = AAPL[,5]
Adjusted_prices = AAPL[,6]

par(mfrow = c(2,3))

plot(Open_prices, main = 'Opening Price of Stocks (Over a given period)')
plot(High_prices, main = 'Highest Price of Stocks (Over a given period)')
plot(Low_prices, main = 'Lowest Price of Stocks (Over a given period)')
plot(Close_prices, main = 'Closing Price of Stocks (Over a given period)')
plot(Volume_prices, main = 'Volume of Stocks (Over a given period)')
plot(Adjusted_prices, main = 'Adjusted Price of Stocks (Over a given period)')

Predic_Price = Adjusted_prices
#class(Predic_Price)


######## Finding the Linear Relation between observations ########

par(mfrow = c(1,2))
Acf(Predic_Price, main = 'ACF for differenced Series')
Pacf(Predic_Price, main = 'PACF for differenced Series ', col = '#cc0000')
Auto_cf = Acf(Predic_Price, plot = FALSE)
Auto_cf
PAuto_cf = Pacf(Predic_Price, plot = FALSE)
PAuto_cf

print(adf.test(Predic_Price))




################### Prediction of Return ##########################

return_AAPL <- 100*diff(log(Predic_Price))

AAPL_return_train <- return_AAPL[1:(0.9*length(return_AAPL))]

AAPL_return_test <- return_AAPL[(0.9*length(return_AAPL)+1):length(return_AAPL)]

auto.arima(AAPL_return_train, seasonal = FALSE)

fit <- Arima(AAPL_return_train, order = c(1,0,0))

preds <- predict(fit, n.ahead = (length(return_AAPL) - (0.9*length(return_AAPL))))$pred
preds


################## Forecasting Predicted Result ##################

test_forecast <- forecast(fit,h = 15)
test_forecast

par(mfrow = c(1,1))
plot(test_forecast, main = "Arima forecast for Apple Stock")

accuracy(preds, AAPL_return_test)



