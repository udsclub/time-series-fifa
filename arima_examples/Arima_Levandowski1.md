Arima\_model
================

``` r
#load and prepare data
stats <- read.csv("D:/UDSC/Time series/player_stats.csv")
price <- read.csv("D:/UDSC/Time series/player_price.csv") 

#convert Unix epoch time to date object
price$timestamp <- anydate((price$timestamp/1000))

#convert price to numeric type
price$xbox_price <- as.numeric(price$xbox_price)

#merge two datasets
data <- left_join(price, stats, by = 'player_id')
```

``` r
# choose one players' id
player <- data %>% filter (player_name == 'Lewandowski' & player_id == 15150 & timestamp > '2016-10-13')

#count number of 0-values
sum(player$xbox_price == 0)
## [1] 0

#if there are 0-values
#player %>% filter(xbox_price == 0) %>% select(timestamp)

#replace NA to approximation based on before/after values (for further modeling)
#player$xbox_price[player$xbox_price == 0] <- NA
#na.approx(player$xbox_price)
```

``` r
#plot the data
ggplot(player, aes(timestamp, xbox_price)) + geom_line(size = 1.0, color = 'blue') + xlab("Date")
```

<img src=https://github.com/valentinadeshko/Time-series-Fifa2017/blob/master/Arima_plots/fig-unnamed-chunk-5-1.png>

``` r
#Lets plot weekly and monthly MA
player$xbox_price_ma = ma(player$xbox_price, order=7) # using the clean count with no outliers
player$xbox_price_ma30 = ma(player$xbox_price, order=30)

ggplot() +
  geom_line(data = player, aes(x = timestamp, y = xbox_price, colour = "Xbox_price"), size = 1.0) +
  geom_line(data = player, aes(x = timestamp, y = xbox_price_ma,   colour = "Weekly Moving Average"), size = 1.0)  +
  geom_line(data = player, aes(x = timestamp, y = xbox_price_ma30, colour = "Monthly Moving Average"), size = 1.0) +
  xlab("Date") + 
  theme(legend.position = "bottom")
## Warning: Removed 6 rows containing missing values (geom_path).
## Warning: Removed 30 rows containing missing values (geom_path).
```

<img src=https://github.com/valentinadeshko/Time-series-Fifa2017/blob/master/Arima_plots/fig-unnamed-chunk-6-1.png>

``` r
#decompose on seasonal, trend and cycle components
count = ts(na.omit(player$xbox_price), frequency=30)
decomp = stl(count, s.window="periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)
```

<img src=https://github.com/valentinadeshko/Time-series-Fifa2017/blob/master/Arima_plots/fig-unnamed-chunk-7-1.png>

We dont see any seasonal components.

``` r
#Stationarity: Dickey-Fuller Test
adf.test(player$xbox_price, alternative = "stationary")
## 
##  Augmented Dickey-Fuller Test
## 
## data:  player$xbox_price
## Dickey-Fuller = -3.7695, Lag order = 6, p-value = 0.02102
## alternative hypothesis: stationary
```

We see that the series is stationary enough to do any kind of time series modelling. Lets try to difference the data

``` r
plot(diff(player$xbox_price), type = 'l')
```

<img src=https://github.com/valentinadeshko/Time-series-Fifa2017/blob/master/Arima_plots/fig-unnamed-chunk-9-1.png>

We can see that the data became less volatility. Lets use d = 1

Next step is to find the right parameters to be used in the ARIMA model. We already know that the ‘d’ component is 1 as we need 1 difference to make the series stationary. We do this using the Correlation plots. Following are the ACF plots for the series

``` r
Acf(player$xbox_price, main='')
```

<img src=https://github.com/valentinadeshko/Time-series-Fifa2017/blob/master/Arima_plots/fig-unnamed-chunk-10-1.png>

``` r

Pacf(player$xbox_price, main='')
```

<img src=https://github.com/valentinadeshko/Time-series-Fifa2017/blob/master/Arima_plots/fig-unnamed-chunk-10-2.png>

Still ACF looks not very good, lets differentiate it

``` r
#Next, spikes at particular lags of the differenced series can help inform the 
#choice of p or q for our model

Acf(diff(player$xbox_price), main='ACF for Differenced Series')
```

<img src=https://github.com/valentinadeshko/Time-series-Fifa2017/blob/master/Arima_plots/fig-unnamed-chunk-11-1.png>

``` r
Pacf(diff(player$xbox_price), main='PACF for Differenced Series')
```

<img src=https://github.com/valentinadeshko/Time-series-Fifa2017/blob/master/Arima_plots/fig-unnamed-chunk-11-2.png>

``` r
#Fitting an ARIMA model

auto.arima(player$xbox_price, seasonal=FALSE)
## Series: player$xbox_price 
## ARIMA(3,1,2) with drift         
## 
## Coefficients:
##          ar1      ar2      ar3      ma1     ma2      drift
##       0.0035  -0.9418  -0.1156  -0.2033  0.9401  -2085.707
## s.e.  0.0716   0.0346   0.0702   0.0301  0.0583   1186.043
## 
## sigma^2 estimated as 505275018:  log likelihood=-2868.74
## AIC=5751.47   AICc=5751.93   BIC=5776.15
```

``` r
#Evaluate and Iterate
fit<-auto.arima(player$xbox_price, seasonal=FALSE)
tsdisplay(residuals(fit), lag.max=45, main='(3,1,2) Model Residuals')
```

<img src=https://github.com/valentinadeshko/Time-series-Fifa2017/blob/master/Arima_plots/fig-unnamed-chunk-13-1.png>

``` r

summary(fit)
## Series: player$xbox_price 
## ARIMA(3,1,2) with drift         
## 
## Coefficients:
##          ar1      ar2      ar3      ma1     ma2      drift
##       0.0035  -0.9418  -0.1156  -0.2033  0.9401  -2085.707
## s.e.  0.0716   0.0346   0.0702   0.0301  0.0583   1186.043
## 
## sigma^2 estimated as 505275018:  log likelihood=-2868.74
## AIC=5751.47   AICc=5751.93   BIC=5776.15
## 
## Training set error measures:
##                     ME     RMSE      MAE        MPE     MAPE      MASE
## Training set -46.80549 22163.93 15733.08 -0.1922736 5.009517 0.9603775
##                      ACF1
## Training set -0.002522432
```

``` r
#residuals analysis: Heteroskedasticity
adf.test(residuals(fit), alternative = "stationary")
## Warning in adf.test(residuals(fit), alternative = "stationary"): p-value
## smaller than printed p-value
## 
##  Augmented Dickey-Fuller Test
## 
## data:  residuals(fit)
## Dickey-Fuller = -6.2287, Lag order = 6, p-value = 0.01
## alternative hypothesis: stationary

#residuals analysis: unbiasedness (mean equals 0)
t.test(residuals(fit))
## 
##  One Sample t-test
## 
## data:  residuals(fit)
## t = -0.033457, df = 251, p-value = 0.9733
## alternative hypothesis: true mean is not equal to 0
## 95 percent confidence interval:
##  -2802.024  2708.413
## sample estimates:
## mean of x 
## -46.80549
#residuals are unbiased

#residuals analysis: independent residuals
Box.test(residuals(fit), lag = 10, type = "Ljung-Box", fitdf = 4)
## 
##  Box-Ljung test
## 
## data:  residuals(fit)
## X-squared = 6.0385, df = 6, p-value = 0.4189
# null hypothesis - The data are independently distributed 
```

There are no heteroskedasticity, unbiasedness and correlation in residuals.

``` r
#forecasting
hold <- window(ts(player$xbox_price), start=238)

fit_no_holdout = arima(ts(player$xbox_price[-c(238:252)]), order=c(3,1,2))

fcast_no_holdout <- forecast(fit_no_holdout,h=14)
plot(fcast_no_holdout, main=" ")
lines(ts(player$xbox_price))
```

<img src=https://github.com/valentinadeshko/Time-series-Fifa2017/blob/master/Arima_plots/fig-unnamed-chunk-15-1.png>

Model summary

``` r
summary(fit_no_holdout)
## 
## Call:
## arima(x = ts(player$xbox_price[-c(238:252)]), order = c(3, 1, 2))
## 
## Coefficients:
##          ar1      ar2      ar3      ma1     ma2
##       0.0203  -0.9510  -0.1025  -0.2015  0.9515
## s.e.  0.0727   0.0296   0.0713   0.0287  0.0464
## 
## sigma^2 estimated as 513224278:  log likelihood = -2702.14,  aic = 5416.28
## 
## Training set error measures:
##                     ME     RMSE      MAE        MPE     MAPE      MASE
## Training set -2365.563 22606.65 16159.85 -0.9017721 4.950121 0.9756953
##                     ACF1
## Training set -0.01265072
```
