# ForecastingEnsembles

<!-- badges: start -->
<!-- badges: end -->

The goal of ForecastingEnsembles is to automatically run 23 forecasting models, return 19 plots and tables.

## Installation

You can install the development version of ForecastingEnsembles like so:

``` r
devtools::install_github("InfiniteCuriosity/ForecastingEnsembles")
```

## Example

This is a basic example which shows you how to solve a common problem of forecasting data that is reported weekly, monthly or quarterly. The Forecasting Ensembles package only needs the data, and it will build 23 models:

    Linear1 = TSLM(Value ~ season() + trend()),
    Linear2 = TSLM(Value),
    Linear3 = TSLM(Value ~ season()),
    Linear4 = TSLM(Value ~ trend()),
    Arima1 = ARIMA(Value ~ season() + trend(), stepwise = TRUE, greedy = TRUE, approximation = TRUE),
    Arima2 = ARIMA(Value ~ season(), stepwise = TRUE, greedy = TRUE, approximation = TRUE),
    Arima3 = ARIMA(Value ~ trend(), stepwise = TRUE, greedy = TRUE, approximation = TRUE),
    Arima4 = ARIMA(Value),
    Deterministic = ARIMA(Value ~ 1 + pdq(d = 0), stepwise = TRUE, greedy = TRUE, approximation = TRUE),
    Stochastic = ARIMA(Value ~ pdq(d = 1), stepwise = TRUE, greedy = TRUE, approximation = TRUE),
    Ets1 = ETS(Value ~ season() + trend()),
    Ets2 = ETS(Value ~ trend()),
    Ets3 = ETS(Value ~ season()),
    Ets4 = ETS(Value),
    Holt_Winters_Additive = ETS(Value ~ error("A") + trend("A") + season("A")),
    Holt_Winters_Multiplicative = ETS(Value ~ error("M") + trend("A") + season("M")),
    Holt_Winters_Damped = ETS(Value ~ error("M") + trend("Ad") + season("M")),
    Fourier1 = ARIMA((Value) ~ fourier(K = 1) + PDQ(0, 0, 0)),
    Fourier2 = ARIMA((Value) ~ fourier(K = 2) + PDQ(0, 0, 0)),
    Fourier3 = ARIMA((Value) ~ fourier(K = 3) + PDQ(0, 0, 0)),
    Fourier4 = ARIMA((Value) ~ fourier(K = 4) + PDQ(0, 0, 0)),
    Fourier5 = ARIMA((Value) ~ fourier(K = 5) + PDQ(0, 0, 0)),
    Fourier6 = ARIMA((Value) ~ fourier(K = 6) + PDQ(0, 0, 0))

``` r
library(ForecastingEnsembles)
## Forecasting(time_series_data = Oct_2024_all_nonfarm, train_amount = 0.60, number_of_intervals_to_forecast = 1, use_parallel = "Y", time_interval = "M")
```

The Forecasting Ensembles package returns:

Plot of the value of the time series
Plot of individual seasons
Head of the data
Tail of the data
Plot of the trend
Plot of the seasonally adjusted data
Plot of the decomposition
Plot of anomalies
Plot of subseasons
Plot of multiple lags
Table of time series features
Table of time series quartiles
Table of the best model forecast
Plot of the best model forecast
Plot of best autocorrelation function (ACF)
Plot of the best model histogram of the residuals
Plot of the best model actual vs predicted
Plot of the best model actual vs residuals
Plot of the best model baseline data
