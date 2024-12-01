#' forecasting—function to perform time series analysis and return the results to the user.
#'
#' @param time_series_name the name of the time series in quotation marks
#' @param time_series_data a time series
#' @param train_amount The amount to use for the training set, such as 0.60
#' @param number_of_intervals_to_forecast the number of intervals, such as months or weeks, that are going to be forecast
#' @param time_interval user states whether the time interval is quarterly, monthly or weekly.
#' @param use_parallel "Y" or "N" for parallel processing
#'
#' @returns A series of summary reports and visualizations to fully describe the time series: Forecast accuracy, forecast numbers, forecast plot, innovation residuals,
#' @returns best autocorrelation function (ACF), plot of best histogram of residuals, plot of best actual vs predicted, plot of best actual vs trend
#' @returns plot of best actual vs seasonally adjusted
#' @export Forecasting
#'
#' @importFrom doParallel registerDoParallel
#' @importFrom dplyr arrange bind_rows count filter mutate select
#' @importFrom fable ARIMA ETS MEAN NAIVE NNETAR RW SNAIVE TSLM
#' @importFrom fabletools accuracy as_tsibble augment autoplot components features report
#' @importFrom fable.prophet prophet
#' @importFrom feasts gg_tsresiduals STL
#' @importFrom ggplot2 aes facet_grid geom_line geom_hline geom_abline geom_point ggplot ggtitle guides labs scale_x_continuous scale_y_continuous theme xlab ylab
#' @importFrom gt gt tab_header fmt_number fmt_percent
#' @importFrom magrittr %>%
#' @importFrom parallel makeCluster
#' @importFrom readr read_csv
#' @importFrom stats stl AIC BIC lag sd quantile
#' @importFrom tibble tibble as_tibble
#' @importFrom tidyr pivot_longer drop_na
#' @importFrom tsibble tsibble
#' @importFrom utils tail head

#' @examples
#' Forecasting(time_series_name = "Oct 2024 All nonfarm",
#'   time_series_data = Oct_2024_all_nonfarm,
#'   train_amount = 0.60,
#'   number_of_intervals_to_forecast = 1,
#'   use_parallel = "Y",
#'   time_interval = "M")
#'
#' Forecasting(time_series_name = "Oct 2024 Hourly pay",
#'   time_series_data = Oct_2024_avg_hourly_pay,
#'   train_amount = 0.60,
#'   number_of_intervals_to_forecast = 1,
#'   use_parallel = "Y",
#'   time_interval = "M")
#'
#' Forecasting(time_series_name = "Oct 2024 Unemployment",
#'   time_series_data = Oct_2024_unemployment,
#'   train_amount = 0.60,
#'   number_of_intervals_to_forecast = 1,
#'   use_parallel = "Y",
#'   time_interval = "M")

Forecasting <- function(time_series_name, time_series_data, train_amount, number_of_intervals_to_forecast, use_parallel = c("Y", "N"), time_interval = c("Q", "M", "W")) {
  use_parallel <- 0
  no_cores <- 0

  if (use_parallel == "Y") {
    cl <- parallel::makeCluster(no_cores, type = "FORK")
    doParallel::registerDoParallel(cl)
  }

  time_series_data <- time_series_data

  Label <- 0
  Value <- 0
  Month <- 0
  trend <- 0
  season_adjust <- 0
  time_series_decomposition <- 0
  table_of_value_head <- 0
  table_of_value_tail <- 0
  forecast_accuracy <- 0
  Time_Series <- 0
  Linear1 <- 0
  Linear2 <- 0
  Linear3 <- 0
  Linear4 <- 0
  Arima1 <- 0
  Arima2 <- 0
  Arima3 <- 0
  Arima4 <- 0
  Deterministic <- 0
  Stochastic <- 0
  Ets1 <- 0
  Ets2 <- 0
  Ets3 <- 0
  Ets4 <- 0
  Holt_Winters_Additive <- 0
  Holt_Winters_Multiplicative <- 0
  Holt_Winters_Damped <- 0
  Fourier1 <- 0
  Fourier2 <- 0
  Fourier3 <- 0
  Fourier4 <- 0
  Fourier5 <- 0
  Fourier6 <- 0
  Prophet_Additive <- 0
  Prophet_Multiplicative <- 0
  NeuralNet1 <- 0
  NeuralNet2 <- 0
  NeuralNet3 <- 0
  NeuralNet4 <- 0
  VAR1 <- 0
  vars <- 0
  Mean <- 0
  Naive <- 0
  SNaive <- 0
  Drift <- 0
  .model <- 0
  RMSSE <- 0
  RMSE <- 0
  .resid <- 0
  .fitted <- 0
  .mean <- 0
  .innov <- 0
  Time_Series_AIC_fit <- 0
  AICc <- 0
  Difference <- 0
  Period <- 0


  if (time_interval == "Q") {
    time_series_data <- time_series_data %>%
      dplyr::mutate(Period = tsibble::yearquarter(Period), Value = Value) %>%
      dplyr::select(Period, Value) %>%
      fabletools::as_tsibble(index = Period)
  }
  if (time_interval == "M") {
    time_series_data <- time_series_data %>%
      dplyr::mutate(Period = tsibble::yearmonth(Period), Value = Value) %>%
      dplyr::select(Period, Value) %>%
      fabletools::as_tsibble(index = Period)
  }
  if (time_interval == "W") {
    time_series_data <- time_series_data %>%
      dplyr::mutate(Period = tsibble::yearweek(Period), Value = Value) %>%
      dplyr::select(Period, Value) %>%
      fabletools::as_tsibble(index = Period)
  }

  #### Time series features ####

  value_features <- time_series_data %>%
    fabletools::features(.var = Value, feasts::feat_stl)

  value_trend_strength <- value_features$trend_strength

  value_season_strength_year <- value_features$seasonal_strength_year

  value_season_peak_year <- value_features$seasonal_peak_year

  value_season_trough_year <- value_features$seasonal_trough_year

  value_spikiness <- value_features$spikiness

  value_linearity <- value_features$linearity

  value_curvature <- value_features$curvature

  value_coef_hurst <- feasts::coef_hurst(x = time_series_data$Value)

  value_spectral <- feasts::feat_spectral(x = time_series_data$Value)

  value_box_pierce <- feasts::box_pierce(x = time_series_data$Value)

  value_ljung_box <- feasts::ljung_box(x = time_series_data$Value)
  value_ljung_box_stat <- value_ljung_box[1]
  value_ljung_box_pvalue <- value_ljung_box[2]

  value_unitroot_kpss <- feasts::unitroot_kpss(x = time_series_data$Value)
  value_kpss_stat <- value_unitroot_kpss[1]
  value_kpss_pvalue <- value_unitroot_kpss[2]
  value_unitroot_ndiffs <- feasts::unitroot_ndiffs(x = time_series_data$Value)
  value_unitroot_nsdiffs <- feasts::unitroot_nsdiffs(x = time_series_data$Value)
  value_longest_flat <- feasts::longest_flat_spot(x = time_series_data$Value)
  value_n_crossing_points <- feasts::n_crossing_points(x = time_series_data$Value)
  value_longest_flat_spot <- feasts::longest_flat_spot(x = time_series_data$Value)
  value_feat_acf <- feasts::feat_acf(x = time_series_data$Value)

  all_time_series_features <- gt::gt(data.frame(
    'Feature' = c('Ndiffs', 'NSDiffs', 'Trend strength', 'KPSS stat', 'KPSS p-value', 'Seasonal strength year', 'Seasonal peak year', 'Seasonal trough year', 'Spikeiness', 'Linearity', 'Curvature', 'Coef hurst', 'Spectral',
                  'Box pierce stat', 'Box pierce pvalue', 'Ljung box stat', 'Ljung box p-value', 'Longest_flat', 'Crossing_points'),
    'Amount' = format(round(c(value_unitroot_ndiffs, value_unitroot_nsdiffs, value_trend_strength, value_kpss_stat, value_kpss_pvalue, value_season_peak_year, value_season_peak_year, value_season_trough_year, value_spikiness, value_linearity, value_curvature, value_coef_hurst, value_spectral,
                              value_box_pierce, value_ljung_box_stat, value_ljung_box_pvalue, value_longest_flat, value_n_crossing_points), 4), scientific = F)
  )) %>%
    gt::tab_header(
      title = c("Time series features for", time_series_name)
    )


  #### Quartiles and Quintiles ####
  time_series_quartiles <- time_series_data %>%
    fabletools::features(Value, quantile, prob = seq(0, 1, 0.25))

  time_series_quartiles <- gt::gt(time_series_quartiles) %>%
    gt::tab_header(
      title = c("Time series quartiles", time_series_name)
    )


  #########

  seasonal_plots <- time_series_data %>%
    feasts::gg_season(y = Value, labels = "both") +
    labs(title = paste0("Seasonal plot ", time_series_name)
    )

  #########

  lag_plots <- time_series_data %>%
    feasts::gg_lag(y = Value) +
    labs(title = paste0("Lag plots ", time_series_name)
    )


  # Baseline forecasts—Can you beat these?
  Mean_value <- mean(time_series_data$Value)
  SD_Value <- sd(time_series_data$Value)
  Mean_value_most_recent <- mean(tail(time_series_data$Value, 3))
  SD_value_most_recent <- sd(tail(time_series_data$Value, 3))

  Min_value <- min(time_series_data$Value)

  Max_value <- max(time_series_data$Value)

  value_mean <- mean(time_series_data$Value)
  value_sd <- sd(time_series_data$Value)
  Baseline_full <- gt::gt(data.frame("Names" = c('Mean value', 'Mean value 3 most recent', 'Minimum', 'Maximum'),
                                     "Measures" = c(Mean_value, Mean_value_most_recent, Min_value, Max_value))
  ) %>%
    gt::tab_header(
      title = c("Minimum, mean and maximum statistics", time_series_name)
    )



  # <----- Plot of Value ----------------------------------------------> ####
  plot_of_value <- time_series_data %>%
    ggplot2::ggplot(aes(x = Period, y = Value)) +
    ggplot2::geom_line(aes(x = Period, y = Value)) +
    ggplot2::geom_point(aes(x = Period, y = Value)) +
    ggplot2::labs(title = paste0("Value per unit of time, ", time_series_name))


  # <----- Plot of individual periods ----------------------------------------------> ####
  plot_of_individual_periods <- time_series_data %>%
    feasts::gg_season(Value) +
    ggplot2::labs(title = paste0("Plot of individual periods, ", time_series_name))

  # <----- Plot of subseasons ----------------------------------------------> ####
  plot_of_subseasons <- time_series_data %>%
    feasts::gg_subseries(y = Value) +
    ggplot2::labs(title = paste0("Plot of subseasons, ", time_series_name))

  # <----- Plot of multiple lags ---------------------------------------------->
  plot_of_multiple_lags <- time_series_data %>%
    feasts::gg_lag(y = Value, geom = "point") +
    ggplot2::geom_point() +
    ggplot2::labs(title = paste0("Plot of multiple lags, ", time_series_name))

  # <----- Tail of Table of Value ---------------------------------------------> ####

  table_of_value_tail <- gt::gt(tail(time_series_data[, c(1:2)]),
                                caption = paste0("Tail (most recent) of total value data set, ", time_series_name)
  ) %>%
    gt::fmt_number(columns = c("Value"), decimals = 0, use_seps = TRUE)

  # <----- Head of Table of Value ---------------------------------------------> ####

  table_of_value_head <- gt::gt(head(time_series_data[, c(1:2)]),
                                caption = paste0("Head (beginning) of total value data set, ", time_series_name)
  ) %>%
    gt::fmt_number(columns = c("Value"), decimals = 0, use_seps = TRUE)

  # <----- Plot of Trend of the Value ---------------------------------> ####
  time_series_decomposition <- time_series_data %>%
    fabletools::model(stl = STL(Value)) %>%
    fabletools::components()

  plot_of_trend <- time_series_decomposition %>%
    tibble::as_tibble() %>%
    ggplot2::ggplot(mapping = aes(x = Period, y = Value)) +
    ggplot2::geom_line(aes(x = Period, y = Value)) +
    ggplot2::geom_line(aes(x = Period, y = trend, color = "gray")) +
    ggplot2::labs(
      y = "Total number of value",
      title = paste0("Total value in black, with trend (in red), ", time_series_name)
    )

  # <----- Plot Current Value vs Seasonally Adjusted-> ####

  plot_of_seasonally_adjusted <- time_series_decomposition %>%
    tibble::as_tibble() %>%
    ggplot2::ggplot(mapping = aes(x = Period, y = season_adjust)) +
    ggplot2::geom_line(aes(x = Period, y = Value)) +
    ggplot2::geom_line(aes(x = Period, y = season_adjust, color = "gray")) +
    ggplot2::labs(
      y = "Total number of value",
      title = paste0("Total value in black, with season_adjust (in red), ", time_series_name)
    )

  # <----- Plot of Decomposition of the Value--------------------------> ####

  plot_of_decomposition <- time_series_decomposition %>%
    feasts::autoplot() +
    ggplot2::labs(title = paste0("Plot of decomposition, ", time_series_name))

  # <----- Plot of Anomalies of the Value------------------------------> ####

  remainder <- time_series_decomposition$remainder

  remainder1 <- stats::sd(remainder)

  time_series_anomalies <- ggplot2::ggplot(data = time_series_decomposition, aes(x = Period, y = remainder)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = c(remainder1, -remainder1), linetype = "dashed", color = "blue") +
    ggplot2::geom_hline(yintercept = c(2 * remainder1, -2 * remainder1), linetype = "dashed", color = "red") +
    ggplot2::geom_hline(yintercept = 0, color = "black") +
    ggplot2::labs(title = "Anomalies in value data \nblue line = 1 standard deviation +/- 0, red line = 2 standard deviations +/- 0")


  # Baseline forecasts—Can you beat these?
  Mean_value <- mean(time_series_data$Value)
  SD_Value <- sd(time_series_data$Value)
  Mean_value_most_recent <- mean(head(time_series_data$Value, 3))
  SD_value_most_recent <- sd(head(time_series_data$Value, 3))

  Min_value <- min(time_series_data$Value)
  Max_value <- max(time_series_data$Value)

  value_mean <- mean(time_series_data$Value)
  value_sd <- sd(time_series_data$Value)

  baseline_full <- gt::gt(data.frame('Baseline' = c('Value'),
                                     'Mean' = c(value_mean),
                                     'Std.Dev' = c(value_sd)))%>%
    gt::tab_header(
      title = paste0("Results from the entire time series, ", time_series_name),
    )

  # <--- Models to predict Value ----> ####

  time_series_train <- time_series_data[1:round(train_amount * nrow(time_series_data)), ] %>% tidyr::drop_na()
  time_series_test <- time_series_data[nrow(time_series_train) + 1:(nrow(time_series_data) - nrow(time_series_train)), ] %>% tidyr::drop_na()



  ##############################################

  ###### Start AICc methods here ###############

  ##############################################

  # These models will be evaluated using AICc
  time_series_fit <- time_series_train %>%
    fabletools::model(
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
    )

  time_series_results <- time_series_fit %>%
    fabletools::glance(time_series_test) %>%
    dplyr::select(.model, AICc, AIC, BIC) %>%
    dplyr::arrange(AICc)

  forecast_accuracy_table <- gt::gt(time_series_results, caption = paste0("Time series forecast accuracy, sorted by AICc, ", time_series_name))

  #### Evaluate results starting here ####

  if (time_series_results[1, 1] == "Arima1") {
    Best_Model <- time_series_data %>%
      fabletools::model(
        Arima1 = fable::ARIMA(Value ~ season() + trend(), stepwise = TRUE, greedy = TRUE, approximation = TRUE)
      )

    Best_Forecast <- Best_Model %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast <- gt::gt(data = Best_Forecast, caption = paste0("Best Forecast, ", time_series_name)) %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot <- Best_Model %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::labs(title = paste0("Arima 1 model forecast of value,  ", time_series_name)) +
      ggplot2::ylab("Total value")

    Best_Innovation_Residuals <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Period, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = paste0("Arima 1 innovation residuals, ", time_series_name))

    Best_ACF <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = paste0("Arima 1 Autocorrelation function, ", time_series_name))

    Best_Histogram_of_Residuals <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = paste0("Arima 1 histogram of residuals, ", time_series_name))

    Best_Actual_vs_Predicted <-
      ggplot2::ggplot(fabletools::augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Arima1 Actual vs Predicted, ", time_series_name)) +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals <-
      ggplot2::ggplot(fabletools::augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Arima1 Actual vs Residuals, ", time_series_name)) +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (time_series_results[1, 1] == "Arima2") {
    Best_Model <- time_series_data %>%
      fabletools::model(
        Arima2 = ARIMA(Value ~ season(), stepwise = TRUE, greedy = TRUE, approximation = TRUE)
      )

    Best_Forecast <- Best_Model %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast <- gt::gt(data = Best_Forecast, caption = paste0("Best Forecast using Arima 2, ", time_series_name)) %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot <- Best_Model %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::labs(title = paste0("Arima 2 model forecast of value, ", time_series_name)) +
      ggplot2::ylab("Total value")

    Best_Innovation_Residuals <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Period, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = paste0("Arima 2 innovation residuals, ", time_series_name))

    Best_ACF <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = paste0("Arima 2 Autocorrelation function, ", time_series_name))

    Best_Histogram_of_Residuals <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = paste0("Arima 2 histogram of residuals, ", time_series_name))

    Best_Actual_vs_Predicted <-
      ggplot2::ggplot(fabletools::augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Arima 2 Actual vs Predicted, ", time_series_name)) +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals <-
      ggplot2::ggplot(fabletools::augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Arima 2 Actual vs Residuals, ", time_series_name)) +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (time_series_results[1, 1] == "Arima3") {
    Best_Model <- time_series_data %>%
      fabletools::model(
        Arima3 = ARIMA(Value ~ trend(), stepwise = TRUE, greedy = TRUE, approximation = TRUE)
      )

    Best_Forecast <- Best_Model %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast <- gt::gt(data = Best_Forecast, caption = paste0("Best Forecast using Arima 3, ", time_series_name)) %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot <- Best_Model %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::labs(title = paste0("Arima 3 model forecast of value, ", time_series_name)) +
      ggplot2::ylab("Total value")

    Best_Innovation_Residuals <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Period, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = paste0("Arima 3 innovation residuals, ", time_series_name))

    Best_ACF <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = paste0("Arima 3 Autocorrelation function, ", time_series_name))

    Best_Histogram_of_Residuals <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = paste0("Arima 3 histogram of residuals, ", time_series_name))

    Best_Actual_vs_Predicted <-
      ggplot2::ggplot(fabletools::augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Arima 3 Actual vs Predicted, ", time_series_name)) +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals <-
      ggplot2::ggplot(fabletools::augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Arima 3 Actual vs Residuals, ", time_series_name)) +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (time_series_results[1, 1] == "Arima4") {
    Best_Model <- time_series_data %>%
      fabletools::model(
        Arima4 = ARIMA(Value)
      )

    Best_Forecast <- Best_Model %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast <- gt::gt(data = Best_Forecast, caption = paste0("Best Forecast using Arima 4, ", time_series_name)) %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot <- Best_Model %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::labs(title = paste0("Arima 4 model forecast of value, ", time_series_name)) +
      ggplot2::ylab("Total value")

    Best_Innovation_Residuals <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Period, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = paste0("Arima 4 innovation residuals", time_series_name))

    Best_ACF <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = paste0("Arima 4 Autocorrelation function, ", time_series_name))

    Best_Histogram_of_Residuals <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = paste0("Arima 4 histogram of residuals, ", time_series_name))

    Best_Actual_vs_Predicted <-
      ggplot2::ggplot(fabletools::augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Arima 4 Actual vs Predicted, ", time_series_name)) +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals <-
      ggplot2::ggplot(fabletools::augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Arima 4 Actual vs Residuals, ", time_series_name)) +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (time_series_results[1, 1] == "Deterministic") {
    Best_Model <- time_series_data %>%
      fabletools::model(
        Deterministic = ARIMA(Value ~ 1 + pdq(d = 0), stepwise = TRUE, greedy = TRUE, approximation = TRUE)
      )

    Best_Forecast <- Best_Model %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast <- gt::gt(data = Best_Forecast, caption = paste0("Best Forecast using Deterministic, ", time_series_name)) %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot <- Best_Model %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::labs(title = paste0("Deterministic model forecast of value, ", time_series_name)) +
      ggplot2::ylab("Total value")

    Best_Innovation_Residuals <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Period, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = paste0("Deterministic innovation residuals", time_series_name))

    Best_ACF <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = paste0("Deterministic Autocorrelation function, ", time_series_name))

    Best_Histogram_of_Residuals <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = paste0("Deterministic histogram of residuals, ", time_series_name))

    Best_Actual_vs_Predicted <-
      ggplot2::ggplot(fabletools::augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Deterministic Actual vs Predicted, ", time_series_name)) +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals <-
      ggplot2::ggplot(fabletools::augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Deterministic Actual vs Residuals, ", time_series_name)) +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (time_series_results[1, 1] == "Ets1") {
    Best_Model <- time_series_data %>%
      fabletools::model(
        Ets1 = ETS(Value ~ season() + trend())
      )

    Best_Forecast <- Best_Model %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast <- gt::gt(data = Best_Forecast, caption = paste0("Best Forecast using Ets1, ", time_series_name)) %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot <- Best_Model %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::labs(title = paste0("Ets1 model forecast of value, ", time_series_name)) +
      ggplot2::ylab("Total value")

    Best_Innovation_Residuals <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Period, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = paste0("Ets1 innovation residuals, ", time_series_name))

    Best_ACF <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = paste0("Ets1 Autocorrelation function, ", time_series_name))

    Best_Histogram_of_Residuals <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = paste0("Ets1 histogram of residuals, ", time_series_name))

    Best_Actual_vs_Predicted <-
      ggplot2::ggplot(fabletools::augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Ets1 Actual vs Predicted, ", time_series_name)) +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals <-
      ggplot2::ggplot(fabletools::augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Ets1 Actual vs Residuals,", time_series_name)) +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (time_series_results[1, 1] == "Ets2") {
    Best_Model <- time_series_data %>%
      fabletools::model(
        Ets2 = ETS(Value ~ trend())
      )

    Best_Forecast <- Best_Model %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast <- gt::gt(data = Best_Forecast, caption = paste0("Best Forecast using Ets 2, ", time_series_name)) %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot <- Best_Model %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::labs(title = paste0("Ets 2 model forecast of value, ", time_series_name)) +
      ggplot2::ylab("Total value")

    Best_Innovation_Residuals <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Period, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = paste0("Ets 2 innovation residuals, ", time_series_name))

    Best_ACF <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = paste0("Ets 2 Autocorrelation function, ", time_series_name))

    Best_Histogram_of_Residuals <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = paste0("Ets 2 histogram of residuals, ", time_series_name))

    Best_Actual_vs_Predicted <-
      ggplot2::ggplot(fabletools::augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Ets 2 Actual vs Predicted, ", time_series_name)) +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals <-
      ggplot2::ggplot(fabletools::augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Ets 2 Actual vs Residuals, ", time_series_name)) +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (time_series_results[1, 1] == "Ets3") {
    Best_Model <- time_series_data %>%
      fabletools::model(
        Ets3 = ETS(Value ~ season())
      )

    Best_Forecast <- Best_Model %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast <- gt::gt(data = Best_Forecast, caption = paste0("Best Forecast using Ets 3, ", time_series_name)) %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot <- Best_Model %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::labs(title = paste0("Ets 3 model forecast of value, ", time_series_name)) +
      ggplot2::ylab("Total value")

    Best_Innovation_Residuals <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Period, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = paste0("Ets 3 innovation residuals, ", time_series_name))

    Best_ACF <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = paste0("Ets 3 Autocorrelation function, ", time_series_name))

    Best_Histogram_of_Residuals <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = paste0("Ets 3 histogram of residuals, ", time_series_name))

    Best_Actual_vs_Predicted <-
      ggplot2::ggplot(fabletools::augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Ets 3 Actual vs Predicted, ", time_series_name)) +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals <-
      ggplot2::ggplot(fabletools::augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Ets 3 Actual vs Residuals, ", time_series_name)) +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (time_series_results[1, 1] == "Ets4") {
    Best_Model <- time_series_data %>%
      fabletools::model(
        Ets4 = ETS(Value)
      )

    Best_Forecast <- Best_Model %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast <- gt::gt(data = Best_Forecast, caption = paste0("Best Forecast using Ets 4, ", time_series_name)) %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot <- Best_Model %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::labs(title = paste0("Ets 4 model forecast of value, ", time_series_name)) +
      ggplot2::ylab("Total value")

    Best_Innovation_Residuals <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Period, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = paste0("Ets 4 innovation residuals, ", time_series_name))

    Best_ACF <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = paste0("Ets 4 Autocorrelation function, ", time_series_name))

    Best_Histogram_of_Residuals <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = paste0("Ets 4 histogram of residuals, ", time_series_name))

    Best_Actual_vs_Predicted <-
      ggplot2::ggplot(fabletools::augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Ets 4 Actual vs Predicted, ", time_series_name)) +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals <-
      ggplot2::ggplot(fabletools::augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Ets 4 Actual vs Residuals, ", time_series_name)) +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (time_series_results[1, 1] == "Fourier1") {
    Best_Model <- time_series_data %>%
      fabletools::model(
        Fourier1 = ARIMA((Value) ~ fourier(K = 1) + PDQ(0, 0, 0))
      )

    Best_Forecast <- Best_Model %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast <- gt::gt(data = Best_Forecast, caption = paste0("Best Forecast using Fourier 1, ", time_series_name)) %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot <- Best_Model %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::labs(title = paste0("Fourier 1 model forecast of value, ", time_series_name)) +
      ggplot2::ylab("Total value")

    Best_Innovation_Residuals <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Period, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = paste0("Fourier 1 innovation residuals, ", time_series_name))

    Best_ACF <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = paste0("Fourier 1 Autocorrelation function, ", time_series_name))

    Best_Histogram_of_Residuals <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = paste0("Fourier 1 histogram of residuals, ", time_series_name))

    Best_Actual_vs_Predicted <-
      ggplot2::ggplot(fabletools::augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Fourier 1 Actual vs Predicted, ", time_series_name)) +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals <-
      ggplot2::ggplot(fabletools::augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Fourier 1 Actual vs Residuals, ", time_series_name)) +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (time_series_results[1, 1] == "Fourier2") {
    Best_Model <- time_series_data %>%
      fabletools::model(
        Fourier2 = ARIMA((Value) ~ fourier(K = 2) + PDQ(0, 0, 0))
      )

    Best_Forecast <- Best_Model %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast <- gt::gt(data = Best_Forecast, caption = paste0("Best Forecast using Fourier 2, ", time_series_name)) %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot <- Best_Model %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::labs(title = paste0("Fourier 2 model forecast of value, ", time_series_name)) +
      ggplot2::ylab("Total value")

    Best_Innovation_Residuals <- fabletools::augment(Best_Model) %>%
      ggplot2::ggplot(aes(x = Period, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = paste0("Fourier 2 innovation residuals, ", time_series_name))

    Best_ACF <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = paste0("Fourier 2 Autocorrelation function, ", time_series_name))

    Best_Histogram_of_Residuals <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = paste0("Fourier 2 histogram of residuals, ", time_series_name))

    Best_Actual_vs_Predicted <-
      ggplot2::ggplot(fabletools::augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Fourier 2 Actual vs Predicted, ", time_series_name)) +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals <-
      ggplot2::ggplot(fabletools::augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Fourier 2 Actual vs Residuals, ", time_series_name)) +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (time_series_results[1, 1] == "Fourier3") {
    Best_Model <- time_series_data %>%
      fabletools::model(
        Fourier3 = ARIMA((Value) ~ fourier(K = 3) + PDQ(0, 0, 0))
      )

    Best_Forecast <- Best_Model %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast <- gt::gt(data = Best_Forecast, caption = "Best Forecast using Fourier 3") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot <- Best_Model %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::labs(title = paste0("Fourier 3 model forecast of value, ", time_series_name)) +
      ggplot2::ylab("Total value")

    Best_Innovation_Residuals <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Period, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = paste0("Fourier 3 innovation residuals, ", time_series_name))

    Best_ACF <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = paste0("Fourier 3 Autocorrelation function, ", time_series_name))

    Best_Histogram_of_Residuals <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = paste0("Fourier 3 histogram of residuals, ", time_series_name))

    Best_Actual_vs_Predicted <-
      ggplot2::ggplot(fabletools::augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Fourier 3 Actual vs Predicted, ", time_series_name)) +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals <-
      ggplot2::ggplot(fabletools::augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Fourier 3 Actual vs Residuals, ", time_series_name)) +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (time_series_results[1, 1] == "Fourier4") {
    Best_Model <- time_series_data %>%
      fabletools::model(
        Fourier4 = ARIMA((Value) ~ fourier(K = 4) + PDQ(0, 0, 0))
      )

    Best_Forecast <- Best_Model %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast <- gt::gt(data = Best_Forecast, caption = paste0("Best Forecast using Fourier 4, ", time_series_name)) %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot <- Best_Model %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::labs(title = paste0("Fourier 4 model forecast of value, ", time_series_data)) +
      ggplot2::ylab("Total value")

    Best_Innovation_Residuals <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Period, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = paste0("Fourier 4 innovation residuals, ", time_series_name))

    Best_ACF <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = paste0("Fourier 4 Autocorrelation function, ", time_series_name))

    Best_Histogram_of_Residuals <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = paste0("Fourier 4 histogram of residuals, ", time_series_name))

    Best_Actual_vs_Predicted <-
      ggplot2::ggplot(fabletools::augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Fourier 4 Actual vs Predicted, ", time_series_name)) +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals <-
      ggplot2::ggplot(fabletools::augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Fourier 4 Actual vs Residuals, ", time_series_name)) +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (time_series_results[1, 1] == "Fourier5") {
    Best_Model <- time_series_data %>%
      fabletools::model(
        Fourier5 = ARIMA((Value) ~ fourier(K = 5) + PDQ(0, 0, 0))
      )

    Best_Forecast <- Best_Model %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast <- gt::gt(data = Best_Forecast, caption = "Best Forecast using Fourier 5") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot <- Best_Model %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::labs(title = paste0("Fourier 5 model forecast of value, ", time_series_name)) +
      ggplot2::ylab("Total value")

    Best_Innovation_Residuals <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Period, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = paste0("Fourier 5 innovation residuals, ", time_series_name))

    Best_ACF <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = paste0("Fourier 5 Autocorrelation function, ", time_series_name))

    Best_Histogram_of_Residuals <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = paste0("Fourier 5 histogram of residuals, ", time_series_name))

    Best_Actual_vs_Predicted <-
      ggplot2::ggplot(fabletools::augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Fourier 5 Actual vs Predicted, ", time_series_name)) +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals <-
      ggplot2::ggplot(fabletools::augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Fourier 5 Actual vs Residuals, ", time_series_name)) +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (time_series_results[1, 1] == "Fourier6") {
    Best_Model <- time_series_data %>%
      fabletools::model(
        Fourier6 = ARIMA((Value) ~ fourier(K = 6) + PDQ(0, 0, 0))
      )

    Best_Forecast <- Best_Model %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast <- gt::gt(data = Best_Forecast, caption = paste0("Best Forecast using Fourier 6, ", time_series_name)) %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot <- Best_Model %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::labs(title = paste0("Fourier 6 model forecast of value, ", time_series_name)) +
      ggplot2::ylab("Total value")

    Best_Innovation_Residuals <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Period, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = paste0("Fourier 6 innovation residuals, ", time_series_name))

    Best_ACF <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = paste0("Fourier 6 Autocorrelation function, ", time_series_name))

    Best_Histogram_of_Residuals <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = paste0("Fourier 6 histogram of residuals, ", time_series_name))

    Best_Actual_vs_Predicted <-
      ggplot2::ggplot(fabletools::augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Fourier 6 Actual vs Predicted, ", time_series_name)) +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals <-
      ggplot2::ggplot(fabletools::augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Fourier 6 Actual vs Residuals, ", time_series_name)) +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (time_series_results[1, 1] == "Holt_Winters_Additive") {
    Best_Model <- time_series_data %>%
      fabletools::model(
        Holt_Winters_Additive = ETS(Value ~ error("A") + trend("A") + season("A"))
      )

    Best_Forecast <- Best_Model %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast <- gt::gt(data = Best_Forecast, caption = paste0("Best Forecast using Holt-Winters Additive, ", time_series_name)) %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot <- Best_Model %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::labs(title = paste0("Holt-Winters Additive model forecast of value, ", time_series_name)) +
      ggplot2::ylab("Total value")

    Best_Innovation_Residuals <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Period, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = paste0("Holt-Winters Additive innovation residuals, ", time_series_name))

    Best_ACF <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = paste0("Holt-Winters Additive Autocorrelation function, ", time_series_name))

    Best_Histogram_of_Residuals <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = paste0("Holt-Winters Additive histogram of residuals, ", time_series_name))

    Best_Actual_vs_Predicted <-
      ggplot2::ggplot(fabletools::augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Holt-Winters Additive Actual vs Predicted, ", time_series_name)) +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals <-
      ggplot2::ggplot(fabletools::augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Holt-Winters Additive Actual vs Residuals, ", time_series_name)) +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (time_series_results[1, 1] == "Holt_Winters_Multiplicative") {
    Best_Model <- time_series_data %>%
      fabletools::model(
        Holt_Winters_Multiplicative = ETS(Value ~ error("M") + trend("A") + season("M"))
      )

    Best_Forecast <- Best_Model %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast <- gt::gt(data = Best_Forecast, caption = paste0("Best Forecast using Holt-Winters Multiplicative, ", time_series_name)) %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot <- Best_Model %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::labs(title = paste0("Holt-Winters Multiplicative model forecast of value, ", time_series_name)) +
      ggplot2::ylab("Total value")

    Best_Innovation_Residuals <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Period, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = paste0("Holt-Winters Multiplicative innovation residuals, ", time_series_name))

    Best_ACF <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = paste0("Holt-Winters Multiplicative Autocorrelation function, ", time_series_name))

    Best_Histogram_of_Residuals <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = paste0("Holt-Winters Multiplicative histogram of residuals, ", time_series_name))

    Best_Actual_vs_Predicted <-
      ggplot2::ggplot(fabletools::augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Holt-Winters Multiplicative Actual vs Predicted, ", time_series_name)) +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals <-
      ggplot2::ggplot(fabletools::augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Holt-Winters Multiplicative Actual vs Residuals, ", time_series_name)) +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (time_series_results[1, 1] == "Holt_Winters_Damped") {
    Best_Model <- time_series_data %>%
      fabletools::model(
        Holt_Winters_Damped = ETS(Value ~ error("M") + trend("Ad") + season("M"))
      )

    Best_Forecast <- Best_Model %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast <- gt::gt(data = Best_Forecast, caption = paste0("Best Forecast using Holt-Winters Damped, ", time_series_name)) %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot <- Best_Model %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::labs(title = paste0("Holt-Winters Damped model forecast of value, ", time_series_name)) +
      ggplot2::ylab("Total value")

    Best_Innovation_Residuals <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Period, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = paste0("Holt-Winters Damped innovation residuals, ", time_series_name))

    Best_ACF <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = paste0("Holt-Winters Damped Autocorrelation function, ", time_series_name))

    Best_Histogram_of_Residuals <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = paste0("Holt-Winters Damped histogram of residuals, ", time_series_name))

    Best_Actual_vs_Predicted <-
      ggplot2::ggplot(fabletools::augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Holt-Winters Damped Actual vs Predicted, ", time_series_name)) +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals <-
      ggplot2::ggplot(fabletools::augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Holt-Winters Damped Actual vs Residuals, ", time_series_name)) +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (time_series_results[1, 1] == "Linear1") {
    Best_Model <- time_series_data %>%
      fabletools::model(
        Linear1 = TSLM(Value ~ season() + trend())
      )

    Best_Forecast <- Best_Model %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast <- gt::gt(data = Best_Forecast, caption = "Best Forecast using Linear 1") %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot <- Best_Model %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::labs(title = paste0("Linear 1 model forecast of value, ", time_series_name)) +
      ggplot2::ylab("Total value")

    Best_Innovation_Residuals <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Period, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = paste0("Linear 1 innovation residuals, ", time_series_name))

    Best_ACF <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = paste0("Linear 1 Autocorrelation function, ", time_series_name))

    Best_Histogram_of_Residuals <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = paste0("Linear 1 histogram of residuals, ", time_series_name))

    Best_Actual_vs_Predicted <-
      ggplot2::ggplot(fabletools::augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Linear 1 Actual vs Predicted, ", time_series_name)) +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals <-
      ggplot2::ggplot(fabletools::augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Linear 1 Actual vs Residuals, ", time_series_name)) +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (time_series_results[1, 1] == "Linear2") {
    Best_Model <- time_series_data %>%
      fabletools::model(
        Linear2 = TSLM(Value)
      )

    Best_Forecast <- Best_Model %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast <- gt::gt(data = Best_Forecast, caption = paste0("Best Forecast using Linear 2, ", time_series_name)) %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot <- Best_Model %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::labs(title = paste0("Linear 2 model forecast of value, ", time_series_name)) +
      ggplot2::ylab("Total value")

    Best_Innovation_Residuals <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Period, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = paste0("Linear 2 innovation residuals, ", time_series_name))

    Best_ACF <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = paste0("Linear 2 Autocorrelation function, ", time_series_name))

    Best_Histogram_of_Residuals <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = paste0("Linear 2 histogram of residuals, ", time_series_name))

    Best_Actual_vs_Predicted <-
      ggplot2::ggplot(fabletools::augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Linear 2 Actual vs Predicted, ", time_series_name)) +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals <-
      ggplot2::ggplot(fabletools::augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Linear 2 Actual vs Residuals, ", time_series_name)) +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (time_series_results[1, 1] == "Linear3") {
    Best_Model <- time_series_data %>%
      fabletools::model(
        Linear3 = TSLM(Value ~ season())
      )

    Best_Forecast <- Best_Model %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast <- gt::gt(data = Best_Forecast, caption = paste0("Best Forecast using Linear 3, ", time_series_name)) %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot <- Best_Model %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::labs(title = paste0("Linear 3 model forecast of value, ", time_series_name)) +
      ggplot2::ylab("Total value")

    Best_Innovation_Residuals <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Period, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = paste0("Linear 3 innovation residuals, ", time_series_name))

    Best_ACF <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = paste0("Linear 3 Autocorrelation function, ", time_series_name))

    Best_Histogram_of_Residuals <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = paste0("Linear 3 histogram of residuals, ", time_series_name))

    Best_Actual_vs_Predicted <-
      ggplot2::ggplot(fabletools::augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Linear 3 Actual vs Predicted, ", time_series_name)) +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals <-
      ggplot2::ggplot(fabletools::augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Linear 3 Actual vs Residuals, ", time_series_name)) +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (time_series_results[1, 1] == "Linear4") {
    Best_Model <- time_series_data %>%
      fabletools::model(
        Linear4 = TSLM(Value ~ trend())
      )

    Best_Forecast <- Best_Model %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast <- gt::gt(data = Best_Forecast, caption = paste0("Best Forecast using Linear 4, ", time_series_name)) %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot <- Best_Model %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::labs(title = paste0("Linear 4 model forecast of value, ", time_series_name)) +
      ggplot2::ylab("Total value")

    Best_Innovation_Residuals <- fabletools::augment(Best_Model) %>%
      ggplot2::ggplot(aes(x = Period, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = paste0("Linear 4 innovation residuals, ", time_series_name))

    Best_ACF <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = paste0("Linear 4 Autocorrelation function, ", time_series_name))

    Best_Histogram_of_Residuals <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = paste0("Linear 4 histogram of residuals, ", time_series_name))

    Best_Actual_vs_Predicted <-
      ggplot2::ggplot(fabletools::augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Linear 4 Actual vs Predicted, ", time_series_name)) +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals <-
      ggplot2::ggplot(fabletools::augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Linear 4 Actual vs Residuals, ", time_series_name)) +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  if (time_series_results[1, 1] == "Stochastic") {
    Best_Model <- time_series_data %>%
      fabletools::model(
        Stochastic = ARIMA(Value ~ pdq(d = 1), stepwise = TRUE, greedy = TRUE, approximation = TRUE)
      )

    Best_Forecast <- Best_Model %>%
      fabletools::forecast(h = number_of_intervals_to_forecast)

    Best_Forecast <- gt::gt(data = Best_Forecast, caption = paste0("Best Forecast using Stochastic, ", time_series_name)) %>%
      gt::fmt_number(columns = c(".mean"), decimals = 0, use_seps = TRUE)

    Best_Forecast_plot <- Best_Model %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      feasts::autoplot(time_series_test) +
      ggplot2::labs(title = paste0("Stochastic model forecast of value, ", time_series_name)) +
      ggplot2::ylab("Total value")

    Best_Innovation_Residuals <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = Period, y = .innov)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::labs(title = paste0("Stochastic innovation residuals, ", time_series_name))

    Best_ACF <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      feasts::ACF(.innov) %>%
      feasts::autoplot() +
      ggplot2::labs(title = paste0("Stochastic Autocorrelation function, ", time_series_name))

    Best_Histogram_of_Residuals <- fabletools::augment(Best_Model) %>%
      drop_na() %>%
      ggplot2::ggplot(aes(x = .resid)) +
      ggplot2::geom_histogram(bins = round(nrow(time_series_data) / 5)) +
      ggplot2::geom_vline(xintercept = 0, color = "red") +
      ggplot2::labs(title = paste0("Stochastic histogram of residuals, ", time_series_name))

    Best_Actual_vs_Predicted <-
      ggplot2::ggplot(fabletools::augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Stochastic Actual vs Predicted, ", time_series_name)) +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted")

    Best_Actual_vs_Residuals <-
      ggplot2::ggplot(fabletools::augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Stochastic Actual vs Residuals, ", time_series_name)) +
      ggplot2::geom_hline(yintercept = 0, color = "red") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Residuals")
  }

  return(list(
    "Plot of value" = plot_of_value, "Plot of individual periods" = plot_of_individual_periods, "Head of the data" = table_of_value_head, "Tail of the data" = table_of_value_tail, "Plot of trend" = plot_of_trend,
    "Plot of seasonally adjusted" = plot_of_seasonally_adjusted, "Plot of decomposition" = plot_of_decomposition, "Plot of anomalies" = time_series_anomalies,
    "Plot of subseasons" = plot_of_subseasons, "Plot of multiple lags" = plot_of_multiple_lags, "All time series features" = all_time_series_features,
    "Time series quartiles" = time_series_quartiles, "Best model" = Best_Model, "Best forecast" = Best_Forecast, "Best forecast plot" = Best_Forecast_plot,
    "Best innovation residuals" = Best_Innovation_Residuals, "Best ACF" = Best_ACF, "Best histogram of residuals" = Best_Histogram_of_Residuals,
    "Best actual vs predicted" = Best_Actual_vs_Predicted, "Best actual vs residuals" = Best_Actual_vs_Residuals, "Forecast accuracy" = forecast_accuracy_table,
    "Baseline data" = Baseline_full
  )
  )
}
