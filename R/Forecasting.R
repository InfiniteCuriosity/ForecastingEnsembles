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
#' @importFrom distributional dist_sample
#' @importFrom doParallel registerDoParallel
#' @importFrom dplyr arrange bind_rows count filter mutate select summarise
#' @importFrom fable ARIMA ETS MEAN NAIVE NNETAR RW SNAIVE TSLM
#' @importFrom fabletools accuracy as_fable as_tsibble augment autoplot components features model report point_accuracy_measures
#' @importFrom fable.prophet prophet
#' @importFrom feasts gg_tsresiduals STL
#' @importFrom fracdiff fracdiff
#' @importFrom ggplot2 aes facet_grid geom_line geom_hline geom_abline geom_point ggplot ggtitle guides labs scale_x_continuous scale_y_continuous theme xlab ylab
#' @importFrom gt gt tab_header fmt_number fmt_percent
#' @importFrom lubridate Date
#' @importFrom magrittr %>%
#' @importFrom parallel makeCluster
#' @importFrom readr read_csv
#' @importFrom scales comma
#' @importFrom stats dist stl AIC BIC lag sd quantile
#' @importFrom tibble tibble as_tibble
#' @importFrom tidyr pivot_longer drop_na
#' @importFrom tsibble tsibble
#' @importFrom urca ur.kpss
#' @importFrom utils tail head

Forecasting <- function(time_series_name, time_series_data, train_amount, number_of_intervals_to_forecast, use_parallel = c("Y", "N"), time_interval = c("Q", "M", "W")) {
    use_parallel <- 0
    no_cores <- 0

    if (use_parallel == "Y") {
      cl <- parallel::makeCluster(no_cores, type = "FORK")
      doParallel::registerDoParallel(cl)
    }

    all_time_series_features <- 0
    Label <- 0
    Value <- 0
    Month <- 0
    trend <- 0
    season_adjust <- 0
    time_series_decomposition <- 0
    table_of_value_head <- 0
    table_of_value_tail <- 0
    forecast_accuracy <- 0
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
    ME <- 0
    MAE <- 0
    MPE <- 0
    MAPE <- 0
    .resid <- 0
    .fitted <- 0
    .mean <- 0
    .innov <- 0
    Difference <- 0
    Period <- 0
    Arima5 <- 0
    Arima6 <- 0

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

    if (use_parallel == "Y") {
      cl <- parallel::makeCluster(no_cores, type = "FORK")
      doParallel::registerDoParallel(cl)
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


    #### Quartiles ####
    time_series_quartiles <- time_series_data %>%
      fabletools::features(Value, quantile, prob = seq(0, 1, 0.25))

    time_series_quartiles <- gt::gt(time_series_quartiles) %>%
      gt::tab_header(
        title = c("Time series quartiles", time_series_name)
      )

    #########

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

    # <----- Tail of Table of Value ---------------------------------------------> ####

    table_of_value_tail <- gt::gt(tail(time_series_data[, c(1:2)]),
                                  caption = paste0("Tail (most recent) of total value data set, ", time_series_name)
    ) %>%
      gt::fmt_number(columns = c("Value"), decimals = 4, use_seps = TRUE)

    # <----- Head of Table of Value ---------------------------------------------> ####

    table_of_value_head <- gt::gt(head(time_series_data[, c(1:2)]),
                                  caption = paste0("Head (beginning) of total value data set, ", time_series_name)
    ) %>%
      gt::fmt_number(columns = c("Value"), decimals = 4, use_seps = TRUE)

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

    # <--- 34 individual models to predict Value ----> ####

    Time_Series_train <- time_series_data[1:round(train_amount*nrow(time_series_data)), ]
    Time_Series_test <- time_series_data[nrow(Time_Series_train) +1 : (nrow(time_series_data)- nrow(Time_Series_train)), ]

    fit <- Time_Series_train %>%
      fabletools::model(
        Linear1 = fable::TSLM(Value ~ season() + trend()),
        Linear2 = fable::TSLM(Value),
        Linear3 = fable::TSLM(Value ~ season()),
        Linear4 = fable::TSLM(Value ~ trend()),
        Arima1 = fable::ARIMA(Value ~ season() + trend(),stepwise = TRUE, greedy = TRUE, approximation = TRUE),
        Arima2 = fable::ARIMA(Value ~ season(),stepwise = TRUE, greedy = TRUE, approximation = TRUE),
        Arima3 = fable::ARIMA(Value ~ trend(),stepwise = TRUE, greedy = TRUE, approximation = TRUE),
        Arima4 = fable::ARIMA(Value),
        Deterministic = fable::ARIMA(Value ~  1 + pdq(d = 0), stepwise = TRUE, greedy = TRUE, approximation = TRUE),
        Stochastic = fable::ARIMA(Value ~ pdq(d = 1), stepwise = TRUE, greedy = TRUE, approximation = TRUE),
        Ets1 = fable::ETS(Value ~ season() + trend()),
        Ets2 = fable::ETS(Value ~ trend()),
        Ets3 = fable::ETS(Value ~ season()),
        Ets4 = fable::ETS(Value),
        Holt_Winters_Additive = fable::ETS(Value ~ error("A") + trend("A") + season("A")),
        Holt_Winters_Multiplicative = fable::ETS(Value ~ error("M") + trend("A") + season("M")),
        Holt_Winters_Damped = fable::ETS(Value ~ error("M") + trend("Ad") + season("M")),
        Fourier1 = fable::ARIMA(Value ~ fourier(K = 1) + PDQ(0,0,0)),
        Fourier2 = fable::ARIMA(Value ~ fourier(K = 2) + PDQ(0,0,0)),
        Fourier3 = fable::ARIMA(Value ~ fourier(K = 3) + PDQ(0,0,0)),
        Fourier4 = fable::ARIMA(Value ~ fourier(K = 4) + PDQ(0,0,0)),
        Fourier5 = fable::ARIMA(Value ~ fourier(K = 5) + PDQ(0,0,0)),
        Fourier6 = fable::ARIMA(Value ~ fourier(K = 6) + PDQ(0,0,0)),
        Prophet_Additive = fable.prophet::prophet(Value ~ season(period = 12, type = "additive")),
        Prophet_Multiplicative = fable.prophet::prophet(Value ~ season(period = 12, type = "multiplicative")),
        NeuralNet1 = fable::NNETAR(Value),
        NeuralNet2 = fable::NNETAR(Value ~ season()),
        NeuralNet3 = fable::NNETAR(Value ~ trend()),
        NeuralNet4 = fable::NNETAR(Value ~ season() + trend()),
        VAR1 = fable::VAR(Value),
        Mean = fable::MEAN(Value),
        Naive = fable::NAIVE(Value),
        SNaive = fable::SNAIVE(Value),
        Drift = fable::SNAIVE(Value ~ drift())
      )

    value_forecast_accuracy <- fit %>%
      fabletools::forecast(h = number_of_intervals_to_forecast) %>%
      fabletools::accuracy(Time_Series_test, measures = fabletools::point_accuracy_measures[1:5]) %>%
      dplyr::arrange(RMSE)

    value_forecast_report <- gt::gt(value_forecast_accuracy, caption = "Time series forecast accuracy, sorted by Root Mean Squared Error (RMSE)")


    # 5. Diagnostics plots of the best model for value ------------------------

    # Arima1 Results ----------------------------------------------------------

    if(value_forecast_accuracy[1,1] == "Arima1"){

      Best_Model <- time_series_data %>%
        fabletools::model(
          Arima1 = fable::ARIMA(Value ~ season() + trend(),stepwise = TRUE, greedy = TRUE, approximation = TRUE)
        )

      augment(Best_Model) %>% drop_na() %>% drop_na()

      Best_Forecast <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast)

      Best_Forecast_plot <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast) %>%
        autoplot(Time_Series_test) +
        labs(title = "Arima 1 model three month forecast of value")+
        scale_y_continuous(labels = scales::comma) +
        ylab("Total value")

      Best_Forecast_Min <-
        Best_Forecast$.mean - value_forecast_accuracy[1,4]

      Best_Forecast_Max <-
        Best_Forecast$.mean + value_forecast_accuracy[1,4]

      Best_STL_Decomposition <-
        time_series_data[2:nrow(time_series_data),] %>%
        fabletools::model(
          stl = STL(Value)
        ) %>%
        components()

      Best_STL_Decomposition_autoplot <- Best_STL_Decomposition %>% autoplot()

      Best_Innovation_Residuals <- augment(Best_Model) %>% drop_na() %>%
        autoplot(.innov) +
        geom_point() +
        geom_hline(yintercept = 0, color = "red") +
        labs(title = "Arima 1 innovation residuals")

      Best_ACF <- augment(Best_Model) %>% drop_na() %>%
        feasts::ACF(.innov) %>%
        autoplot() +
        labs(title = "Arima 1 Autocorrelation function")

      Best_Histogram_of_Residuals <- augment(Best_Model) %>% drop_na() %>%
        ggplot(aes(x = .resid)) +
        ggplot2::geom_histogram(bins = round(nrow(time_series_data)/5)) +
        ggplot2::geom_vline(xintercept = 0, color = "red") +
        labs(title = "Arima 1 histogram of residuals")

      Best_Actual_vs_Predicted <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
        geom_point() +
        ggtitle("Arima1 Actual vs Predicted") +
        geom_abline(slope = 1, intercept = 0, color = "red") +
        xlab("Actual") +
        ylab("Predicted")

      Best_Actual_vs_Residuals <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
        geom_point() +
        ggtitle("Arima1 Actual vs Residuals") +
        geom_hline(yintercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Residuals")

      Best_Actual_vs_Trend <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = trend)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = trend), color = "red") +
        ggtitle("Arima1 Actual (black) vs trend (red)")+
        xlab("Actual") +
        ylab("Trend")

      Best_Actual_vs_Seasonally_Adjusted <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = season_adjust)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = season_adjust), color = "red") +
        ggtitle("Arima1 Actual (black) vs seasonally adjusted (red)")+
        xlab("Actual") +
        ylab("Seasonally Adjusted value")
    }

    # Arima2 Results ----------------------------------------------------------

    if(value_forecast_accuracy[1,1] == "Arima2"){

      Best_Model <- time_series_data %>%
        model(
          Arima2 = fable::ARIMA(Value ~ season(),stepwise = TRUE, greedy = TRUE, approximation = TRUE)
        )

      augment(Best_Model) %>% drop_na()

      Best_Forecast <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast)

      Best_Forecast_plot <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast) %>%
        autoplot(time_series_data) +
        labs(title = "Arima 2 model three month forecast of value")+
        scale_y_continuous(labels = scales::comma) +
        ylab("Value")

      Best_Forecast_Min <-
        Best_Forecast$.mean - value_forecast_accuracy[1,4]

      Best_Forecast_Max <-
        Best_Forecast$.mean + value_forecast_accuracy[1,4]

      Best_STL_Decomposition <-
        time_series_data[2:nrow(time_series_data),] %>%
        fabletools::model(
          stl = STL(Value)
        ) %>%
        components()

      Best_STL_Decomposition_autoplot <- Best_STL_Decomposition %>% autoplot()

      Best_Innovation_Residuals <- augment(Best_Model) %>% drop_na() %>%
        autoplot(.innov) +
        geom_point() +
        geom_hline(yintercept = 0, color = "red") +
        labs(title = "Arima 2 innovation residuals")

      Best_ACF <- augment(Best_Model) %>% drop_na() %>%
        feasts::ACF(.innov) %>%
        autoplot() +
        labs(title = "Arima 2 Autocorrelation function")

      Best_Histogram_of_Residuals <- augment(Best_Model) %>% drop_na() %>%
        ggplot(aes(x = .resid)) +
        ggplot2::geom_histogram(bins = round(nrow(time_series_data)/5)) +
        ggplot2::geom_vline(xintercept = 0, color = "red") +
        labs(title = "Arima 2 histogram of residuals")

      Best_Actual_vs_Predicted <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
        geom_point() +
        ggtitle("Arima2 Actual vs Predicted") +
        geom_abline(slope = 1, intercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Predicted")

      Best_Actual_vs_Residuals <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
        geom_point() +
        ggtitle("Arima2 Actual vs Residuals") +
        geom_hline(yintercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Residuals")

      Best_Actual_vs_Trend <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = trend)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = trend), color = "red") +
        ggtitle("Arima2 Actual (black) vs trend (red)")+
        xlab("Actual") +
        ylab("Trend")

      Best_Actual_vs_Seasonally_Adjusted <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = season_adjust)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = season_adjust), color = "red") +
        ggtitle("Arima2 Actual (black) vs seasonally adjusted (red)")+
        xlab("Actual") +
        ylab("Seasonally Adjusted value")
    }

    # Arima3 Results ----------------------------------------------------------

    if(value_forecast_accuracy[1,1] == "Arima3"){

      Best_Model <- time_series_data %>%
        fabletools::model(
          Arima3 = fable::ARIMA(Value ~ trend(),stepwise = TRUE, greedy = TRUE, approximation = TRUE)
        )

      augment(Best_Model) %>% drop_na()

      Best_Forecast <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast)

      Best_Forecast_plot <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast) %>%
        autoplot(Time_Series_test) +
        labs(title = "Arima 3 model three month forecast of value")+
        scale_y_continuous(labels = scales::comma) +
        ylab("Total value")

      Best_Forecast_Min <-
        Best_Forecast$.mean - value_forecast_accuracy[1,4]

      Best_Forecast_Max <-
        Best_Forecast$.mean + value_forecast_accuracy[1,4]

      Best_STL_Decomposition <-
        time_series_data[2:nrow(time_series_data),] %>%
        fabletools::model(
          stl = STL(Value)
        ) %>%
        components()

      Best_STL_Decomposition_autoplot <- Best_STL_Decomposition %>% autoplot()

      Best_Innovation_Residuals <- augment(Best_Model) %>% drop_na() %>%
        autoplot(.innov) +
        geom_point() +
        geom_hline(yintercept = 0, color = "red") +
        labs(title = "Arima 3 innovation residuals")

      Best_ACF <- augment(Best_Model) %>% drop_na() %>%
        feasts::ACF(.innov) %>%
        autoplot() +
        labs(title = "Arima 3 Autocorrelation function")

      Best_Histogram_of_Residuals <- augment(Best_Model) %>% drop_na() %>%
        ggplot(aes(x = .resid)) +
        ggplot2::geom_histogram(bins = round(nrow(time_series_data)/5)) +
        ggplot2::geom_vline(xintercept = 0, color = "red") +
        labs(title = "Arima 3 histogram of residuals")

      Best_Actual_vs_Predicted <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
        geom_point() +
        ggtitle("Arima3 Actual vs Predicted") +
        geom_abline(slope = 1, intercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Predicted")

      Best_Actual_vs_Residuals <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
        geom_point() +
        ggtitle("Arima3 Actual vs Residuals") +
        geom_hline(yintercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Residuals")

      Best_Actual_vs_Trend <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = trend)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = trend), color = "red") +
        ggtitle("Arima3 Actual (black) vs trend (red)")+
        xlab("Actual") +
        ylab("Trend")

      Best_Actual_vs_Seasonally_Adjusted <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = season_adjust)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = season_adjust), color = "red") +
        ggtitle("Arima3 Actual (black) vs seasonally adjusted (red)")+
        xlab("Actual") +
        ylab("Seasonally adjusted value")
    }

    # Arima4 Results ----------------------------------------------------------

    if(value_forecast_accuracy[1,1] == "Arima4"){

      Best_Model <- time_series_data %>%
        fabletools::model(
          Arima4 = fable::ARIMA(Value)
        )

      augment(Best_Model) %>% drop_na()

      Best_Forecast <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast)

      Best_Forecast_plot <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast) %>%
        autoplot(Time_Series_test) +
        labs(title = "Arima 4 model three month forecast of total values")+
        scale_y_continuous(labels = scales::comma) +
        ylab("Total values")

      Best_Forecast_Min <-
        Best_Forecast$.mean - value_forecast_accuracy[1,4]

      Best_Forecast_Max <-
        Best_Forecast$.mean + value_forecast_accuracy[1,4]

      Best_STL_Decomposition <-
        time_series_data[2:nrow(time_series_data),] %>%
        fabletools::model(
          stl = STL(Value)
        ) %>%
        components()

      Best_STL_Decomposition_autoplot <- Best_STL_Decomposition %>% autoplot()

      Best_Innovation_Residuals <- augment(Best_Model) %>% drop_na() %>%
        autoplot(.innov) +
        geom_point() +
        geom_hline(yintercept = 0, color = "red") +
        labs(title = "Arima 4 innovation residuals")

      Best_ACF <- augment(Best_Model) %>% drop_na() %>%
        feasts::ACF(.innov) %>%
        autoplot() +
        labs(title = "Arima 4 Autocorrelation function")

      Best_Histogram_of_Residuals <- augment(Best_Model) %>% drop_na() %>%
        ggplot(aes(x = .resid)) +
        ggplot2::geom_histogram(bins = round(nrow(time_series_data)/5)) +
        ggplot2::geom_vline(xintercept = 0, color = "red") +
        labs(title = "Arima 4 histogram of residuals")

      Best_Actual_vs_Predicted <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
        geom_point() +
        ggtitle("Arima4 Actual vs Predicted") +
        geom_abline(slope = 1, intercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Predicted")

      Best_Actual_vs_Residuals <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
        geom_point() +
        ggtitle("Arima4 Actual vs Residuals") +
        geom_hline(yintercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Residuals")

      Best_Actual_vs_Trend <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = trend)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = trend), color = "red") +
        ggtitle("Arima4 Actual (black) vs trend (red)")+
        xlab("Actual") +
        ylab("Trend")

      Best_Actual_vs_Seasonally_Adjusted <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = season_adjust)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = season_adjust), color = "red") +
        ggtitle("Arima4 Actual (black) vs seasonally adjusted (red)")+
        xlab("Actual") +
        ylab("Seasonally adjusted value")
    }

    # Deterministic Results ---------------------------------------------------

    if(value_forecast_accuracy[1,1] == "Deterministic"){

      Best_Model <- time_series_data %>%
        fabletools::model(
          Deterministic = fable::ARIMA(Value ~  1 + pdq(d = 0), stepwise = TRUE, greedy = TRUE, approximation = TRUE)
        )

      augment(Best_Model) %>% drop_na()

      Best_Forecast <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast)

      Best_Forecast_plot <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast) %>%
        autoplot(Time_Series_test) +
        labs(title = "Deterministic model three month forecast of total values")+
        scale_y_continuous(labels = scales::comma) +
        ylab("Total values")

      Best_Forecast_Min <-
        Best_Forecast$.mean - value_forecast_accuracy[1,4]

      Best_Forecast_Max <-
        Best_Forecast$.mean + value_forecast_accuracy[1,4]

      Best_STL_Decomposition <-
        time_series_data[2:nrow(time_series_data),] %>%
        fabletools::model(
          stl = STL(Value)
        ) %>%
        components()

      Best_STL_Decomposition_autoplot <- Best_STL_Decomposition %>% autoplot()

      Best_Innovation_Residuals <- augment(Best_Model) %>% drop_na() %>%
        autoplot(.innov) +
        geom_point() +
        geom_hline(yintercept = 0, color = "red") +
        labs(title = "Deterministic innovation residuals")

      Best_ACF <- augment(Best_Model) %>% drop_na() %>%
        feasts::ACF(.innov) %>%
        autoplot() +
        labs(title = "Deterministic Autocorrelation function")

      Best_Histogram_of_Residuals <- augment(Best_Model) %>% drop_na() %>%
        ggplot(aes(x = .resid)) +
        ggplot2::geom_histogram(bins = round(nrow(time_series_data)/5)) +
        ggplot2::geom_vline(xintercept = 0, color = "red") +
        labs(title = "Deterministic histogram of residuals")

      Best_Actual_vs_Predicted <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
        geom_point() +
        ggtitle("Deterministic Actual vs Predicted") +
        geom_abline(slope = 1, intercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Predicted")

      Best_Actual_vs_Residuals <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
        geom_point() +
        ggtitle("Deterministic Actual vs Residuals") +
        geom_hline(yintercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Residuals")

      Best_Actual_vs_Trend <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = trend)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = trend), color = "red") +
        ggtitle("Deterministic Actual (black) vs trend (red)")+
        xlab("Actual") +
        ylab("Trend")

      Best_Actual_vs_Seasonally_Adjusted <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = season_adjust)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = season_adjust), color = "red") +
        ggtitle("Deterministic Actual (black) vs seasonally adjusted (red)")+
        xlab("Actual") +
        ylab("Seasonally adjusted value")
    }

    # Drift Results ---------------------------------------------------

    if(value_forecast_accuracy[1,1] == "Drift"){

      Best_Model <- time_series_data %>%
        fabletools::model(
          Drift = fable::SNAIVE(Value ~ drift())
        )

      augment(Best_Model) %>% drop_na()

      Best_Forecast <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast)

      Best_Forecast_plot <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast) %>%
        autoplot(Time_Series_test) +
        labs(title = "Drift model three month forecast of total values")+
        scale_y_continuous(labels = scales::comma) +
        ylab("Total values")

      Best_Forecast_Min <-
        Best_Forecast$.mean - value_forecast_accuracy[1,4]

      Best_Forecast_Max <-
        Best_Forecast$.mean + value_forecast_accuracy[1,4]

      Best_STL_Decomposition <-
        time_series_data[2:nrow(time_series_data),] %>%
        fabletools::model(
          stl = STL(Value)
        ) %>%
        components()

      Best_STL_Decomposition_autoplot <- Best_STL_Decomposition %>% autoplot()

      Best_Innovation_Residuals <- augment(Best_Model) %>% drop_na() %>% drop_na() %>%
        autoplot(.innov) +
        geom_point() +
        geom_hline(yintercept = 0, color = "red") +
        labs(title = "Drift innovation residuals")

      Best_ACF <- augment(Best_Model) %>% drop_na() %>%
        feasts::ACF(.innov) %>%
        autoplot() +
        labs(title = "Drift Autocorrelation function")

      Best_Histogram_of_Residuals <- augment(Best_Model) %>% drop_na() %>%
        ggplot(aes(x = .resid)) +
        ggplot2::geom_histogram(bins = round(nrow(time_series_data)/5)) +
        ggplot2::geom_vline(xintercept = 0, color = "red") +
        labs(title = "Drift histogram of residuals")

      Best_Actual_vs_Predicted <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
        geom_point() +
        ggtitle("Drift Actual vs Predicted") +
        geom_abline(slope = 1, intercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Predicted")

      Best_Actual_vs_Residuals <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
        geom_point() +
        ggtitle("Drift Actual vs Residuals") +
        geom_hline(yintercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Residuals")

      Best_Actual_vs_Trend <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = trend)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = trend), color = "red") +
        ggtitle("Drift Actual (black) vs trend (red)")+
        xlab("Actual") +
        ylab("Trend")

      Best_Actual_vs_Seasonally_Adjusted <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = season_adjust)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = season_adjust), color = "red") +
        ggtitle("Drift Actual (black) vs seasonally adjusted (red)")+
        xlab("Actual") +
        ylab("Seasonally adjusted value")
    }

    # Ets1 Results ------------------------------------------------------------

    if(value_forecast_accuracy[1,1] == "Ets1"){

      Best_Model <- time_series_data %>%
        fabletools::model(
          Ets1 = fable::ETS(Value ~ season() + trend())
        )

      augment(Best_Model) %>% drop_na()

      Best_Forecast <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast)

      Best_Forecast_plot <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast) %>%
        autoplot(Time_Series_test) +
        labs(title = "ETS 1 model three month forecast of total values")+
        scale_y_continuous(labels = scales::comma) +
        ylab("Total values")

      Best_Forecast_Min <-
        Best_Forecast$.mean - value_forecast_accuracy[1,4]

      Best_Forecast_Max <-
        Best_Forecast$.mean + value_forecast_accuracy[1,4]

      Best_STL_Decomposition <-
        time_series_data[2:nrow(time_series_data),] %>%
        fabletools::model(
          stl = STL(Value)
        ) %>%
        components()

      Best_STL_Decomposition_autoplot <- Best_STL_Decomposition %>% autoplot()

      Best_Innovation_Residuals <- augment(Best_Model) %>% drop_na() %>%
        autoplot(.innov) +
        geom_point() +
        geom_hline(yintercept = 0, color = "red") +
        labs(title = "Ets 1 innovation residuals")

      Best_ACF <- augment(Best_Model) %>% drop_na() %>%
        feasts::ACF(.innov) %>%
        autoplot() +
        labs(title = "Ets 1 Autocorrelation function")

      Best_Histogram_of_Residuals <- augment(Best_Model) %>% drop_na() %>%
        ggplot(aes(x = .resid)) +
        ggplot2::geom_histogram(bins = round(nrow(time_series_data)/5)) +
        ggplot2::geom_vline(xintercept = 0, color = "red") +
        labs(title = "Ets 1 histogram of residuals")

      Best_Actual_vs_Predicted <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
        geom_point() +
        ggtitle("Ets1 Actual vs Predicted") +
        geom_abline(slope = 1, intercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Predicted")

      Best_Actual_vs_Residuals <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
        geom_point() +
        ggtitle("Ets1 Actual vs Residuals") +
        geom_hline(yintercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Residuals")

      Best_Actual_vs_Trend <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = trend)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = trend), color = "red") +
        ggtitle("Ets1 Actual (black) vs trend (red)")+
        xlab("Actual") +
        ylab("Trend")

      Best_Actual_vs_Seasonally_Adjusted <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = season_adjust)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = season_adjust), color = "red") +
        ggtitle("Ets1 Actual (black) vs seasonally adjusted (red)")+
        xlab("Actual") +
        ylab("Seasonally adjusted value")
    }

    # Ets2 Results ------------------------------------------------------------

    if(value_forecast_accuracy[1,1] == "Ets2"){

      Best_Model <- time_series_data %>%
        fabletools::model(
          Ets2 = fable::ETS(Value ~ trend())
        )

      augment(Best_Model) %>% drop_na()

      Best_Forecast <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast)

      Best_Forecast_plot <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast) %>%
        autoplot(Time_Series_test) +
        labs(title = "ETS 2 model three month forecast of total values")+
        scale_y_continuous(labels = scales::comma) +
        ylab("Total values")

      Best_Forecast_Min <-
        Best_Forecast$.mean - value_forecast_accuracy[1,4]

      Best_Forecast_Max <-
        Best_Forecast$.mean + value_forecast_accuracy[1,4]

      Best_STL_Decomposition <-
        time_series_data[2:nrow(time_series_data),] %>%
        fabletools::model(
          stl = STL(Value)
        ) %>%
        components()

      Best_STL_Decomposition_autoplot <- Best_STL_Decomposition %>% autoplot()

      Best_Innovation_Residuals <- augment(Best_Model) %>% drop_na() %>%
        autoplot(.innov) +
        geom_point() +
        geom_hline(yintercept = 0, color = "red") +
        labs(title = "Ets 2 innovation residuals")

      Best_ACF <- augment(Best_Model) %>% drop_na() %>%
        feasts::ACF(.innov) %>%
        autoplot() +
        labs(title = "Ets 2 Autocorrelation function")

      Best_Histogram_of_Residuals <- augment(Best_Model) %>% drop_na() %>%
        ggplot(aes(x = .resid)) +
        ggplot2::geom_histogram(bins = round(nrow(time_series_data)/5)) +
        ggplot2::geom_vline(xintercept = 0, color = "red") +
        labs(title = "Ets 2 histogram of residuals")

      Best_Actual_vs_Predicted <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
        geom_point() +
        ggtitle("Ets2 Actual vs Predicted") +
        geom_abline(slope = 1, intercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Predicted")

      Best_Actual_vs_Residuals <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
        geom_point() +
        ggtitle("Ets2 Actual vs Residuals") +
        geom_hline(yintercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Residuals")

      Best_Actual_vs_Trend <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = trend)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = trend), color = "red") +
        ggtitle("Ets2 Actual (black) vs trend (red)")+
        xlab("Actual") +
        ylab("Trend")

      Best_Actual_vs_Seasonally_Adjusted <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = season_adjust)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = season_adjust), color = "red") +
        ggtitle("Ets2 Actual (black) vs seasonally adjusted (red)")+
        xlab("Actual") +
        ylab("Seasonally adjusted value")
    }

    # Ets3 Results ------------------------------------------------------------

    if(value_forecast_accuracy[1,1] == "Ets3"){

      Best_Model <- time_series_data %>%
        fabletools::model(
          Ets3 = fable::ETS(Value ~ season())
        )

      augment(Best_Model) %>% drop_na()

      Best_Forecast <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast)

      Best_Forecast_plot <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast) %>%
        autoplot(Time_Series_test) +
        labs(title = "ETS 3 model three month forecast of total values")+
        scale_y_continuous(labels = scales::comma) +
        ylab("Total value")

      Best_Forecast_Min <-
        Best_Forecast$.mean - value_forecast_accuracy[1,4]

      Best_Forecast_Max <-
        Best_Forecast$.mean + value_forecast_accuracy[1,4]

      Best_STL_Decomposition <-
        time_series_data[2:nrow(time_series_data),] %>%
        fabletools::model(
          stl = STL(Value)
        ) %>%
        components()

      Best_STL_Decomposition_autoplot <- Best_STL_Decomposition %>% autoplot()

      Best_Innovation_Residuals <- augment(Best_Model) %>% drop_na() %>%
        autoplot(.innov) +
        geom_point() +
        geom_hline(yintercept = 0, color = "red") +
        labs(title = "Ets 3 innovation residuals")

      Best_ACF <- augment(Best_Model) %>% drop_na() %>%
        feasts::ACF(.innov) %>%
        autoplot() +
        labs(title = "Ets 3 Autocorrelation function")

      Best_Histogram_of_Residuals <- augment(Best_Model) %>% drop_na() %>%
        ggplot(aes(x = .resid)) +
        ggplot2::geom_histogram(bins = round(nrow(time_series_data)/5)) +
        ggplot2::geom_vline(xintercept = 0, color = "red") +
        labs(title = "Ets 3 histogram of residuals")

      Best_Actual_vs_Predicted <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
        geom_point() +
        ggtitle("Ets3 Actual vs Predicted") +
        geom_abline(slope = 1, intercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Predicted")

      Best_Actual_vs_Residuals <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
        geom_point() +
        ggtitle("Ets3 Actual vs Residuals") +
        geom_hline(yintercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Residuals")

      Best_Actual_vs_Trend <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = trend)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = trend), color = "red") +
        ggtitle("Ets3 Actual (black) vs trend (red)")+
        xlab("Actual") +
        ylab("Trend")

      Best_Actual_vs_Seasonally_Adjusted <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = season_adjust)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = season_adjust), color = "red") +
        ggtitle("Ets3 Actual (black) vs seasonally adjusted (red)")+
        xlab("Actual") +
        ylab("Seasonally adjusted value")
    }

    # Ets4 Results ------------------------------------------------------------

    if(value_forecast_accuracy[1,1] == "Ets4"){

      Best_Model <- time_series_data %>%
        fabletools::model(
          Ets4 = fable::ETS(Value)
        )

      augment(Best_Model) %>% drop_na()

      Best_Forecast <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast)

      Best_Forecast_plot <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast) %>%
        autoplot(Time_Series_test) +
        labs(title = "ETS 4 model three month forecast of total values")+
        scale_y_continuous(labels = scales::comma) +
        ylab("Total values")

      Best_Forecast_Min <-
        Best_Forecast$.mean - value_forecast_accuracy[1,4]

      Best_Forecast_Max <-
        Best_Forecast$.mean + value_forecast_accuracy[1,4]

      Best_STL_Decomposition <-
        time_series_data[2:nrow(time_series_data),] %>%
        fabletools::model(
          stl = STL(Value)
        ) %>%
        components()

      Best_STL_Decomposition_autoplot <- Best_STL_Decomposition %>% autoplot()

      Best_Innovation_Residuals <- augment(Best_Model) %>% drop_na() %>%
        autoplot(.innov) +
        geom_point() +
        geom_hline(yintercept = 0, color = "red") +
        labs(title = "Ets 4 innovation residuals")

      Best_ACF <- augment(Best_Model) %>% drop_na() %>%
        feasts::ACF(.innov) %>%
        autoplot() +
        labs(title = "Ets 4 Autocorrelation function")

      Best_Histogram_of_Residuals <- augment(Best_Model) %>% drop_na() %>%
        ggplot(aes(x = .resid)) +
        ggplot2::geom_histogram(bins = round(nrow(time_series_data)/5)) +
        ggplot2::geom_vline(xintercept = 0, color = "red") +
        labs(title = "Ets 4 histogram of residuals")

      Best_Actual_vs_Predicted <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
        geom_point() +
        ggtitle("Ets4 Actual vs Predicted") +
        geom_abline(slope = 1, intercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Predicted")

      Best_Actual_vs_Residuals <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
        geom_point() +
        ggtitle("Ets4 Actual vs Residuals") +
        geom_hline(yintercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Residuals")

      Best_Actual_vs_Trend <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = trend)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = trend), color = "red") +
        ggtitle("Ets4 Actual (black) vs trend (red)")+
        xlab("Actual") +
        ylab("Trend")

      Best_Actual_vs_Seasonally_Adjusted <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = season_adjust)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = season_adjust), color = "red") +
        ggtitle("Ets4 Actual (black) vs seasonally adjusted (red)")+
        xlab("Actual") +
        ylab("Seasonally adjusted value")
    }

    # Fourier1 Results --------------------------------------------------------

    if(value_forecast_accuracy[1,1] == "Fourier1"){

      Best_Model <- time_series_data %>%
        fabletools::model(
          Fourier1 = fable::ARIMA(Value ~ fourier(K = 1) + PDQ(0,0,0))
        )

      augment(Best_Model) %>% drop_na()

      Best_Forecast <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast)

      Best_Forecast_plot <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast) %>%
        autoplot(Time_Series_test) +
        labs(title = "Fourier 1 model three month forecast of total values")+
        scale_y_continuous(labels = scales::comma) +
        ylab("Total values")

      Best_Forecast_Min <-
        Best_Forecast$.mean - value_forecast_accuracy[1,4]

      Best_Forecast_Max <-
        Best_Forecast$.mean + value_forecast_accuracy[1,4]

      Best_STL_Decomposition <-
        time_series_data[2:nrow(time_series_data),] %>%
        fabletools::model(
          stl = STL(Value)
        ) %>%
        components()

      Best_STL_Decomposition_autoplot <- Best_STL_Decomposition %>% autoplot()

      Best_Innovation_Residuals <- augment(Best_Model) %>% drop_na() %>%
        autoplot(.innov) +
        geom_point() +
        geom_hline(yintercept = 0, color = "red") +
        labs(title = "Fourier 1 innovation residuals")

      Best_ACF <- augment(Best_Model) %>% drop_na() %>%
        feasts::ACF(.innov) %>%
        autoplot() +
        labs(title = "Fourier 1 Autocorrelation function")

      Best_Histogram_of_Residuals <- augment(Best_Model) %>% drop_na() %>%
        ggplot(aes(x = .resid)) +
        ggplot2::geom_histogram(bins = round(nrow(time_series_data)/5)) +
        ggplot2::geom_vline(xintercept = 0, color = "red") +
        labs(title = "Fourier 1 histogram of residuals")

      Best_Actual_vs_Predicted <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
        geom_point() +
        ggtitle("Fourier1 Actual vs Predicted") +
        geom_abline(slope = 1, intercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Predicted")

      Best_Actual_vs_Residuals <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
        geom_point() +
        ggtitle("Fourier1 Actual vs Residuals") +
        geom_hline(yintercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Residuals")

      Best_Actual_vs_Trend <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = trend)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = trend), color = "red") +
        ggtitle("Fourier1 Actual (black) vs trend (red)")+
        xlab("Actual") +
        ylab("Trend")

      Best_Actual_vs_Seasonally_Adjusted <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = season_adjust)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = season_adjust), color = "red") +
        ggtitle("Fourier1 Actual (black) vs seasonally adjusted (red)")+
        xlab("Actual") +
        ylab("Seasonally adjusted value")
    }

    # Fourier2 Results --------------------------------------------------------

    if(value_forecast_accuracy[1,1] == "Fourier2"){

      Best_Model <- time_series_data %>%
        fabletools::model(
          Fourier2 = fable::ARIMA(Value ~ fourier(K = 2) + PDQ(0,0,0))
        )

      augment(Best_Model) %>% drop_na()

      Best_Forecast <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast)

      Best_Forecast_plot <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast) %>%
        autoplot(Time_Series_test) +
        labs(title = "Fourier 2 model three month forecast of total values")+
        scale_y_continuous(labels = scales::comma) +
        ylab("Total values")

      Best_Forecast_Min <-
        Best_Forecast$.mean - value_forecast_accuracy[1,4]

      Best_Forecast_Max <-
        Best_Forecast$.mean + value_forecast_accuracy[1,4]

      Best_STL_Decomposition <-
        time_series_data[2:nrow(time_series_data),] %>%
        fabletools::model(
          stl = STL(Value)
        ) %>%
        components()

      Best_STL_Decomposition_autoplot <- Best_STL_Decomposition %>% autoplot()

      Best_Innovation_Residuals <- augment(Best_Model) %>% drop_na() %>%
        autoplot(.innov) +
        geom_point() +
        geom_hline(yintercept = 0, color = "red") +
        labs(title = "Fourier 2 innovation residuals")

      Best_ACF <- augment(Best_Model) %>% drop_na() %>%
        feasts::ACF(.innov) %>%
        autoplot() +
        labs(title = "Fourier 2 Autocorrelation function")

      Best_Histogram_of_Residuals <- augment(Best_Model) %>% drop_na() %>%
        ggplot(aes(x = .resid)) +
        ggplot2::geom_histogram(bins = round(nrow(time_series_data)/5)) +
        ggplot2::geom_vline(xintercept = 0, color = "red") +
        labs(title = "Fourier 2 histogram of residuals")

      Best_Actual_vs_Predicted <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
        geom_point() +
        ggtitle("Fourier2 Actual vs Predicted") +
        geom_abline(slope = 1, intercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Predicted")

      Best_Actual_vs_Residuals <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
        geom_point() +
        ggtitle("Fourier2 Actual vs Residuals") +
        geom_hline(yintercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Residuals")

      Best_Actual_vs_Trend <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = trend)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = trend), color = "red") +
        ggtitle("Fourier2 Actual (black) vs trend (red)")+
        xlab("Actual") +
        ylab("Trend")

      Best_Actual_vs_Seasonally_Adjusted <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = season_adjust)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = season_adjust), color = "red") +
        ggtitle("Fourier2 Actual (black) vs seasonally adjusted (red)")+
        xlab("Actual") +
        ylab("Seasonally adjusted value")
    }

    # Fourier3 Results --------------------------------------------------------

    if(value_forecast_accuracy[1,1] == "Fourier3"){

      Best_Model <- time_series_data %>%
        fabletools::model(
          Fourier3 = fable::ARIMA(Value ~ fourier(K = 3) + PDQ(0,0,0))
        )

      augment(Best_Model) %>% drop_na()

      Best_Forecast <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast)

      Best_Forecast_plot <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast) %>%
        autoplot(Time_Series_test) +
        labs(title = "Fourier 3 model three month forecast of total values")+
        scale_y_continuous(labels = scales::comma) +
        ylab("Total values")

      Best_Forecast_Min <-
        Best_Forecast$.mean - value_forecast_accuracy[1,4]

      Best_Forecast_Max <-
        Best_Forecast$.mean + value_forecast_accuracy[1,4]

      Best_STL_Decomposition <-
        time_series_data[2:nrow(time_series_data),] %>%
        fabletools::model(
          stl = STL(Value)
        ) %>%
        components()

      Best_STL_Decomposition_autoplot <- Best_STL_Decomposition %>% autoplot()

      Best_Innovation_Residuals <- augment(Best_Model) %>% drop_na() %>%
        autoplot(.innov) +
        geom_point() +
        geom_hline(yintercept = 0, color = "red") +
        labs(title = "Fourier 3 innovation residuals")

      Best_ACF <- augment(Best_Model) %>% drop_na() %>%
        feasts::ACF(.innov) %>%
        feasts::autoplot() +
        labs(title = "Fourier 3 Autocorrelation function")

      Best_Histogram_of_Residuals <- augment(Best_Model) %>% drop_na() %>%
        ggplot(aes(x = .resid)) +
        ggplot2::geom_histogram(bins = round(nrow(time_series_data)/5)) +
        ggplot2::geom_vline(xintercept = 0, color = "red") +
        labs(title = "Fourier 3 histogram of residuals")

      Best_Actual_vs_Predicted <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
        geom_point() +
        ggtitle("Fourier3 Actual vs Predicted") +
        geom_abline(slope = 1, intercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Predicted")

      Best_Actual_vs_Residuals <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
        geom_point() +
        ggtitle("Fourier3 Actual vs Residuals") +
        geom_hline(yintercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Residuals")

      Best_Actual_vs_Trend <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = trend)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = trend), color = "red") +
        ggtitle("Fourier3 Actual (black) vs trend (red)")+
        xlab("Actual") +
        ylab("Trend")

      Best_Actual_vs_Seasonally_Adjusted <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = season_adjust)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = season_adjust), color = "red") +
        ggtitle("Fourier3 Actual (black) vs seasonally adjusted (red)")+
        xlab("Actual") +
        ylab("Seasonally adjusted value")
    }

    # Fourier4 Results --------------------------------------------------------

    if(value_forecast_accuracy[1,1] == "Fourier4"){

      Best_Model <- time_series_data %>%
        fabletools::model(
          Fourier4 = fable::ARIMA(Value ~ fourier(K = 4) + PDQ(0,0,0))
        )

      augment(Best_Model) %>% drop_na()

      Best_Forecast <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast)

      Best_Forecast_plot <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast) %>%
        autoplot(Time_Series_test) +
        labs(title = "Fourier 4 model three month forecast of total values")+
        scale_y_continuous(labels = scales::comma) +
        ylab("Total values")

      Best_Forecast_Min <-
        Best_Forecast$.mean - value_forecast_accuracy[1,4]

      Best_Forecast_Max <-
        Best_Forecast$.mean + value_forecast_accuracy[1,4]

      Best_STL_Decomposition <-
        time_series_data[2:nrow(time_series_data),] %>%
        fabletools::model(
          stl = STL(Value)
        ) %>%
        components()

      Best_STL_Decomposition_autoplot <- Best_STL_Decomposition %>% autoplot()

      Best_Innovation_Residuals <- augment(Best_Model) %>% drop_na() %>%
        autoplot(.innov) +
        geom_point() +
        geom_hline(yintercept = 0, color = "red") +
        labs(title = "Fourier 4 innovation residuals")

      Best_ACF <- augment(Best_Model) %>% drop_na() %>%
        feasts::ACF(.innov) %>%
        autoplot() +
        labs(title = "Fourier 4 Autocorrelation function")

      Best_Histogram_of_Residuals <- augment(Best_Model) %>% drop_na() %>%
        ggplot(aes(x = .resid)) +
        ggplot2::geom_histogram(bins = round(nrow(time_series_data)/5)) +
        ggplot2::geom_vline(xintercept = 0, color = "red") +
        labs(title = "Fourier 4 histogram of residuals")

      Best_Actual_vs_Predicted <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
        geom_point() +
        ggtitle("Fourier4 Actual vs Predicted") +
        geom_abline(slope = 1, intercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Predicted")

      Best_Actual_vs_Residuals <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
        geom_point() +
        ggtitle("Fourier4 Actual vs Residuals") +
        geom_hline(yintercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Residuals")

      Best_Actual_vs_Trend <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = trend)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = trend), color = "red") +
        ggtitle("Fourier4 Actual (black) vs trend (red)")+
        xlab("Actual") +
        ylab("Trend")

      Best_Actual_vs_Seasonally_Adjusted <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = season_adjust)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = season_adjust), color = "red") +
        ggtitle("Fourier4 Actual (black) vs seasonally adjusted (red)")+
        xlab("Actual") +
        ylab("Seasonally adjusted value")
    }

    # Fourier5 Results --------------------------------------------------------

    if(value_forecast_accuracy[1,1] == "Fourier5"){

      Best_Model <- time_series_data %>%
        fabletools::model(
          Fourier5 = fable::ARIMA(Value ~ fourier(K = 5) + PDQ(0,0,0))
        )

      augment(Best_Model) %>% drop_na()

      Best_Forecast <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast)

      Best_Forecast_plot <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast) %>%
        autoplot(Time_Series_test) +
        labs(title = "Fourier 5 model three month forecast of total values")+
        scale_y_continuous(labels = scales::comma) +
        ylab("Total values")

      Best_Forecast_Min <-
        Best_Forecast$.mean - value_forecast_accuracy[1,4]

      Best_Forecast_Max <-
        Best_Forecast$.mean + value_forecast_accuracy[1,4]

      Best_STL_Decomposition <-
        time_series_data[2:nrow(time_series_data),] %>%
        fabletools::model(
          stl = STL(Value)
        ) %>%
        components()

      Best_STL_Decomposition_autoplot <- Best_STL_Decomposition %>% autoplot()

      Best_Innovation_Residuals <- augment(Best_Model) %>% drop_na() %>%
        autoplot(.innov) +
        geom_point() +
        geom_hline(yintercept = 0, color = "red") +
        labs(title = "Fourier 5 innovation residuals")

      Best_ACF <- augment(Best_Model) %>% drop_na() %>%
        feasts::ACF(.innov) %>%
        autoplot() +
        labs(title = "Fourier 5 Autocorrelation function")

      Best_Histogram_of_Residuals <- augment(Best_Model) %>% drop_na() %>%
        ggplot(aes(x = .resid)) +
        ggplot2::geom_histogram(bins = round(nrow(time_series_data)/5)) +
        ggplot2::geom_vline(xintercept = 0, color = "red") +
        labs(title = "Fourier 5 histogram of residuals")

      Best_Actual_vs_Predicted <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
        geom_point() +
        ggtitle("Fourier5 Actual vs Predicted") +
        geom_abline(slope = 1, intercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Actual")

      Best_Actual_vs_Residuals <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
        geom_point() +
        ggtitle("Fourier5 Actual vs Residuals") +
        geom_hline(yintercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Residuals")

      Best_Actual_vs_Trend <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = trend)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = trend), color = "red") +
        ggtitle("Fourier5 Actual (black) vs trend (red)")+
        xlab("Actual") +
        ylab("Trend")

      Best_Actual_vs_Seasonally_Adjusted <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = season_adjust)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = season_adjust), color = "red") +
        ggtitle("Fourier5 Actual (black) vs seasonally adjusted (red)")+
        xlab("Actual") +
        ylab("Seasonally adjusted value")
    }

    # Fourier6 Results --------------------------------------------------------

    if(value_forecast_accuracy[1,1] == "Fourier6"){

      Best_Model <- time_series_data %>%
        fabletools::model(
          Fourier6 = fable::ARIMA(Value ~ fourier(K = 6) + PDQ(0,0,0))
        )

      augment(Best_Model) %>% drop_na()

      Best_Forecast <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast)

      Best_Forecast_plot <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast) %>%
        autoplot(Time_Series_test) +
        labs(title = "Fourier 6 model three month forecast of total values")+
        scale_y_continuous(labels = scales::comma) +
        ylab("Total values")

      Best_Forecast_Min <-
        Best_Forecast$.mean - value_forecast_accuracy[1,4]

      Best_Forecast_Max <-
        Best_Forecast$.mean + value_forecast_accuracy[1,4]

      Best_STL_Decomposition <-
        time_series_data[2:nrow(time_series_data),] %>%
        fabletools::model(
          stl = STL(Value)
        ) %>%
        components()

      Best_STL_Decomposition_autoplot <- Best_STL_Decomposition %>% autoplot()

      Best_Innovation_Residuals <- augment(Best_Model) %>% drop_na() %>%
        autoplot(.innov) +
        geom_point() +
        geom_hline(yintercept = 0, color = "red") +
        labs(title = "Fourier 6 innovation residuals")

      Best_ACF <- augment(Best_Model) %>% drop_na() %>%
        feasts::ACF(.innov) %>%
        autoplot() +
        labs(title = "Fourier 6 Autocorrelation function")

      Best_Histogram_of_Residuals <- augment(Best_Model) %>% drop_na() %>%
        ggplot(aes(x = .resid)) +
        ggplot2::geom_histogram(bins = round(nrow(time_series_data)/5)) +
        ggplot2::geom_vline(xintercept = 0, color = "red") +
        labs(title = "Fourier 6 histogram of residuals")
      Best_Actual_vs_Predicted <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
        geom_point() +
        ggtitle("Fourier6 Actual vs Predicted") +
        geom_abline(slope = 1, intercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Predicted")

      Best_Actual_vs_Residuals <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
        geom_point() +
        ggtitle("Fourier6 Actual vs Residuals") +
        geom_hline(yintercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Residuals")

      Best_Actual_vs_Trend <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = trend)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = trend), color = "red") +
        ggtitle("Fourier6 Actual (black) vs trend (red)")+
        xlab("Actual") +
        ylab("Trend")

      Best_Actual_vs_Seasonally_Adjusted <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = season_adjust)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = season_adjust), color = "red") +
        ggtitle("Fourier6 Actual (black) vs seasonally adjusted (red)")+
        xlab("Actual") +
        ylab("Seasonally adjusted value")
    }

    # Holt-Winters Additive Results -------------------------------------------

    if(value_forecast_accuracy[1,1] == "Holt_Winters_Additive"){

      Best_Model <- time_series_data %>%
        fabletools::model(
          Holt_Winters_Additive = fable::ETS(Value ~ error("A") + trend("A") + season("A"))
        )

      augment(Best_Model) %>% drop_na()

      Best_Forecast <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast)

      Best_Forecast_plot <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast) %>%
        autoplot(Time_Series_test) +
        labs(title = "Holt-Winters Additive model three month forecast of total values")+
        scale_y_continuous(labels = scales::comma) +
        ylab("Total values")

      Best_Forecast_Min <-
        Best_Forecast$.mean - value_forecast_accuracy[1,4]

      Best_Forecast_Max <-
        Best_Forecast$.mean + value_forecast_accuracy[1,4]

      Best_STL_Decomposition <-
        time_series_data[2:nrow(time_series_data),] %>%
        fabletools::model(
          stl = STL(Value)
        ) %>%
        components()

      Best_STL_Decomposition_autoplot <- Best_STL_Decomposition %>% autoplot()

      Best_Innovation_Residuals <- augment(Best_Model) %>% drop_na() %>%
        autoplot(.innov) +
        geom_point() +
        geom_hline(yintercept = 0, color = "red") +
        labs(title = "Holt-Winters Additive innovation residuals")

      Best_ACF <- augment(Best_Model) %>% drop_na() %>%
        feasts::ACF(.innov) %>%
        autoplot() +
        labs(title = "Holt-Winters Additive Autocorrelation function")

      Best_Histogram_of_Residuals <- augment(Best_Model) %>% drop_na() %>%
        ggplot(aes(x = .resid)) +
        ggplot2::geom_histogram(bins = round(nrow(time_series_data)/5)) +
        ggplot2::geom_vline(xintercept = 0, color = "red") +
        labs(title = "Holt-Winters Additive histogram of residuals")

      Best_Actual_vs_Predicted <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
        geom_point() +
        ggtitle("Holt_Winters_Additive Actual vs Predicted") +
        geom_abline(slope = 1, intercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Predicted")

      Best_Actual_vs_Residuals <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
        geom_point() +
        ggtitle("Holt_Winters_Additive Actual vs Residuals") +
        geom_hline(yintercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Residuals")

      Best_Actual_vs_Trend <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = trend)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = trend), color = "red") +
        ggtitle("Holt_Winters_Additive Actual (black) vs trend (red)")+
        xlab("Actual") +
        ylab("Trend")

      Best_Actual_vs_Seasonally_Adjusted <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = season_adjust)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = season_adjust), color = "red") +
        ggtitle("Holt_Winters_Additive Actual (black) vs seasonally adjusted (red)")+
        xlab("Actual") +
        ylab("Seasonally adjusted value")
    }

    # Holt-Winters Multiplicative Results -------------------------------------

    if(value_forecast_accuracy[1,1] == "Holt_Winters_Multiplicative"){

      Best_Model <- time_series_data %>%
        fabletools::model(
          Holt_Winters_Multiplicative = fable::ETS(Value ~ error("M") + trend("A") + season("M"))
        )

      augment(Best_Model) %>% drop_na()

      Best_Forecast <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast)

      Best_Forecast_plot <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast) %>%
        autoplot(Time_Series_test) +
        labs(title = "Holt-Winters Multiplicative model three month forecast of total values")+
        scale_y_continuous(labels = scales::comma) +
        ylab("Total values")

      Best_Forecast_Min <-
        Best_Forecast$.mean - value_forecast_accuracy[1,4]

      Best_Forecast_Max <-
        Best_Forecast$.mean + value_forecast_accuracy[1,4]

      Best_STL_Decomposition <-
        time_series_data[2:nrow(time_series_data),] %>%
        fabletools::model(
          stl = STL(Value)
        ) %>%
        components()

      Best_STL_Decomposition_autoplot <- Best_STL_Decomposition %>% autoplot()

      Best_Innovation_Residuals <- augment(Best_Model) %>% drop_na() %>%
        autoplot(.innov) +
        geom_point() +
        geom_hline(yintercept = 0, color = "red") +
        labs(title = "Holt-Winters Multiplicative innovation residuals")

      Best_ACF <- augment(Best_Model) %>% drop_na() %>%
        feasts::ACF(.innov) %>%
        autoplot() +
        labs(title = "Holt-Winters Multiplicative Autocorrelation function")

      Best_Histogram_of_Residuals <- augment(Best_Model) %>% drop_na() %>%
        ggplot(aes(x = .resid)) +
        ggplot2::geom_histogram(bins = round(nrow(time_series_data)/5)) +
        ggplot2::geom_vline(xintercept = 0, color = "red") +
        labs(title = "Holt-Winters Multiplicative histogram of residuals")

      Best_Actual_vs_Predicted <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
        geom_point() +
        ggtitle("Holt_Winters_Multiplicative Actual vs Predicted") +
        geom_abline(slope = 1, intercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Predicted")

      Best_Actual_vs_Residuals <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
        geom_point() +
        ggtitle("Holt_Winters_Multiplicative Actual vs Residuals") +
        geom_hline(yintercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Residuals")

      Best_Actual_vs_Trend <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = trend)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = trend), color = "red") +
        ggtitle("Holt_Winters_Multiplicative Actual (black) vs trend (red)")+
        xlab("Actual") +
        ylab("Trend")

      Best_Actual_vs_Seasonally_Adjusted <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = season_adjust)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = season_adjust), color = "red") +
        ggtitle("Holt_Winters_Multiplicative Actual (black) vs seasonally adjusted (red)")+
        xlab("Actual") +
        ylab("Seasonally adjusted value")
    }

    # Holt-Winters Damped Results ---------------------------------------------

    if(value_forecast_accuracy[1,1] == "Holt_Winters_Damped"){

      Best_Model <- time_series_data %>%
        fabletools::model(
          Holt_Winters_Damped = fable::ETS(Value ~ error("M") + trend("Ad") + season("M"))
        )

      augment(Best_Model) %>% drop_na()

      Best_Forecast <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast)

      Best_Forecast_plot <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast) %>%
        autoplot(Time_Series_test) +
        labs(title = "Holt-Winters Damped model three month forecast of total values")+
        scale_y_continuous(labels = scales::comma) +
        ylab("Total values")

      Best_Forecast_Min <-
        Best_Forecast$.mean - value_forecast_accuracy[1,4]

      Best_Forecast_Max <-
        Best_Forecast$.mean + value_forecast_accuracy[1,4]

      Best_STL_Decomposition <-
        time_series_data[2:nrow(time_series_data),] %>%
        fabletools::model(
          stl = STL(Value)
        ) %>%
        components()

      Best_STL_Decomposition_autoplot <- Best_STL_Decomposition %>% autoplot()

      Best_Innovation_Residuals <- augment(Best_Model) %>% drop_na() %>%
        autoplot(.innov) +
        geom_point() +
        geom_hline(yintercept = 0, color = "red") +
        labs(title = "Holt-Winters Damped innovation residuals")

      Best_ACF <- augment(Best_Model) %>% drop_na() %>%
        feasts::ACF(.innov) %>%
        autoplot() +
        labs(title = "Holt-Winters Damped Autocorrelation function")

      Best_Histogram_of_Residuals <- augment(Best_Model) %>% drop_na() %>%
        ggplot(aes(x = .resid)) +
        ggplot2::geom_histogram(bins = round(nrow(time_series_data)/5)) +
        ggplot2::geom_vline(xintercept = 0, color = "red") +
        labs(title = "Holt-Winters Damped histogram of residuals")

      Best_Actual_vs_Predicted <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
        geom_point() +
        ggtitle("Holt_Winters_Damped Actual vs Predicted") +
        geom_abline(slope = 1, intercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Predicted")

      Best_Actual_vs_Residuals <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
        geom_point() +
        ggtitle("Holt_Winters_Damped Actual vs Residuals") +
        geom_hline(yintercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Residuals")

      Best_Actual_vs_Trend <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = trend)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = trend), color = "red") +
        ggtitle("Holt_Winters_Damped Actual (black) vs trend (red)")+
        xlab("Actual") +
        ylab("Trend")

      Best_Actual_vs_Seasonally_Adjusted <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = season_adjust)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = season_adjust), color = "red") +
        ggtitle("Holt_Winters_Damped Actual (black) vs seasonally adjusted (red)")+
        xlab("Actual") +
        ylab("Seasonally adjusted value")
    }


    # Linear1 Results ---------------------------------------------------------

    if(value_forecast_accuracy[1,1] == "Linear1"){

      Best_Model <- time_series_data %>%
        fabletools::model(
          Linear1 = TSLM(Value ~ season() + trend())
        )

      augment(Best_Model) %>% drop_na()

      Best_Forecast <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast)

      Best_Forecast_plot <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast) %>%
        autoplot(Time_Series_test) +
        labs(title = "Linear 1 model three month forecast of total values")+
        scale_y_continuous(labels = scales::comma) +
        ylab("Total values")

      Best_Forecast_Min <-
        Best_Forecast$.mean - value_forecast_accuracy[1,4]

      Best_Forecast_Max <-
        Best_Forecast$.mean + value_forecast_accuracy[1,4]

      Best_STL_Decomposition <-
        time_series_data[2:nrow(time_series_data),] %>%
        fabletools::model(
          stl = STL(Value)
        ) %>%
        components()

      Best_STL_Decomposition_autoplot <- Best_STL_Decomposition %>% autoplot()

      Best_Innovation_Residuals <- augment(Best_Model) %>% drop_na() %>%
        autoplot(.innov) +
        geom_point() +
        geom_hline(yintercept = 0, color = "red") +
        labs(title = "Linear 1 innovation residuals")

      Best_ACF <- augment(Best_Model) %>% drop_na() %>%
        feasts::ACF(.innov) %>%
        autoplot() +
        labs(title = "Linear 1 Autocorrelation function")

      Best_Histogram_of_Residuals <- augment(Best_Model) %>% drop_na() %>%
        ggplot(aes(x = .resid)) +
        ggplot2::geom_histogram(bins = round(nrow(time_series_data)/5)) +
        ggplot2::geom_vline(xintercept = 0, color = "red") +
        labs(title = "Linear 1 histogram of residuals")

      Best_Actual_vs_Predicted <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
        geom_point() +
        ggtitle("Linear1 Actual vs Predicted")+
        geom_abline(slope = 1, intercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Predicted")

      Best_Actual_vs_Residuals <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
        geom_point() +
        ggtitle("Linear1 Actual vs Residuals") +
        geom_hline(yintercept = 0, color = "red")+
        geom_abline(slope = 1, intercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Residuals")

      Best_Actual_vs_Trend <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = trend)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = trend), color = "red") +
        ggtitle("Linear1 Actual (black) vs trend (red)")+
        xlab("Actual") +
        ylab("Trend")

      Best_Actual_vs_Seasonally_Adjusted <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = season_adjust)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = season_adjust), color = "red") +
        ggtitle("Linear1 Actual (black) vs seasonally adjusted (red)")+
        xlab("Actual") +
        ylab("Seasonally adjusted value")
    }

    # Linear2 Results ---------------------------------------------------------

    if(value_forecast_accuracy[1,1] == "Linear2"){

      Best_Model <- time_series_data %>%
        fabletools::model(
          Linear2 = TSLM(Value ~ season() + trend())
        )

      augment(Best_Model) %>% drop_na()

      Best_Forecast <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast)

      Best_Forecast_plot <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast) %>%
        autoplot(Time_Series_test) +
        labs(title = "Linear 2 model three month forecast of total values")+
        scale_y_continuous(labels = scales::comma) +
        ylab("Total values")

      Best_Forecast_Min <-
        Best_Forecast$.mean - value_forecast_accuracy[1,4]

      Best_Forecast_Max <-
        Best_Forecast$.mean + value_forecast_accuracy[1,4]

      Best_STL_Decomposition <-
        time_series_data[2:nrow(time_series_data),] %>%
        fabletools::model(
          stl = STL(Value)
        ) %>%
        components()

      Best_STL_Decomposition_autoplot <- Best_STL_Decomposition %>% autoplot()

      Best_Innovation_Residuals <- augment(Best_Model) %>% drop_na() %>%
        autoplot(.innov) +
        geom_point() +
        geom_hline(yintercept = 0, color = "red") +
        labs(title = "Linear 2 innovation residuals")

      Best_ACF <- augment(Best_Model) %>% drop_na() %>%
        feasts::ACF(.innov) %>%
        autoplot() +
        labs(title = "Linear 2 Autocorrelation function")

      Best_Histogram_of_Residuals <- augment(Best_Model) %>% drop_na() %>%
        ggplot(aes(x = .resid)) +
        ggplot2::geom_histogram(bins = round(nrow(time_series_data)/5)) +
        ggplot2::geom_vline(xintercept = 0, color = "red") +
        labs(title = "Linear 2 histogram of residuals")

      Best_Actual_vs_Predicted <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
        geom_point() +
        ggtitle("Linear2 Actual vs Predicted")+
        geom_abline(slope = 1, intercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Predicted")

      Best_Actual_vs_Residuals <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
        geom_point() +
        ggtitle("Linear2 Actual vs Residuals") +
        geom_hline(yintercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Residuals")

      Best_Actual_vs_Trend <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = trend)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = trend), color = "red") +
        ggtitle("Linear2 Actual (black) vs trend (red)")+
        xlab("Actual") +
        ylab("Trend")

      Best_Actual_vs_Seasonally_Adjusted <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = season_adjust)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = season_adjust), color = "red") +
        ggtitle("Linear2 Actual (black) vs seasonally adjusted (red)")+
        xlab("Actual") +
        ylab("Seasonally adjusted value")
    }


    # Linear3 Results ---------------------------------------------------------

    if(value_forecast_accuracy[1,1] == "Linear3"){

      Best_Model <- time_series_data %>%
        fabletools::model(
          Linear3 = TSLM(Value ~ season())
        )

      augment(Best_Model) %>% drop_na()

      Best_Forecast <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast)

      Best_Forecast_plot <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast) %>%
        autoplot(Time_Series_test) +
        labs(title = "Linear 3 model three month forecast of total values")+
        scale_y_continuous(labels = scales::comma) +
        ylab("Total values")

      Best_Forecast_Min <-
        Best_Forecast$.mean - value_forecast_accuracy[1,4]

      Best_Forecast_Max <-
        Best_Forecast$.mean + value_forecast_accuracy[1,4]

      Best_STL_Decomposition <-
        time_series_data[2:nrow(time_series_data),] %>%
        fabletools::model(
          stl = STL(Value)
        ) %>%
        components()

      Best_STL_Decomposition_autoplot <- Best_STL_Decomposition %>% autoplot()

      Best_Innovation_Residuals <- augment(Best_Model) %>% drop_na() %>%
        autoplot(.innov) +
        geom_point() +
        geom_hline(yintercept = 0, color = "red") +
        labs(title = "Linear 3 innovation residuals")

      Best_ACF <- augment(Best_Model) %>% drop_na() %>%
        feasts::ACF(.innov) %>%
        autoplot() +
        labs(title = "Linear 3 Autocorrelation function")

      Best_Histogram_of_Residuals <- augment(Best_Model) %>% drop_na() %>%
        ggplot(aes(x = .resid)) +
        ggplot2::geom_histogram(bins = round(nrow(time_series_data)/5)) +
        ggplot2::geom_vline(xintercept = 0, color = "red") +
        labs(title = "Linear 3 histogram of residuals")

      Best_Actual_vs_Predicted <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
        geom_point() +
        ggtitle("Linear3 Actual vs Predicted")+
        geom_abline(slope = 1, intercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Predicted")

      Best_Actual_vs_Residuals <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
        geom_point() +
        ggtitle("Linear3 Actual vs Residuals") +
        geom_hline(yintercept = 0, color = "red")+
        geom_abline(slope = 1, intercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Residuals")

      Best_Actual_vs_Trend <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = trend)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = trend), color = "red") +
        ggtitle("Linear3 Actual (black) vs trend (red)")+
        xlab("Actual") +
        ylab("Trend")

      Best_Actual_vs_Seasonally_Adjusted <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = season_adjust)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = season_adjust), color = "red") +
        ggtitle("Linear3 Actual (black) vs seasonally adjusted (red)")+
        xlab("Actual") +
        ylab("Seasonally adjusted value")
    }

    # Linear4 Results ---------------------------------------------------------

    if(value_forecast_accuracy[1,1] == "Linear4"){

      Best_Model <- time_series_data %>%
        fabletools::model(
          Linear4 = TSLM(Value ~ trend())
        )

      augment(Best_Model) %>% drop_na()

      Best_Forecast <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast)

      Best_Forecast_plot <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast) %>%
        autoplot(Time_Series_test) +
        labs(title = "Linear 4 model three month forecast of total values")+
        scale_y_continuous(labels = scales::comma) +
        ylab("Total values")

      Best_Forecast_Min <-
        Best_Forecast$.mean - value_forecast_accuracy[1,4]

      Best_Forecast_Max <-
        Best_Forecast$.mean + value_forecast_accuracy[1,4]

      Best_STL_Decomposition <-
        time_series_data[2:nrow(time_series_data),] %>%
        fabletools::model(
          stl = STL(Value)
        ) %>%
        components()

      Best_STL_Decomposition_autoplot <- Best_STL_Decomposition %>% autoplot()

      Best_Innovation_Residuals <- augment(Best_Model) %>% drop_na() %>%
        autoplot(.innov) +
        geom_point() +
        geom_hline(yintercept = 0, color = "red") +
        labs(title = "Linear 4 innovation residuals")

      Best_ACF <- augment(Best_Model) %>% drop_na() %>%
        feasts::ACF(.innov) %>%
        autoplot() +
        labs(title = "Linear 4 Autocorrelation function")

      Best_Histogram_of_Residuals <- augment(Best_Model) %>% drop_na() %>%
        ggplot(aes(x = .resid)) +
        ggplot2::geom_histogram(bins = round(nrow(time_series_data)/5)) +
        ggplot2::geom_vline(xintercept = 0, color = "red") +
        labs(title = "Linear 4 histogram of residuals")

      Best_Actual_vs_Predicted <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
        geom_point() +
        ggtitle("Linear4 Actual vs Predicted")+
        geom_abline(slope = 1, intercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Predicted")

      Best_Actual_vs_Residuals <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
        geom_point() +
        ggtitle("Linear4 Actual vs Residuals") +
        geom_hline(yintercept = 0, color = "red")+
        geom_abline(slope = 1, intercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Residuals")

      Best_Actual_vs_Trend <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = trend)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = trend), color = "red") +
        ggtitle("Linear4 Actual (black) vs trend (red)")+
        xlab("Actual") +
        ylab("Trend")

      Best_Actual_vs_Seasonally_Adjusted <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = season_adjust)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = season_adjust), color = "red") +
        ggtitle("Linear4 Actual (black) vs seasonally adjusted (red)")+
        xlab("Actual") +
        ylab("Seasonally adjusted value")
    }

    # Mean Results ------------------------------------------------------------

    if(value_forecast_accuracy[1,1] == "Mean"){

      Best_Model <- time_series_data %>%
        fabletools::model(
          Mean = fable::MEAN(Value)
        )

      augment(Best_Model) %>% drop_na()

      Best_Forecast <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast)

      Best_Forecast_plot <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast) %>%
        autoplot(Time_Series_test) +
        labs(title = "Mean model three month forecast of total values")+
        scale_y_continuous(labels = scales::comma) +
        ylab("Total values")

      Best_Forecast_Min <-
        Best_Forecast$.mean - value_forecast_accuracy[1,4]

      Best_Forecast_Max <-
        Best_Forecast$.mean + value_forecast_accuracy[1,4]

      Best_STL_Decomposition <-
        time_series_data[2:nrow(time_series_data),] %>%
        fabletools::model(
          stl = STL(Value)
        ) %>%
        components()

      Best_STL_Decomposition_autoplot <- Best_STL_Decomposition %>% autoplot()

      Best_Innovation_Residuals <- augment(Best_Model) %>% drop_na() %>%
        autoplot(.innov) +
        geom_point() +
        geom_hline(yintercept = 0, color = "red") +
        labs(title = "Mean innovation residuals")

      Best_ACF <- augment(Best_Model) %>% drop_na() %>%
        feasts::ACF(.innov) %>%
        autoplot() +
        labs(title = "Mean Autocorrelation function")

      Best_Histogram_of_Residuals <- augment(Best_Model) %>% drop_na() %>%
        ggplot(aes(x = .resid)) +
        ggplot2::geom_histogram(bins = round(nrow(time_series_data)/5)) +
        ggplot2::geom_vline(xintercept = 0, color = "red") +
        labs(title = "Mean histogram of residuals")

      Best_Actual_vs_Predicted <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
        geom_point() +
        ggtitle("Mean Actual vs Predicted") +
        geom_abline(slope = 1, intercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Predicted")

      Best_Actual_vs_Residuals <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
        geom_point() +
        ggtitle("Mean Actual vs Residuals") +
        geom_hline(yintercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Residuals")

      Best_Actual_vs_Trend <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = trend)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = trend), color = "red") +
        ggtitle("Mean Actual (black) vs trend (red)")+
        xlab("Actual") +
        ylab("Trend")

      Best_Actual_vs_Seasonally_Adjusted <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = season_adjust)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = season_adjust), color = "red") +
        ggtitle("Mean Actual (black) vs seasonally adjusted (red)")+
        xlab("Actual") +
        ylab("Seasonally adjusted value")
    }

    # Naive Results -----------------------------------------------------------

    if(value_forecast_accuracy[1,1] == "Naive"){

      Best_Model <- time_series_data %>%
        fabletools::model(
          Naive = fable::NAIVE(Value)
        )

      augment(Best_Model) %>% drop_na()

      Best_Forecast <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast)

      Best_Forecast_plot <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast) %>%
        autoplot(Time_Series_test) +
        labs(title = "Naive model three month forecast of total values")+
        scale_y_continuous(labels = scales::comma) +
        ylab("Total values")

      Best_Forecast_Min <-
        Best_Forecast$.mean - value_forecast_accuracy[1,4]

      Best_Forecast_Max <-
        Best_Forecast$.mean + value_forecast_accuracy[1,4]

      Best_STL_Decomposition <-
        time_series_data[2:nrow(time_series_data),] %>%
        fabletools::model(
          stl = STL(Value)
        ) %>%
        components()

      Best_STL_Decomposition_autoplot <- Best_STL_Decomposition %>% autoplot()

      Best_Innovation_Residuals <- augment(Best_Model) %>% drop_na()[2:nrow(time_series_data),] %>%
        autoplot(.innov) +
        geom_point() +
        geom_hline(yintercept = 0, color = "red") +
        labs(title = "Naive innovation residuals")

      Best_ACF <- augment(Best_Model) %>% drop_na() %>%
        feasts::ACF(.innov) %>%
        autoplot() +
        labs(title = "Naive Autocorrelation function")

      Best_Histogram_of_Residuals <- augment(Best_Model) %>% drop_na()[2:nrow(Time_Series_test),] %>%
        ggplot(aes(x = .resid)) +
        ggplot2::geom_histogram(bins = round(nrow(time_series_data)/5)) +
        ggplot2::geom_vline(xintercept = 0, color = "red") +
        labs(title = "Naive histogram of residuals")

      Best_Actual_vs_Predicted <-
        ggplot(augment(Best_Model) %>% drop_na()[2:nrow(augment(Best_Model) %>% drop_na()),], mapping = aes(x = Value, y = .fitted)) +
        geom_point() +
        ggtitle("Naive Actual vs Predicted") +
        geom_abline(slope = 1, intercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Predicted")

      Best_Actual_vs_Residuals <-
        ggplot(augment(Best_Model) %>% drop_na()[2:nrow(augment(Best_Model) %>% drop_na()),], mapping = aes(x = Value, y = .resid)) +
        geom_point() +
        ggtitle("Naive Actual vs Residuals") +
        geom_hline(yintercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Residuals")

      Best_Actual_vs_Trend <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = trend)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = trend), color = "red") +
        ggtitle("Naive Actual (black) vs trend (red)")+
        xlab("Actual") +
        ylab("Trend")

      Best_Actual_vs_Seasonally_Adjusted <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = season_adjust)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = season_adjust), color = "red") +
        ggtitle("Naive Actual (black) vs seasonally adjusted (red)")+
        xlab("Actual") +
        ylab("Seasonally adjusted value")
    }


    # NeuralNet1 Results ------------------------------------------------------

    if(value_forecast_accuracy[1,1] == "NeuralNet1"){

      Best_Model <- time_series_data %>%
        fabletools::model(
          NeuralNet1 = fable::NNETAR(Value)
        )

      augment(Best_Model) %>% drop_na()

      Best_Forecast <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast)

      Best_Forecast_plot <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast) %>%
        autoplot(Time_Series_test) +
        labs(title = "NeuralNet 1 model three month forecast of total values")+
        scale_y_continuous(labels = scales::comma) +
        ylab("Total values")

      Best_Forecast_Min <-
        Best_Forecast$.mean - value_forecast_accuracy[1,4]

      Best_Forecast_Max <-
        Best_Forecast$.mean + value_forecast_accuracy[1,4]

      Best_STL_Decomposition <-
        time_series_data[2:nrow(time_series_data),] %>%
        fabletools::model(
          stl = STL(Value)
        ) %>%
        components()

      Best_STL_Decomposition_autoplot <- Best_STL_Decomposition %>% autoplot()

      Best_Innovation_Residuals <- augment(Best_Model) %>% drop_na() %>%
        autoplot(.innov) +
        geom_point() +
        geom_hline(yintercept = 0, color = "red") +
        labs(title = "NeuralNet 1 innovation residuals")

      Best_ACF <- augment(Best_Model) %>% drop_na() %>%
        feasts::ACF(.innov) %>%
        autoplot() +
        labs(title = "NeuralNet 1 Autocorrelation function")

      Best_Histogram_of_Residuals <-augment(Best_Model) %>% drop_na() %>%
        ggplot(aes(x = .resid)) +
        ggplot2::geom_histogram(bins = round(nrow(time_series_data)/5)) +
        ggplot2::geom_vline(xintercept = 0, color = "red") +
        labs(title = "NeuralNet 1 histogram of residuals")

      Best_Actual_vs_Predicted <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
        geom_point() +
        ggtitle("NeuralNet1 Actual vs Predicted") +
        geom_abline(slope = 1, intercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Predicted")

      Best_Actual_vs_Residuals <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
        geom_point() +
        ggtitle("NeuralNet1 Actual vs Residuals") +
        geom_hline(yintercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Residuals")

      Best_Actual_vs_Trend <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = trend)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = trend), color = "red") +
        ggtitle("NeuralNet1 Actual (black) vs trend (red)")+
        xlab("Actual") +
        ylab("Trend")

      Best_Actual_vs_Seasonally_Adjusted <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = season_adjust)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = season_adjust), color = "red") +
        ggtitle("NeuralNet1 Actual (black) vs seasonally adjusted (red)")+
        xlab("Actual") +
        ylab("Seasonally adjusted value")
    }

    # NeuralNet2 Results ------------------------------------------------------

    if(value_forecast_accuracy[1,1] == "NeuralNet2"){

      Best_Model <- time_series_data %>%
        fabletools::model(
          NeuralNet2 = fable::NNETAR(Value ~ season())
        )

      augment(Best_Model) %>% drop_na()

      Best_Forecast <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast)

      Best_Forecast_plot <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast) %>%
        autoplot(Time_Series_test) +
        labs(title = "NeuralNet 2 model three month forecast of total values")+
        scale_y_continuous(labels = scales::comma) +
        ylab("Total values")

      Best_Forecast_Min <-
        Best_Forecast$.mean - value_forecast_accuracy[1,4]

      Best_Forecast_Max <-
        Best_Forecast$.mean + value_forecast_accuracy[1,4]

      Best_STL_Decomposition <-
        time_series_data[2:nrow(time_series_data),] %>%
        fabletools::model(
          stl = STL(Value)
        ) %>%
        components()

      Best_STL_Decomposition_autoplot <- Best_STL_Decomposition %>% autoplot()

      Best_Innovation_Residuals <- augment(Best_Model) %>% drop_na() %>%
        autoplot(.innov) +
        geom_point() +
        geom_hline(yintercept = 0, color = "red") +
        labs(title = "NeuralNet 2 innovation residuals")

      Best_ACF <- augment(Best_Model) %>% drop_na() %>%
        feasts::ACF(.innov) %>%
        autoplot() +
        labs(title = "NeuralNet 2 Autocorrelation function")

      Best_Histogram_of_Residuals <- augment(Best_Model) %>% drop_na() %>%
        ggplot(aes(x = .resid)) +
        ggplot2::geom_histogram(bins = round(nrow(time_series_data)/5)) +
        ggplot2::geom_vline(xintercept = 0, color = "red") +
        labs(title = "NeuralNet 2 histogram of residuals")

      Best_Actual_vs_Predicted <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
        geom_point() +
        ggtitle("NeuralNet2 Actual vs Predicted") +
        geom_abline(slope = 1, intercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Predicted")

      Best_Actual_vs_Residuals <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
        geom_point() +
        ggtitle("NeuralNet2 Actual vs Residuals") +
        geom_hline(yintercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Residuals")

      Best_Actual_vs_Trend <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = trend)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = trend), color = "red") +
        ggtitle("NeuralNet2 Actual (black) vs trend (red)")+
        xlab("Actual") +
        ylab("Trend")

      Best_Actual_vs_Seasonally_Adjusted <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = season_adjust)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = season_adjust), color = "red") +
        ggtitle("NeuralNet2 Actual (black) vs seasonally adjusted (red)")+
        xlab("Actual") +
        ylab("Seasonally adjusted value")
    }

    # NeuralNet3 Results ------------------------------------------------------

    if(value_forecast_accuracy[1,1] == "NeuralNet3"){

      Best_Model <- time_series_data %>%
        fabletools::model(
          NeuralNet3 = fable::NNETAR(Value ~ trend())
        )

      augment(Best_Model) %>% drop_na()

      Best_Forecast <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast)

      Best_Forecast_plot <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast) %>%
        autoplot(Time_Series_test) +
        labs(title = "NeuralNet 3 model three month forecast of total values")+
        scale_y_continuous(labels = scales::comma) +
        ylab("Total values")

      Best_Forecast_Min <-
        Best_Forecast$.mean - value_forecast_accuracy[1,4]

      Best_Forecast_Max <-
        Best_Forecast$.mean + value_forecast_accuracy[1,4]

      Best_STL_Decomposition <-
        time_series_data[2:nrow(time_series_data),] %>%
        fabletools::model(
          stl = STL(Value)
        ) %>%
        components()

      Best_STL_Decomposition_autoplot <- Best_STL_Decomposition %>% autoplot()

      Best_Innovation_Residuals <- augment(Best_Model) %>% drop_na() %>%
        autoplot(.innov) +
        geom_point() +
        geom_hline(yintercept = 0, color = "red") +
        labs(title = "NeuralNet 3 innovation residuals")

      Best_ACF <- augment(Best_Model) %>% drop_na() %>%
        feasts::ACF(.innov) %>%
        autoplot() +
        labs(title = "NeuralNet 3 Autocorrelation function")

      Best_Histogram_of_Residuals <- augment(Best_Model) %>% drop_na() %>%
        ggplot(aes(x = .resid)) +
        ggplot2::geom_histogram(bins = round(nrow(time_series_data)/5)) +
        ggplot2::geom_vline(xintercept = 0, color = "red") +
        labs(title = "NeuralNet 3 histogram of residuals")

      Best_Actual_vs_Predicted <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
        geom_point() +
        ggtitle("NeuralNet3 Actual vs Predicted") +
        geom_abline(slope = 1, intercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Predicted")

      Best_Actual_vs_Residuals <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
        geom_point() +
        ggtitle("NeuralNet3 Actual vs Residuals") +
        geom_hline(yintercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Residuals")

      Best_Actual_vs_Trend <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = trend)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = trend), color = "red") +
        ggtitle("NeuralNet3 Actual (black) vs trend (red)")+
        xlab("Actual") +
        ylab("Trend")

      Best_Actual_vs_Seasonally_Adjusted <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = season_adjust)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = season_adjust), color = "red") +
        ggtitle("NeuralNet3 Actual (black) vs seasonally adjusted (red)")+
        xlab("Actual") +
        ylab("Seasonally adjusted value")
    }

    # NeuralNet4 Results ------------------------------------------------------

    if(value_forecast_accuracy[1,1] == "NeuralNet4"){

      Best_Model <- time_series_data %>%
        fabletools::model(
          NeuralNet4 = fable::NNETAR(Value ~ season() + trend())
        )

      augment(Best_Model) %>% drop_na()

      Best_Forecast <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast)

      Best_Forecast_plot <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast) %>%
        autoplot(Time_Series_test) +
        labs(title = "NeuralNet 4 model three month forecast of total values")+
        scale_y_continuous(labels = scales::comma) +
        ylab("Total values")

      Best_Forecast_Min <-
        Best_Forecast$.mean - value_forecast_accuracy[1,4]

      Best_Forecast_Max <-
        Best_Forecast$.mean + value_forecast_accuracy[1,4]

      Best_STL_Decomposition <-
        time_series_data[2:nrow(time_series_data),] %>%
        fabletools::model(
          stl = STL(Value)
        ) %>%
        components()

      Best_STL_Decomposition_autoplot <- Best_STL_Decomposition %>% autoplot()

      Best_Innovation_Residuals <- augment(Best_Model) %>% drop_na() %>%
        autoplot(.innov) +
        geom_point() +
        geom_hline(yintercept = 0, color = "red") +
        labs(title = "NeuralNet 4 innovation residuals")

      Best_ACF <- augment(Best_Model) %>% drop_na() %>%
        feasts::ACF(.innov) %>%
        autoplot() +
        labs(title = "NeuralNet 4 Autocorrelation function")

      Best_Histogram_of_Residuals <- augment(Best_Model) %>% drop_na() %>%
        ggplot(aes(x = .resid)) +
        ggplot2::geom_histogram(bins = round(nrow(time_series_data)/5)) +
        ggplot2::geom_vline(xintercept = 0, color = "red") +
        labs(title = "NeuralNet 4 histogram of residuals")

      Best_Actual_vs_Predicted <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
        geom_point() +
        ggtitle("NeuralNet4 Actual vs Predicted") +
        geom_abline(slope = 1, intercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Predicted")

      Best_Actual_vs_Residuals <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
        geom_point() +
        ggtitle("NeuralNet4 Actual vs Residuals") +
        geom_hline(yintercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Residuals")

      Best_Actual_vs_Trend <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = trend)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = trend), color = "red") +
        ggtitle("NeuralNet4 Actual (black) vs trend (red)")+
        xlab("Actual") +
        ylab("Trend")

      Best_Actual_vs_Seasonally_Adjusted <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = season_adjust)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = season_adjust), color = "red") +
        ggtitle("NeuralNet4 Actual (black) vs seasonally adjusted (red)")+
        xlab("Actual") +
        ylab("Seasonally adjusted value")
    }

    # Prophet Additive Results ------------------------------------------------

    if(value_forecast_accuracy[1,1] == "Prophet_Additive"){

      Best_Model <- time_series_data %>%
        fabletools::model(
          Prophet_Additive = fable.prophet::prophet(Value ~ season(period = 12, type = "additive"))
        )

      augment(Best_Model) %>% drop_na()

      Best_Forecast <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast)

      Best_Forecast_plot <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast) %>%
        autoplot(Time_Series_test) +
        labs(title = "Prophet Additive model three month forecast of total values")+
        scale_y_continuous(labels = scales::comma) +
        ylab("Total values")

      Best_Forecast_Min <-
        Best_Forecast$.mean - value_forecast_accuracy[1,4]

      Best_Forecast_Max <-
        Best_Forecast$.mean + value_forecast_accuracy[1,4]

      Best_STL_Decomposition <-
        time_series_data[2:nrow(time_series_data),] %>%
        fabletools::model(
          stl = STL(Value)
        ) %>%
        components()

      Best_STL_Decomposition_autoplot <- Best_STL_Decomposition %>% autoplot()

      Best_Innovation_Residuals <- augment(Best_Model) %>% drop_na() %>%
        autoplot(.innov) +
        geom_point() +
        geom_hline(yintercept = 0, color = "red") +
        labs(title = "Prophet Additive innovation residuals")

      Best_ACF <- augment(Best_Model) %>% drop_na() %>%
        feasts::ACF(.innov) %>%
        autoplot() +
        labs(title = "Prophet Additive Autocorrelation function")

      Best_Histogram_of_Residuals <- augment(Best_Model) %>% drop_na() %>%
        ggplot(aes(x = .resid)) +
        ggplot2::geom_histogram(bins = round(nrow(time_series_data)/5)) +
        ggplot2::geom_vline(xintercept = 0, color = "red") +
        labs(title = "Prophet Additive histogram of residuals")

      Best_Actual_vs_Predicted <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
        geom_point() +
        ggtitle("Prophet_Additive Actual vs Predicted") +
        geom_abline(slope = 1, intercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Predicted")

      Best_Actual_vs_Residuals <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
        geom_point() +
        ggtitle("Prophet_Additive Actual vs Residuals") +
        geom_hline(yintercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Residuals")

      Best_Actual_vs_Trend <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = trend)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = trend), color = "red") +
        ggtitle("Prophet_Additive Actual (black) vs trend (red)")+
        xlab("Actual") +
        ylab("Trend")

      Best_Actual_vs_Seasonally_Adjusted <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = season_adjust)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = season_adjust), color = "red") +
        ggtitle("Prophet_Additive Actual (black) vs seasonally adjusted (red)")+
        xlab("Actual") +
        ylab("Seasonally adjusted value")
    }

    # Prophet Multiplicative Results ------------------------------------------

    if(value_forecast_accuracy[1,1] == "Prophet_Multiplicative"){

      Best_Model <- time_series_data %>%
        fabletools::model(
          Prophet_Multiplicative = fable.prophet::prophet(Value ~ season(period = 12, type = "multiplicative"))
        )

      augment(Best_Model) %>% drop_na()

      Best_Forecast <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast)

      Best_Forecast_plot <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast) %>%
        autoplot(Time_Series_test) +
        labs(title = "Prophet Multiplicative model three month forecast of total values")+
        scale_y_continuous(labels = scales::comma) +
        ylab("Total values")

      Best_Forecast_Min <-
        Best_Forecast$.mean - value_forecast_accuracy[1,4]

      Best_Forecast_Max <-
        Best_Forecast$.mean + value_forecast_accuracy[1,4]

      Best_STL_Decomposition <-
        time_series_data[2:nrow(time_series_data),] %>%
        fabletools::model(
          stl = STL(Value)
        ) %>%
        components()

      Best_STL_Decomposition_autoplot <- Best_STL_Decomposition %>% autoplot()

      Best_Innovation_Residuals <- augment(Best_Model) %>% drop_na() %>%
        autoplot(.innov) +
        geom_point() +
        geom_hline(yintercept = 0, color = "red") +
        labs(title = "Prophet Multiplicative innovation residuals")

      Best_ACF <- augment(Best_Model) %>% drop_na() %>%
        feasts::ACF(.innov) %>%
        autoplot() +
        labs(title = "Prophet Multiplicative Autocorrelation function")

      Best_Histogram_of_Residuals <- augment(Best_Model) %>% drop_na() %>%
        ggplot(aes(x = .resid)) +
        ggplot2::geom_histogram(bins = round(nrow(time_series_data)/5)) +
        ggplot2::geom_vline(xintercept = 0, color = "red") +
        labs(title = "Prophet Multiplicative histogram of residuals")
      Best_Actual_vs_Predicted <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
        geom_point() +
        ggtitle("Prophet_Multiplicative Actual vs Predicted") +
        geom_abline(slope = 1, intercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Predicted")

      Best_Actual_vs_Residuals <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
        geom_point() +
        ggtitle("Prophet_Multiplicative Actual vs Residuals") +
        geom_hline(yintercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Residuals")

      Best_Actual_vs_Trend <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = trend)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = trend), color = "red") +
        ggtitle("Prophet_Multiplicative Actual (black) vs trend (red)")+
        xlab("Actual") +
        ylab("Trend")

      Best_Actual_vs_Seasonally_Adjusted <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = season_adjust)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = season_adjust), color = "red") +
        ggtitle("Prophet_Multiplicative Actual (black) vs seasonally adjusted (red)")+
        xlab("Actual") +
        ylab("Seasonally adjusted value")
    }

    # SNaive Results ----------------------------------------------------------

    if(value_forecast_accuracy[1,1] == "SNaive"){

      Best_Model <- time_series_data %>%
        fabletools::model(
          SNaive = fable::SNAIVE(Value)
        )

      augment(Best_Model) %>% drop_na()

      Best_Forecast <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast)

      Best_Forecast_plot <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast) %>%
        autoplot(Time_Series_test) +
        labs(title = "Seasonal Naive model three month forecast of total values")+
        scale_y_continuous(labels = scales::comma) +
        ylab("Total values")

      Best_Forecast_Min <-
        Best_Forecast$.mean - value_forecast_accuracy[1,4]

      Best_Forecast_Max <-
        Best_Forecast$.mean + value_forecast_accuracy[1,4]

      Best_STL_Decomposition <-
        time_series_data[2:nrow(time_series_data),] %>%
        fabletools::model(
          stl = STL(Value)
        ) %>%
        components()

      Best_STL_Decomposition_autoplot <- Best_STL_Decomposition %>% autoplot()

      Best_Innovation_Residuals <- augment(Best_Model) %>% drop_na() %>%
        autoplot(.innov) +
        geom_point() +
        geom_hline(yintercept = 0, color = "red") +
        labs(title = "SNaive innovation residuals")

      Best_ACF <- augment(Best_Model) %>% drop_na() %>%
        feasts::ACF(.innov) %>%
        autoplot() +
        labs(title = "SNaive Autocorrelation function")

      Best_Histogram_of_Residuals <- augment(Best_Model) %>% drop_na() %>%
        ggplot(aes(x = .resid)) +
        ggplot2::geom_histogram(bins = round(nrow(time_series_data)/5)) +
        ggplot2::geom_vline(xintercept = 0, color = "red") +
        labs(title = "SNaive histogram of residuals")

      Best_Actual_vs_Predicted <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
        geom_point() +
        ggtitle("SNaive Actual vs Predicted") +
        geom_abline(slope = 1, intercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Predicted")

      Best_Actual_vs_Residuals <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
        geom_point() +
        ggtitle("SNaive Actual vs Residuals") +
        geom_hline(yintercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Residuals")

      Best_Actual_vs_Trend <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = trend)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = trend), color = "red") +
        ggtitle("SNaive Actual (black) vs trend (red)")+
        xlab("Actual") +
        ylab("Trend")

      Best_Actual_vs_Seasonally_Adjusted <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = season_adjust)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = season_adjust), color = "red") +
        ggtitle("SNaive Actual (black) vs seasonally adjusted (red)")+
        xlab("Actual") +
        ylab("Seasonally adjusted value")
    }

    # Stochastic Results ------------------------------------------------------

    if(value_forecast_accuracy[1,1] == "Stochastic"){

      Best_Model <- time_series_data %>%
        fabletools::model(
          Stochastic = fable::ARIMA(Value ~ pdq(d = 1), stepwise = TRUE, greedy = TRUE, approximation = TRUE)
        )

      augment(Best_Model) %>% drop_na()

      Best_Forecast <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast)

      Best_Forecast_plot <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast) %>%
        autoplot(Time_Series_test) +
        labs(title = "Stochastic model three month forecast of total values")+
        scale_y_continuous(labels = scales::comma) +
        ylab("Total values")

      Best_Forecast_Min <-
        Best_Forecast$.mean - value_forecast_accuracy[1,4]

      Best_Forecast_Max <-
        Best_Forecast$.mean + value_forecast_accuracy[1,4]

      Best_STL_Decomposition <-
        time_series_data[2:nrow(time_series_data),] %>%
        fabletools::model(
          stl = STL(Value)
        ) %>%
        components()

      Best_STL_Decomposition_autoplot <- Best_STL_Decomposition %>% autoplot()

      Best_Innovation_Residuals <- augment(Best_Model) %>% drop_na() %>%
        autoplot(.innov) +
        geom_point() +
        geom_hline(yintercept = 0, color = "red") +
        labs(title = "Stochastic innovation residuals")

      Best_ACF <- augment(Best_Model) %>% drop_na() %>%
        feasts::ACF(.innov) %>%
        autoplot() +
        labs(title = "Stochastic Autocorrelation function")

      Best_Histogram_of_Residuals <- augment(Best_Model) %>% drop_na() %>%
        ggplot(aes(x = .resid)) +
        ggplot2::geom_histogram(bins = round(nrow(time_series_data)/5)) +
        ggplot2::geom_vline(xintercept = 0, color = "red") +
        labs(title = "Stochastic histogram of residuals")

      Best_Actual_vs_Predicted <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
        geom_point() +
        ggtitle("Stochastic Actual vs Predicted") +
        geom_abline(slope = 1, intercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Predicted")

      Best_Actual_vs_Residuals <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
        geom_point() +
        ggtitle("Stochastic Actual vs Residuals") +
        geom_hline(yintercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Residuals")

      Best_Actual_vs_Trend <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = trend)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = trend), color = "red") +
        ggtitle("Stochastic Actual (black) vs trend (red)")+
        xlab("Actual") +
        ylab("Trend")

      Best_Actual_vs_Seasonally_Adjusted <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = season_adjust)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = season_adjust), color = "red") +
        ggtitle("Stochastic Actual (black) vs seasonally adjusted (red)")+
        xlab("Actual") +
        ylab("Seasonally adjusted value")
    }

    # VAR Results -------------------------------------------------------------

    if(value_forecast_accuracy[1,1] == "VAR"){

      Best_Model <- time_series_data %>%
        fabletools::model(
          VAR1 = fable::VAR(Value)
        )

      augment(Best_Model) %>% drop_na()

      Best_Forecast <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast)

      Best_Forecast_plot <- Best_Model %>%
        fabletools::forecast(h = number_of_intervals_to_forecast) %>%
        autoplot(Time_Series_test) +
        labs(title = "VAR model three month forecast of total values")+
        scale_y_continuous(labels = scales::comma) +
        ylab("Total values")

      Best_Forecast_Min <-
        Best_Forecast$.mean - value_forecast_accuracy[1,4]

      Best_Forecast_Max <-
        Best_Forecast$.mean + value_forecast_accuracy[1,4]

      Best_STL_Decomposition <-
        time_series_data[2:nrow(time_series_data),] %>%
        fabletools::model(
          stl = STL(Value)
        ) %>%
        components()

      Best_STL_Decomposition_autoplot <- Best_STL_Decomposition %>% autoplot()

      Best_Innovation_Residuals <- augment(Best_Model) %>% drop_na()[6:nrow(time_series_data),] %>%
        autoplot(.innov) +
        geom_point() +
        geom_hline(yintercept = 0, color = "red") +
        labs(title = "VAR innovation residuals")

      Best_ACF <- augment(Best_Model) %>% drop_na() %>%
        feasts::ACF(.innov) %>%
        autoplot() +
        labs(title = "VAR Autocorrelation function")

      Best_Histogram_of_Residuals <- augment(Best_Model) %>% drop_na()[6:nrow(time_series_data),] %>%
        ggplot(aes(x = .resid)) +
        ggplot2::geom_histogram(bins = round(nrow(time_series_data)/5)) +
        ggplot2::geom_vline(xintercept = 0, color = "red") +
        labs(title = "VAR histogram of residuals")

      Best_Actual_vs_Predicted <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .fitted)) +
        geom_point() +
        ggtitle("VAR Actual vs Predicted") +
        geom_abline(slope = 1, intercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Predicted")

      Best_Actual_vs_Residuals <-
        ggplot(augment(Best_Model) %>% drop_na(), mapping = aes(x = Value, y = .resid)) +
        geom_point() +
        ggtitle("VAR Actual vs Residuals") +
        geom_hline(yintercept = 0, color = "red")+
        xlab("Actual") +
        ylab("Residuals")

      Best_Actual_vs_Trend <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = trend)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = trend), color = "red") +
        ggtitle("VAR Actual (black) vs trend (red)")+
        xlab("Actual") +
        ylab("Trend")

      Best_Actual_vs_Seasonally_Adjusted <-
        ggplot(Best_STL_Decomposition, mapping = aes(x = Value, y = season_adjust)) +
        geom_line(mapping = aes(y = Value)) +
        geom_line(aes(y = season_adjust), color = "red") +
        ggtitle("VAR Actual (black) vs seasonally adjusted (red)")+
        xlab("Actual") +
        ylab("Seasonally adjusted value")
    }


    # 7. Summary Results for the model with lowest RMSE -----------------------

return(list('All_time_series_features' = all_time_series_features, 'Value forecast report' = value_forecast_report, 'Baseline_full' = Baseline_full, 'Best forecast min' = Best_Forecast_Min,
    'Best forecast max'= Best_Forecast_Max, 'Best_model' = Best_Model, 'time_series_quartiles' = time_series_quartiles,
    'tail_of_data' = table_of_value_tail, 'head_of_data' = table_of_value_head, 'trend' = plot_of_trend,
    'Seasonally_adjusted' = plot_of_seasonally_adjusted, 'Decomposition' = plot_of_decomposition, 'Anomolaies' = time_series_anomalies,
    'All_time_series_features' = all_time_series_features, 'Best_Forecast' = Best_Forecast, 'Best_forecast_plot' = Best_Forecast_plot,
    'Best_innovation_residuals' = Best_Innovation_Residuals, 'Best_ACF' = Best_ACF, 'Best Histogram of Residuals' = Best_Histogram_of_Residuals,
    'Best Actual vs Predicted' = Best_Actual_vs_Predicted, 'Best Actual vs Residual' = Best_Actual_vs_Residuals, 'Best Actual vs Trend' = Best_Actual_vs_Trend,
    'Best Actual vs Seasonally Adjusted' = Best_Actual_vs_Seasonally_Adjusted)
)
}
