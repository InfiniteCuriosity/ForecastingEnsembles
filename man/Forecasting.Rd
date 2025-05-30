% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Forecasting.R
\name{Forecasting}
\alias{Forecasting}
\title{forecasting—function to perform time series analysis and return the results to the user.}
\usage{
Forecasting(
  time_series_name,
  time_series_data,
  train_amount,
  number_of_intervals_to_forecast,
  use_parallel = c("Y", "N"),
  time_interval = c("Q", "M", "W")
)
}
\arguments{
\item{time_series_name}{the name of the time series in quotation marks}

\item{time_series_data}{a time series}

\item{train_amount}{The amount to use for the training set, such as 0.60}

\item{number_of_intervals_to_forecast}{the number of intervals, such as months or weeks, that are going to be forecast}

\item{use_parallel}{"Y" or "N" for parallel processing}

\item{time_interval}{user states whether the time interval is quarterly, monthly or weekly.}
}
\value{
A series of summary reports and visualizations to fully describe the time series: Forecast accuracy, forecast numbers, forecast plot, innovation residuals,

best autocorrelation function (ACF), plot of best histogram of residuals, plot of best actual vs predicted, plot of best actual vs trend

plot of best actual vs seasonally adjusted
}
\description{
forecasting—function to perform time series analysis and return the results to the user.
}
