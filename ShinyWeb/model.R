# packages ----------------------------------------------------------------

library(stats)
library(tseries)
library(astsa)
library(forecast)
library(forecastHybrid)
library(prophet)
library(caret)
library(bsts)

set.seed(202208)

auto_forecast_function <- function(ts_train, test_length, add_value, model) {
     
     set.seed(202208)
     
     index_labels <- c("SMAPE", "RMSE", "MASE", "R_Squared")
     
     ts_train <- ts_train + add_value
     train_length <- length(ts_train)
     
     # NNET --------------------------------------------------------------------
     
     if ('Neural Network' == model){
          mod <- nnetar(ts_train, lambda = "auto")
          outcome <- forecast(mod, h = test_length)
          
          outcome_plot_1 <- data.frame(
               date = zoo::as.Date(time(outcome$x)),
               simu = as.numeric(as.matrix(outcome$x)) - add_value,
               fit = as.numeric(as.matrix(outcome$fitted)) - add_value
          )
          outcome_plot_2 <- data.frame(
               date = zoo::as.Date(time(outcome$mean)),
               mean = as.matrix(outcome$mean) - add_value
          )

          return(list(
               outcome_plot_1 = outcome_plot_1,
               outcome_plot_2 = outcome_plot_2,
               mod = mod
          ))
     }
     
     # Prophet -------------------------------------------------------------------
     
     if ('Prophet' == model) {
          mod <- prophet(
               data.frame(
                    ds = zoo::as.Date(time(ts_train)),
                    y = as.numeric(ts_train)
               ),
               interval.width = 0.95,
               weekly.seasonality = FALSE,
               daily.seasonality = FALSE
          )
          future <- make_future_dataframe(mod, periods = test_length, freq = "month")
          outcome <- predict(mod, future)
          
          outcome_plot_1 <- data.frame(
               date = as.Date(mod$history.dates),
               simu = as.numeric(ts_train) - add_value,
               fit = as.numeric(outcome$yhat[1:length(ts_train)]) - add_value
          )
          outcome_plot_2 <- data.frame(
               date = as.Date(outcome$ds),
               mean = as.numeric(outcome$yhat) - add_value,
               lower_80 = as.numeric(outcome$yhat_lower) - add_value,
               lower_95 = as.numeric(outcome$yhat_lower) - add_value,
               upper_80 = as.numeric(outcome$yhat_upper) - add_value,
               upper_95 = as.numeric(outcome$yhat_upper) - add_value
          ) |>
               tail(test_length)
          
          return(list(
               outcome_plot_1 = outcome_plot_1,
               outcome_plot_2 = outcome_plot_2,
               mod = mod
          ))
     }
     
     # ETS ---------------------------------------------------------------------
     
     if ('ETS' == model) {
          mod <- ets(ts_train, ic = "aicc", lambda = "auto")
          outcome <- forecast(mod, h = test_length)
          
          outcome_plot_1 <- data.frame(
               date = zoo::as.Date(time(outcome$x)),
               simu = as.numeric(as.matrix(outcome$x)) - add_value,
               fit = as.numeric(as.matrix(outcome$fitted)) - add_value
          )
          outcome_plot_2 <- data.frame(
               date = zoo::as.Date(time(outcome$mean)),
               mean = as.matrix(outcome$mean) - add_value,
               lower_80 = as.matrix(outcome$lower[, 1]) - add_value,
               lower_95 = as.matrix(outcome$lower[, 2]) - add_value,
               upper_80 = as.matrix(outcome$upper[, 1]) - add_value,
               upper_95 = as.matrix(outcome$upper[, 2]) - add_value
          )
          
          return(list(
               outcome_plot_1 = outcome_plot_1,
               outcome_plot_2 = outcome_plot_2,
               mod = mod
          ))
     }
     
     # SARIMA -------------------------------------------------------------------
     
     if ('SARIMA' == model) {
          mod <- auto.arima(ts_train, seasonal = T, ic = "aicc", lambda = "auto")
          outcome <- forecast(mod, h = test_length)
          
          outcome_plot_1 <- data.frame(
               date = zoo::as.Date(time(outcome$x)),
               simu = as.numeric(as.matrix(outcome$x)) - add_value,
               fit = as.numeric(as.matrix(outcome$fitted)) - add_value
          )
          outcome_plot_2 <- data.frame(
               date = zoo::as.Date(time(outcome$mean)),
               mean = as.matrix(outcome$mean) - add_value,
               lower_80 = as.matrix(outcome$lower[, 1]) - add_value,
               lower_95 = as.matrix(outcome$lower[, 2]) - add_value,
               upper_80 = as.matrix(outcome$upper[, 1]) - add_value,
               upper_95 = as.matrix(outcome$upper[, 2]) - add_value
          )
          
          return(list(
               outcome_plot_1 = outcome_plot_1,
               outcome_plot_2 = outcome_plot_2,
               mod = mod
          ))
     }
     
     # Mixture ts --------------------------------------------------------------
     
     if ('Hybrid' == model) {
          mod <- hybridModel(ts_train,
                             lambda = "auto",
                             models = c("aesn"),
                             a.args = list(seasonal = T),
                             weights = "equal", parallel = TRUE,
                             errorMethod = "MAE"
          )
          outcome <- forecast(mod, h = test_length)
          
          outcome_plot_1 <- data.frame(
               date = zoo::as.Date(time(outcome$x)),
               simu = as.numeric(as.matrix(outcome$x)) - add_value,
               fit = as.numeric(as.matrix(outcome$fitted)) - add_value
          )
          outcome_plot_2 <- data.frame(
               date = zoo::as.Date(time(outcome$mean)),
               mean = as.matrix(outcome$mean) - add_value,
               lower_80 = as.matrix(outcome$lower[, 1]) - add_value,
               lower_95 = as.matrix(outcome$lower[, 2]) - add_value,
               upper_80 = as.matrix(outcome$upper[, 1]) - add_value,
               upper_95 = as.matrix(outcome$upper[, 2]) - add_value
          )
          
          return(list(
               outcome_plot_1 = outcome_plot_1,
               outcome_plot_2 = outcome_plot_2,
               mod = mod
          ))
     }
     
     # Bayesian --------------------------------------------------------------
     
     if ("Bayesian structural" == model) {
          ss <- AddLocalLinearTrend(list(), ts_train)
          ss <- AddSeasonal(ss, ts_train, nseasons = 12)
          mod <- bsts(ts_train, state.specification = ss, niter = 500, seed = 20231007)
          
          burn <- SuggestBurn(0.1, mod)
          outcome <- predict.bsts(mod, horizon = test_length, burn = burn, quantiles = c(0.025, 0.1, 0.9, 0.975))
          
          outcome_plot_1 <- data.frame(
               date = zoo::as.Date(time(ts_train)),
               simu = as.numeric(ts_train) - add_value,
               fit = as.numeric(-colMeans(mod$one.step.prediction.errors[-(1:burn), ]) + ts_train) - add_value
          )
          outcome_plot_2 <- data.frame(
               date = seq.Date(from = as.Date(time(ts_train))[length(ts_train)],
                               by = "month",
                               length.out = test_length + 1)[-1],
               mean = outcome$mean - add_value,
               lower_80 = outcome$interval[2, ] - add_value,
               lower_95 = outcome$interval[1, ] - add_value,
               upper_80 = outcome$interval[3, ] - add_value,
               upper_95 = outcome$interval[4, ] - add_value
          )
          
          return(list(
               outcome_plot_1 = outcome_plot_1,
               outcome_plot_2 = outcome_plot_2,
               mod = mod
          ))
     }
}

auto_select_function <- function(ts_train, ts_test, ts_obse, add_value, models) {
     
     set.seed(202208)
     
     index_labels <- c("SMAPE", "RMSE", "MASE", "R_Squared")
     
     ts_train <- ts_train + add_value
     train_length <- length(ts_train)
     test_length <- length(ts_test)
     
     fit_goodness <- data.frame(
          Method = character(),
          Index = character(),
          Train = numeric(),
          Test = numeric(),
          "Train and Test" = numeric()
     )
     outcomes <- list()
     
     # NNET --------------------------------------------------------------------
     
     if ('Neural Network' %in% models){
          mod <- nnetar(ts_train, lambda = "auto")
          outcome <- forecast(mod, h = test_length)
          
          outcome_plot_1 <- data.frame(
               date = zoo::as.Date(time(outcome$x)),
               simu = as.numeric(as.matrix(outcome$x)) - add_value,
               fit = as.numeric(as.matrix(outcome$fitted)) - add_value
          )
          outcome_plot_2 <- data.frame(
               date = zoo::as.Date(time(outcome$mean)),
               mean = as.matrix(outcome$mean) - add_value
          )
          
          fit_goodness <- fit_goodness |>
               rbind(data.frame(
                    Method = "Neural Network",
                    Index = index_labels,
                    Train = evaluate_forecast(
                         outcome_plot_1$fit[!is.na(outcome_plot_1$fit)],
                         outcome_plot_1$simu[!is.na(outcome_plot_1$fit)]
                    ),
                    Test = evaluate_forecast(outcome_plot_2$mean, ts_test),
                    "Train and Test" = evaluate_forecast(c(
                         outcome_plot_1$fit[-which(is.na(outcome_plot_1$fit))],
                         outcome_plot_2$mean
                    ), ts_obse[-which(is.na(outcome_plot_1$fit))])
               ))
          outcomes[['Neural Network']] <- list(
               outcome_plot_1 = outcome_plot_1,
               outcome_plot_2 = outcome_plot_2
          )
          
          rm(mod, outcome, outcome_plot_1, outcome_plot_2)
     }
     
     # Prophet -------------------------------------------------------------------
     
     if ('Prophet' %in% models) {
          mod <- prophet(
               data.frame(
                    ds = zoo::as.Date(time(ts_train)),
                    y = as.numeric(ts_train)
               ),
               interval.width = 0.95,
               weekly.seasonality = FALSE,
               daily.seasonality = FALSE
          )
          future <- make_future_dataframe(mod, periods = test_length, freq = "month")
          outcome <- predict(mod, future)
          
          outcome_plot_1 <- data.frame(
               date = as.Date(mod$history.dates),
               simu = as.numeric(ts_train) - add_value,
               fit = as.numeric(outcome$yhat[1:length(ts_train)]) - add_value
          )
          outcome_plot_2 <- data.frame(
               date = as.Date(outcome$ds),
               mean = as.numeric(outcome$yhat) - add_value,
               lower_80 = as.numeric(outcome$yhat_lower) - add_value,
               lower_95 = as.numeric(outcome$yhat_lower) - add_value,
               upper_80 = as.numeric(outcome$yhat_upper) - add_value,
               upper_95 = as.numeric(outcome$yhat_upper) - add_value
          ) |>
               tail(test_length)
          
          fit_goodness <- fit_goodness |>
               rbind(
                    data.frame(
                         Method = "Prophet",
                         Index = index_labels,
                         Train = evaluate_forecast(outcome_plot_1$simu, outcome_plot_1$fit),
                         Test = evaluate_forecast(outcome_plot_2$mean, ts_test),
                         "Train and Test" = evaluate_forecast(c(outcome_plot_1$fit, outcome_plot_2$mean), ts_obse)
                    )
               )
          outcomes[['Prophet']] <- list(
               outcome_plot_1 = outcome_plot_1,
               outcome_plot_2 = outcome_plot_2,
               mod = mod
          )
          
          rm(mod, future, outcome, outcome_plot_1, outcome_plot_2)
     }
     
     # ETS ---------------------------------------------------------------------
     
     if ('ETS' %in% models) {
          mod <- ets(ts_train, ic = "aicc", lambda = "auto")
          outcome <- forecast(mod, h = test_length)
          
          outcome_plot_1 <- data.frame(
               date = zoo::as.Date(time(outcome$x)),
               simu = as.numeric(as.matrix(outcome$x)) - add_value,
               fit = as.numeric(as.matrix(outcome$fitted)) - add_value
          )
          outcome_plot_2 <- data.frame(
               date = zoo::as.Date(time(outcome$mean)),
               mean = as.matrix(outcome$mean) - add_value,
               lower_80 = as.matrix(outcome$lower[, 1]) - add_value,
               lower_95 = as.matrix(outcome$lower[, 2]) - add_value,
               upper_80 = as.matrix(outcome$upper[, 1]) - add_value,
               upper_95 = as.matrix(outcome$upper[, 2]) - add_value
          )
          
          fit_goodness <- fit_goodness |>
               rbind(
                    data.frame(
                         Method = "ETS",
                         Index = index_labels,
                         Train = evaluate_forecast(outcome_plot_1$simu, outcome_plot_1$fit),
                         Test = evaluate_forecast(outcome_plot_2$mean, ts_test),
                         "Train and Test" = evaluate_forecast(c(outcome_plot_1$fit, outcome_plot_2$mean), ts_obse)
                    )
               )
          outcomes[['ETS']] <- list(
               outcome_plot_1 = outcome_plot_1,
               outcome_plot_2 = outcome_plot_2,
               mod = mod
          )
          
          rm(mod, outcome, outcome_plot_1, outcome_plot_2)
     }
     
     # SARIMA -------------------------------------------------------------------
     
     if ('SARIMA' %in% models) {
          mod <- auto.arima(ts_train, seasonal = T, ic = "aicc", lambda = "auto")
          outcome <- forecast(mod, h = test_length)
          
          outcome_plot_1 <- data.frame(
               date = zoo::as.Date(time(outcome$x)),
               simu = as.numeric(as.matrix(outcome$x)) - add_value,
               fit = as.numeric(as.matrix(outcome$fitted)) - add_value
          )
          outcome_plot_2 <- data.frame(
               date = zoo::as.Date(time(outcome$mean)),
               mean = as.matrix(outcome$mean) - add_value,
               lower_80 = as.matrix(outcome$lower[, 1]) - add_value,
               lower_95 = as.matrix(outcome$lower[, 2]) - add_value,
               upper_80 = as.matrix(outcome$upper[, 1]) - add_value,
               upper_95 = as.matrix(outcome$upper[, 2]) - add_value
          )
          
          fit_goodness <- fit_goodness |>
               rbind(
                    data.frame(
                         Method = "SARIMA",
                         Index = index_labels,
                         Train = evaluate_forecast(outcome_plot_1$fit, outcome_plot_1$simu),
                         Test = evaluate_forecast(outcome_plot_2$mean, ts_test),
                         "Train and Test" = evaluate_forecast(c(outcome_plot_1$fit, outcome_plot_2$mean), ts_obse)
                    )
               )
          outcomes[['SARIMA']] <- list(
               outcome_plot_1 = outcome_plot_1,
               outcome_plot_2 = outcome_plot_2,
               mod = mod
          )
          
          rm(mod, outcome, outcome_plot_1, outcome_plot_2)
     }
     
     # Mixture ts --------------------------------------------------------------
     
     if ('Hybrid' %in% models) {
          mod <- hybridModel(ts_train,
                             lambda = "auto",
                             models = c("aesn"),
                             a.args = list(seasonal = T),
                             weights = "equal", parallel = TRUE,
                             errorMethod = "MAE"
          )
          outcome <- forecast(mod, h = test_length)
          
          outcome_plot_1 <- data.frame(
               date = zoo::as.Date(time(outcome$x)),
               simu = as.numeric(as.matrix(outcome$x)) - add_value,
               fit = as.numeric(as.matrix(outcome$fitted)) - add_value
          )
          outcome_plot_2 <- data.frame(
               date = zoo::as.Date(time(outcome$mean)),
               mean = as.matrix(outcome$mean) - add_value,
               lower_80 = as.matrix(outcome$lower[, 1]) - add_value,
               lower_95 = as.matrix(outcome$lower[, 2]) - add_value,
               upper_80 = as.matrix(outcome$upper[, 1]) - add_value,
               upper_95 = as.matrix(outcome$upper[, 2]) - add_value
          )
          
          fit_goodness <- fit_goodness |>
               rbind(
                    data.frame(
                         Method = "Hybrid",
                         Index = index_labels,
                         Train = evaluate_forecast(
                              outcome_plot_1$simu[!is.na(outcome_plot_1$fit)],
                              outcome_plot_1$fit[!is.na(outcome_plot_1$fit)]
                         ),
                         Test = evaluate_forecast(outcome_plot_2$mean, ts_test),
                         "Train and Test" = evaluate_forecast(c(
                              outcome_plot_1$fit[-which(is.na(outcome_plot_1$fit))],
                              outcome_plot_2$mean
                         ), ts_obse[-which(is.na(outcome_plot_1$fit))])
                    )
               )
          outcomes[['Hybrid']] <- list(
               outcome_plot_1 = outcome_plot_1,
               outcome_plot_2 = outcome_plot_2,
               mod = mod
          )
          
          rm(mod, outcome, outcome_plot_1, outcome_plot_2)
     }
     
     # Bayesian --------------------------------------------------------------
     
     if ("Bayesian structural" %in% models) {
          ss <- AddLocalLinearTrend(list(), ts_train)
          ss <- AddSeasonal(ss, ts_train, nseasons = 12)
          mod <- bsts(ts_train, state.specification = ss, niter = 500, seed = 20231007)
          
          burn <- SuggestBurn(0.1, mod)
          outcome <- predict.bsts(mod, horizon = test_length, burn = burn, quantiles = c(0.025, 0.1, 0.9, 0.975))
          
          outcome_plot_1 <- data.frame(
               date = zoo::as.Date(time(ts_train)),
               simu = as.numeric(ts_train) - add_value,
               fit = as.numeric(-colMeans(mod$one.step.prediction.errors[-(1:burn), ]) + ts_train) - add_value
          )
          outcome_plot_2 <- data.frame(
               date = as.Date(time(ts_test)),
               mean = outcome$mean - add_value,
               lower_80 = outcome$interval[2, ] - add_value,
               lower_95 = outcome$interval[1, ] - add_value,
               upper_80 = outcome$interval[3, ] - add_value,
               upper_95 = outcome$interval[4, ] - add_value
          )
          
          fit_goodness <- fit_goodness |>
               rbind(
                    data.frame(
                         Method = "Bayesian Structural",
                         Index = index_labels,
                         Train = evaluate_forecast(
                              outcome_plot_1$simu[!is.na(outcome_plot_1$fit)],
                              outcome_plot_1$fit[!is.na(outcome_plot_1$fit)]
                         ),
                         Test = evaluate_forecast(outcome_plot_2$mean, ts_test),
                         "Train and Test" = evaluate_forecast(
                              c(outcome_plot_1$fit, outcome_plot_2$mean),
                              ts_obse
                         )
                    )
               )
          outcomes[['Bayesian structural']] <- list(
               outcome_plot_1 = outcome_plot_1,
               outcome_plot_2 = outcome_plot_2,
               mod = mod
          )
          
          rm(ss, mod, burn, outcome, outcome_plot_1, outcome_plot_2)
     }

     outcomes[['goodness']] <- fit_goodness
     
     return(outcomes)
}

evaluate_forecast <- function(actual, forecast) {
     # find na values
     na_value <- is.na(actual) | is.na(forecast)
     
     # drop na values
     actual <- actual[!na_value]
     forecast <- forecast[!na_value]
     
     smape <- mean(200 * abs(actual - forecast) / (abs(actual) + abs(forecast)))
     rmse <- sqrt(mean((actual - forecast) ^ 2))
     mase <- mean(abs(actual - forecast)) / mean(abs(diff(actual)))
     
     correlation <- cor(actual, forecast)
     r_squared <- correlation^2
     
     return(c('SMAPE' = smape, 'RMSE' = rmse, 'MASE' = mase, 'R_Squared' = r_squared))
}

get_norm_index <- function(DataClean) {
     DataClean <- DataClean |>
          select(Index, Method, Test) |>
          pivot_wider(names_from = Index, values_from = Test) |>
          mutate(
               norSMAPE = -(SMAPE - mean(SMAPE, na.rm = T)) / sd(SMAPE, na.rm = T),
               norRMSE = -(RMSE - mean(RMSE, na.rm = T)) / sd(RMSE, na.rm = T),
               norMASE = -(MASE - mean(MASE, na.rm = T)) / sd(MASE, na.rm = T),
               Index = sum(norSMAPE, norRMSE, norMASE, na.rm = T)
          ) |>
          rowwise() |>
          mutate(
               Index = sum(c_across(norSMAPE:norMASE), na.rm = T)
          ) |>
          ungroup() |>
          mutate(
               Best = Method[which.max(Index)]
          ) |>
          ungroup()
     DataClean$Best <- as.numeric(DataClean$Method == DataClean$Best)
     
     return(DataClean)
}


detect_frequency <- function(date_vector) {
     date_diff <- diff(date_vector)
     min_diff <- as.integer(min(date_diff, na.rm = TRUE))
     if (min_diff %in% 365:366) {
          return(365.25)
     } else if (min_diff %in% 28:31) {
          return(12)
     } else {
          return(1)
     }
}
