# packages ----------------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(jsonlite)
library(stats)
library(tseries)
library(astsa)
library(forecast)
library(forecastHybrid)
library(prophet)
library(caret)
library(bsts)
library(patchwork)
library(Cairo)
library(ggpubr)
library(paletteer)

library(doParallel)

set.seed(202208)

remove(list = ls())

# data load ---------------------------------------------------------------

source("./script/theme_set.R")
source("./script/ggplot.R")

datafile_analysis <- read.xlsx("./data/nation_and_provinces.xlsx",
  detectDates = T, sheet = "Nation"
) |>
  filter(date >= as.Date("2008-1-1")) |>
  mutate(value = as.integer(value))

datafile_class <- read.xlsx("./outcome/appendix/Figure Data/Fig.1 data.xlsx",
  sheet = "panel A"
) |>
  select(-c(value, label))

split_date <- split_dates[1]
train_length <- 12 * seq(10, 6)
disease_name <- datafile_class$disease

datafile_length <- expand.grid(
  disease_name = disease_name,
  train_length = train_length
) |> 
  mutate(test_length = 12*12 - train_length)

# data clean --------------------------------------------------------------

i <- 1

auto_select_function <- function(i) {
  
  set.seed(202208)
  
  test_length <- datafile_length$test_length[i]
  train_length <- datafile_length$train_length[i]
  disease_name <- datafile_length$disease_name[i]
  
  datafile_single <- datafile_analysis |>
    filter(disease_en == disease_name) |>
    select(date, disease_en, value)

  ## Rubella outbreak from March 2019 to July 2019
  if (disease_name == "Rubella") {
    datafile_single$value[datafile_single$date >= as.Date("2019-01-01")] <- NA
  }

  ## simulate date before 2020
  df_simu <- datafile_single |>
    arrange(date) |>
    unique() |>
    filter(date < split_date) |>
    select(value)

  max_case <- max(df_simu$value, na.rm = T)

  ts_obse <- ts(df_simu,
    frequency = 12,
    start = c(
      as.numeric(format(min(datafile_single$date), "%Y")),
      as.numeric(format(min(datafile_single$date), "%m"))
    )
  )

  ts_train <- head(ts_obse, train_length) + add_value
  ts_test <- tail(ts_obse, test_length)

  # NNET --------------------------------------------------------------------

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

  fit_goodness <- data.frame(
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
  )

  rm(mod, outcome, outcome_plot_1, outcome_plot_2)

  # Prophet -------------------------------------------------------------------

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

  rm(mod, future, outcome, outcome_plot_1, outcome_plot_2)

  # ETS ---------------------------------------------------------------------

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

  rm(mod, outcome, outcome_plot_1, outcome_plot_2)

  # SARIMA -------------------------------------------------------------------

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

  rm(mod, outcome, outcome_plot_1, outcome_plot_2)

  # Mixture ts --------------------------------------------------------------

  mod <- hybridModel(ts_train,
    lambda = "auto",
    models = c("aesn"),
    a.args = list(seasonal = T),
    weights = "equal", parallel = TRUE, num.cores = 10,
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

  rm(mod, outcome, outcome_plot_1, outcome_plot_2)

  # Bayesian --------------------------------------------------------------
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

  fit_goodness$disease <- disease_name
  
  DataClean <- fit_goodness |>
    select(disease, Index, Method, Test) |>
    pivot_wider(names_from = Index, values_from = Test) |>
    group_by(disease) |>
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
    group_by(disease) |>
    mutate(
      Best = Method[which.max(Index)],
      train_length = train_length,
      test_length = test_length
    ) |>
    ungroup()
  DataClean$Best <- as.numeric(DataClean$Method == DataClean$Best)
  DataClean$Method <- factor(DataClean$Method, levels = models, labels = models_label)

  return(DataClean)
}

# run model ---------------------------------------------------------------

cl <- makeCluster(round(detectCores()*0.3))
registerDoParallel(cl)
clusterEvalQ(cl, {
  library(tidyverse)
  library(openxlsx)
  library(jsonlite)
  library(stats)
  library(tseries)
  library(astsa)
  library(forecast)
  library(greyforecasting)
  library(forecastHybrid)
  library(prophet)
  library(caret)
  library(bsts)
  library(patchwork)
  library(Cairo)
  library(ggpubr)
  library(paletteer)

  set.seed(202208)
})

clusterExport(cl, ls()[ls() != "cl"], envir = environment())
outcome <- parLapply(cl, 1:nrow(datafile_length), auto_select_function)
stopCluster(cl)

datafile_outcome <- do.call("rbind", outcome)
write.xlsx(datafile_outcome, "./outcome/appendix/Supplementary Appendix 2_4.xlsx")
