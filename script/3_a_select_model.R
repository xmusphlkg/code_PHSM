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
train_length <- 12 * 10
test_length <- 12 * 2
forcast_length <- 12 * 4

disease_name <- datafile_class$disease

# data clean --------------------------------------------------------------

i <- 20

auto_select_function <- function(i) {
  set.seed(202208)
  datafile_single <- datafile_analysis |>
    filter(disease_en == disease_name[i]) |>
    select(date, disease_en, value)
  
  ## Rubella outbreak from March 2019 to July 2019
  if (disease_name[i] == 'Rubella') {
    datafile_single$value[datafile_single$date >= as.Date('2019-01-01')] <- NA
  }

  ## simulate date before 2020
  df_simu <- datafile_single  |>
    arrange(date)  |>
    unique()  |>
    filter(date < split_date)  |>
    select(value)

  max_case <- max(df_simu$value, na.rm = T)

  ts_obse <- ts(df_simu, frequency = 12,
                  start = c(as.numeric(format(min(datafile_single$date), "%Y")),
                            as.numeric(format(min(datafile_single$date), "%m"))))
  
  ts_train <- head(ts_obse, train_length) + add_value
  ts_test <- tail(ts_obse, test_length)

  # NNET --------------------------------------------------------------------

  mod <- nnetar(ts_train, lambda = 'auto')
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

  fig_nnet_1 <- plot_outcome(
    outcome_plot_1,
    outcome_plot_2,
    datafile_single,
    split_date,
    max_case,
    1,
    F,
    "Neural Network"
  )
  rm(mod, outcome, outcome_plot_1, outcome_plot_2)

  # Prophet -------------------------------------------------------------------

  mod <- prophet(data.frame(
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

  fig_prophet_1 <- plot_outcome(
    outcome_plot_1,
    outcome_plot_2,
    datafile_single,
    split_date,
    max_case,
    2,
    T,
    "Prophet"
  )

  rm(mod, future, outcome, outcome_plot_1, outcome_plot_2)

  # ETS ---------------------------------------------------------------------

  mod <- ets(ts_train, ic = 'aicc', lambda = 'auto')
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

  fig_ets_1 <- plot_outcome(
    outcome_plot_1,
    outcome_plot_2,
    datafile_single,
    split_date,
    max_case,
    3,
    T,
    "ETS"
  )

  rm(mod, outcome, outcome_plot_1, outcome_plot_2)

  # SARIMA -------------------------------------------------------------------

  mod <- auto.arima(ts_train, seasonal = T, ic = 'aicc', lambda = 'auto')
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

  fig_sarima_1 <- plot_outcome(
    outcome_plot_1,
    outcome_plot_2,
    datafile_single,
    split_date,
    max_case,
    4,
    T,
    "SARIMA"
  )
  rm(mod, outcome, outcome_plot_1, outcome_plot_2)

  # Mixture ts --------------------------------------------------------------

  mod <- hybridModel(ts_train,
                     lambda = 'auto',
                     models = c("aesn"),
                     a.args = list(seasonal = T),
                     weights = "equal", parallel = TRUE, num.cores = 10,
                     errorMethod = 'MAE')
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

  fig_hyb_1 <- plot_outcome(
    outcome_plot_1,
    outcome_plot_2,
    datafile_single,
    split_date,
    max_case,
    5,
    T,
    "Hybrid"
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

  fig_baye_1 <- plot_outcome(
    outcome_plot_1,
    outcome_plot_2,
    datafile_single,
    split_date,
    max_case,
    6,
    T,
    "Bayesian Structural"
  )
  rm(mod, outcome, outcome_plot_1, outcome_plot_2)

  # summary table ---------------------------------------------------------

  datafile_table <- fit_goodness |>
    mutate(
      Method = factor(Method, levels = models, labels = models_label),
      Train = round(Train, 2),
      Test = round(Test, 2),
      All = round(Train.and.Test, 2)
    ) |>
    arrange(Method) |>
    select(Method, Train, Test, All, Index)
  datafile_table[is.na(datafile_table)] <- ""
  
  table_build <- function(datafile_table, i) {
    index <- index_labels[i]
    data <- datafile_table[datafile_table$Index == index, 1:4]
    ggtexttable(data,
                rows = NULL,
                cols = c("Method", "Train", "Test", "All"),
                theme = ttheme("blank", base_size = 10, padding = unit(c(5, 5), "mm"))) |>
         tab_add_hline(at.row = nrow(datafile_table) / 4 + 1, row.side = "bottom", linewidth = 2) |>
         tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 2) |>
         tab_add_title(paste(LETTERS[i+6], ":", index, " of Models"), face = "bold", size = 14) |>
         tab_add_footnote("*Hybrid: Combined SARIMA, ETS, STL\nand Neural Network model",
                          just = "left", hjust = 1, size = 10)
  }
  fig_table <- lapply(1:length(index_labels), table_build, datafile_table = datafile_table) |> 
       wrap_plots(plot_list, nrow = 1)

  # save --------------------------------------------------------------------

  fig_ts <- fig_nnet_1 + fig_prophet_1 + fig_ets_1 + fig_sarima_1 + fig_hyb_1 + fig_baye_1 +
    plot_layout(ncol = 2, guides = "collect") &
    theme(
      legend.position = "bottom",
      plot.margin = margin(5, 15, 5, 5)
    )

  fig <- cowplot::plot_grid(fig_ts, fig_table, ncol = 1, rel_heights = c(3, 1))

  ggsave(
    filename = paste0("./outcome/appendix/Supplementary Appendix 1_2/", disease_name[i], ".png"),
    fig,
    device = 'png',
    width = 14, height = 15,
    limitsize = FALSE,
    dpi = 300
  )
  fit_goodness$disease <- disease_name[i]

  return(fit_goodness)
}

# run model ---------------------------------------------------------------

i <- 6
# lapply(1:26, auto_select_function)
# auto_select_function(6)

cl <- makeCluster(24)
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
outcome <- parLapply(cl, 1:24, auto_select_function)
stopCluster(cl)

datafile_outcome <- do.call("rbind", outcome)
write.xlsx(datafile_outcome, "./outcome/appendix/Supplementary Appendix 2.xlsx")
