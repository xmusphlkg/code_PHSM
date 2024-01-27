
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

source('./script/theme_set.R')
source('./script/ggplot.R')

datafile_analysis <- read.xlsx('./data/nation_and_provinces.xlsx',
                               detectDates = T, sheet = 'Nation')|>
     filter(date >= as.Date("2008-1-1")) |> 
     mutate(value = as.integer(value))

datafile_class <- read.xlsx('./outcome/appendix/data/Fig.1 data.xlsx',
                            sheet = 'panel A') |> 
     select(-c(value, label))

split_date <- as.Date("2019/12/1")
train_length <- 12*10
test_length <- 12*2
forcast_length <- 12*4

scientific_10 <- function(x) {
     ifelse(x == 0, 0, parse(text = gsub("[+]", "", gsub("e", "%*%10^", scales::scientific_format()(x)))))
}

disease_name <- datafile_class$disease

# data clean --------------------------------------------------------------

i <- 1

auto_select_function <- function(i){
     set.seed(202208)
     datafile_single <- datafile_analysis|> 
          filter(disease_en == disease_name[i])|> 
          select(date, disease_en, value) |> 
          complete(
               date = seq.Date(
                    from = min(date),
                    to = max(date),
                    by = 'month'
               ),
               fill = list(value = 0,
                           disease_en = disease_name[i])
          )
     
     ## simulate date before 2020
     df_simu <- datafile_single  %>% 
          arrange(date) %>% 
          unique() %>% 
          filter(date <= split_date)%>% 
          select(value)
     
     max_case <- max(df_simu$value)
     
     ts_obse_1 <- df_simu %>% 
          ts(frequency = 12,
             start = c(as.numeric(format(min(datafile_single$date), "%Y")),
                       as.numeric(format(min(datafile_single$date), "%m"))))
     
     ts_train_1 <- head(ts_obse_1, train_length)
     ts_test_1 <- tail(ts_obse_1, test_length)
     
     # NNET --------------------------------------------------------------------
     
     mod <- nnetar(ts_train_1)
     outcome <- forecast(mod, h = test_length)
     
     outcome_plot_1 <- data.frame(
          date = zoo::as.Date(time(outcome$x)),
          simu = as.numeric(as.matrix(outcome$x)),
          fit = as.numeric(as.matrix(outcome$fitted))
     )
     outcome_plot_2 <- data.frame(
          date = zoo::as.Date(time(outcome$mean)),
          mean = as.matrix(outcome$mean)
     )
     
     fit_goodness <- data.frame(
          Method = 'Neural Network',
          Index = c('SMAPE', 'RMSE', 'MASE', 'R_Squared'),
          Train = evaluate_forecast(outcome_plot_1$fit[!is.na(outcome_plot_1$fit)],
                                    outcome_plot_1$simu[!is.na(outcome_plot_1$fit)]),
          Test = evaluate_forecast(outcome_plot_2$mean, ts_test_1),
          'Train and Test' = evaluate_forecast(c(outcome_plot_1$fit[-which(is.na(outcome_plot_1$fit))],
                                                 outcome_plot_2$mean), ts_obse_1[-which(is.na(outcome_plot_1$fit))])
     )
     
     fig_nnet_1 <- plot_outcome(
          outcome_plot_1,
          outcome_plot_2,
          datafile_single,
          split_date,
          max_case,
          1,
          F,
          'Neural Network'
     )
     rm(mod, outcome, outcome_plot_1, outcome_plot_2)
     
     # Prophet -------------------------------------------------------------------
     
     mod <- prophet(data.frame(ds = zoo::as.Date(time(ts_train_1)), y = as.numeric(ts_train_1)),
                    interval.width = 0.95,
                    weekly.seasonality = FALSE,
                    daily.seasonality = FALSE)
     future <- make_future_dataframe(mod, periods = test_length, freq = "month")
     forecast <- predict(mod, future)
     
     outcome_plot_1 <- data.frame(
          date = as.Date(mod$history.dates),
          simu = as.numeric(ts_train_1),
          fit = as.numeric(forecast$yhat[1:length(ts_train_1)])
     )
     outcome_plot_2 <- data.frame(
          date = as.Date(forecast$ds),
          mean = as.numeric(forecast$yhat),
          lower_80 = as.numeric(forecast$yhat_lower),
          lower_95 = as.numeric(forecast$yhat_lower),
          upper_80 = as.numeric(forecast$yhat_upper),
          upper_95 = as.numeric(forecast$yhat_upper)
     ) |> 
          tail(test_length)
     
     fit_goodness <- fit_goodness |>
          rbind(
               data.frame(
                    Method = 'Prophet',
                    Index = c('SMAPE', 'RMSE', 'MASE', 'R_Squared'),
                    Train = evaluate_forecast(outcome_plot_1$simu, outcome_plot_1$fit),
                    Test = evaluate_forecast(outcome_plot_2$mean, ts_test_1),
                    'Train and Test' = evaluate_forecast(c(outcome_plot_1$fit, outcome_plot_2$mean), ts_obse_1)
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
          'Prophet'
     )
     
     rm(mod, future, forecast, outcome_plot_1, outcome_plot_2)
     
     # ETS ---------------------------------------------------------------------
     
     mod <- ets(ts_train_1)
     outcome <- forecast(mod, h=test_length)
     
     outcome_plot_1 <- data.frame(
          date = zoo::as.Date(time(outcome$x)),
          simu = as.numeric(as.matrix(outcome$x)),
          fit = as.numeric(as.matrix(outcome$fitted))
     )
     outcome_plot_2 <- data.frame(
          date = zoo::as.Date(time(outcome$mean)),
          mean = as.matrix(outcome$mean),
          lower_80 = as.matrix(outcome$lower[,1]),
          lower_95 = as.matrix(outcome$lower[,2]),
          upper_80 = as.matrix(outcome$upper[,1]),
          upper_95 = as.matrix(outcome$upper[,2])
     )
     
     fit_goodness <- fit_goodness |> 
          rbind(
               data.frame(
                    Method = 'ETS',
                    Index = c('SMAPE', 'RMSE', 'MASE', 'R_Squared'),
                    Train = evaluate_forecast(outcome_plot_1$simu, outcome_plot_1$fit),
                    Test = evaluate_forecast(outcome_plot_2$mean, ts_test_1),
                    'Train and Test' = evaluate_forecast(c(outcome_plot_1$fit, outcome_plot_2$mean), ts_obse_1))
          )
     
     fig_ets_1 <- plot_outcome(
          outcome_plot_1,
          outcome_plot_2,
          datafile_single,
          split_date,
          max_case,
          3,
          T,
          'ETS'
     )
     
     rm(mod, outcome, outcome_plot_1, outcome_plot_2)
     
     # SARIMA -------------------------------------------------------------------
     
     mod <- auto.arima(ts_train_1, seasonal = T)
     outcome <- forecast(mod, h = test_length)
     
     outcome_plot_1 <- data.frame(
          date = zoo::as.Date(time(outcome$x)),
          simu = as.numeric(as.matrix(outcome$x)),
          fit = as.numeric(as.matrix(outcome$fitted))
     )
     outcome_plot_2 <- data.frame(
          date = zoo::as.Date(time(outcome$mean)),
          mean = as.matrix(outcome$mean),
          lower_80 = as.matrix(outcome$lower[,1]),
          lower_95 = as.matrix(outcome$lower[,2]),
          upper_80 = as.matrix(outcome$upper[,1]),
          upper_95 = as.matrix(outcome$upper[,2])
     )
     
     fit_goodness <- fit_goodness |> 
          rbind(
               data.frame(
                    Method = 'SARIMA',
                    Index = c('SMAPE', 'RMSE', 'MASE', 'R_Squared'),
                    Train = evaluate_forecast(outcome_plot_1$fit, outcome_plot_1$simu),
                    Test = evaluate_forecast(outcome_plot_2$mean, ts_test_1),
                    'Train and Test' = evaluate_forecast(c(outcome_plot_1$fit, outcome_plot_2$mean), ts_obse_1)
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
          'SARIMA'
     )
     rm(mod, outcome, outcome_plot_1, outcome_plot_2)
     
     # Mixture ts --------------------------------------------------------------
     
     mod <- hybridModel(ts_train_1, 
                        models = c('aesn'),
                        a.args = list(seasonal = T),
                        weights="equal", parallel=TRUE, num.cores = 10)
     outcome <- forecast(mod, h = test_length)
     
     outcome_plot_1 <- data.frame(
          date = zoo::as.Date(time(outcome$x)),
          simu = as.numeric(as.matrix(outcome$x)),
          fit = as.numeric(as.matrix(outcome$fitted))
     )
     outcome_plot_2 <- data.frame(
          date = zoo::as.Date(time(outcome$mean)),
          mean = as.matrix(outcome$mean),
          lower_80 = as.matrix(outcome$lower[,1]),
          lower_95 = as.matrix(outcome$lower[,2]),
          upper_80 = as.matrix(outcome$upper[,1]),
          upper_95 = as.matrix(outcome$upper[,2])
     )
     
     fit_goodness <- fit_goodness |> 
          rbind(
               data.frame(
                    Method = 'Hybrid',
                    Index = c('SMAPE', 'RMSE', 'MASE', 'R_Squared'),
                    Train = evaluate_forecast(outcome_plot_1$simu[!is.na(outcome_plot_1$fit)],
                                              outcome_plot_1$fit[!is.na(outcome_plot_1$fit)]),
                    Test = evaluate_forecast(outcome_plot_2$mean, ts_test_1),
                    'Train and Test' = evaluate_forecast(c(outcome_plot_1$fit[-which(is.na(outcome_plot_1$fit))],
                                                           outcome_plot_2$mean), ts_obse_1[-which(is.na(outcome_plot_1$fit))]))
          )
     
     fig_hyb_1 <- plot_outcome(
          outcome_plot_1,
          outcome_plot_2,
          datafile_single,
          split_date,
          max_case,
          5,
          T,
          'Hybrid'
     )
     rm(mod, outcome, outcome_plot_1)
     
     # Bayesian --------------------------------------------------------------
     ss <- AddLocalLinearTrend(list(), ts_train_1)
     ss <- AddSeasonal(ss, ts_train_1, nseasons = 12)
     mod <- bsts(ts_train_1, state.specification = ss, niter = 500, seed = 20231007)
     
     burn <- SuggestBurn(0.1, mod)
     outcome <- predict.bsts(mod, horizon = test_length, burn = burn, quantiles = c(0.025, 0.1, 0.9, 0.975))
     
     outcome_plot_1 <- data.frame(
          date = zoo::as.Date(time(ts_train_1)),
          simu = as.numeric(ts_train_1),
          fit = as.numeric(-colMeans(mod$one.step.prediction.errors[-(1:burn),])+ts_train_1)
     )
     outcome_plot_2 <- data.frame(
          date = outcome_plot_2$date,
          mean = outcome$mean,
          lower_80 = outcome$interval[2,],
          lower_95 = outcome$interval[1,],
          upper_80 = outcome$interval[3,],
          upper_95 = outcome$interval[4,]
     )
     
     fit_goodness <- fit_goodness |> 
          rbind(
               data.frame(
                    Method = 'Bayesian Structural',
                    Index = c('SMAPE', 'RMSE', 'MASE', 'R_Squared'),
                    Train = evaluate_forecast(outcome_plot_1$simu[!is.na(outcome_plot_1$fit)],
                                              outcome_plot_1$fit[!is.na(outcome_plot_1$fit)]),
                    Test = evaluate_forecast(outcome_plot_2$mean, ts_test_1),
                    'Train and Test' = evaluate_forecast(c(outcome_plot_1$fit, outcome_plot_2$mean),
                                                         ts_obse_1)
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
          'Bayesian Structural'
     )
     rm(mod, outcome, outcome_plot_1, outcome_plot_2)
     
     # summary table ---------------------------------------------------------
     
     datafile_table <- fit_goodness |>
          mutate(Method = factor(Method,
                                 levels = c('Neural Network', 'Prophet', 
                                            'ETS', 'SARIMA', 'Hybrid', 'Bayesian Structural'),
                                 labels = c('Neural Network', 'Prophet', 
                                            'ETS', 'SARIMA', 'Hybrid*', 'Bayesian Structural')),
                 Train = round(Train, 2),
                 Test = round(Test, 2),
                 All = round(Train.and.Test, 2)) |> 
          arrange(Method) |> 
          select(Method, Train, Test, All, Index)
     datafile_table[is.na(datafile_table)] <- ""
     
     table1 <- ggtexttable(datafile_table[datafile_table$Index == "RMSE", 1:4],
                           rows = NULL,
                           cols = c('Method', 'Train', 'Test', 'All'),
                           theme = ttheme("blank", base_size = 10, padding = unit(c(5, 5), "mm"))) |>
          tab_add_hline(at.row = nrow(datafile_table)/4+1, row.side = "bottom", linewidth = 2) |>
          tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 2) |> 
          tab_add_title(paste0(LETTERS[8], " : RMSE of Models"), face = "bold", size = 14) |> 
          tab_add_footnote('*Hybrid: Combined SARIMA, ETS, STL\nand Neural Network model', 
                           just = "left",hjust = 1,size = 10)
     table2 <- ggtexttable(datafile_table[datafile_table$Index == "SMAPE", 1:4],
                           rows = NULL,
                           cols = c('Method', 'Train', 'Test', 'All'),
                           theme = ttheme("blank", base_size = 10, padding = unit(c(5, 5), "mm"))) |>
          tab_add_hline(at.row = nrow(datafile_table)/4+1, row.side = "bottom", linewidth = 2) |>
          tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 2) |> 
          tab_add_title(paste0(LETTERS[9], " : SMAPE of Models"), face = "bold", size = 14) |> 
          tab_add_footnote('*Hybrid: Combined SARIMA, ETS, STL\nand Neural Network model', 
                           just = "left",hjust = 1,size = 10)
     table3 <- ggtexttable(datafile_table[datafile_table$Index == "MASE", 1:4],
                           rows = NULL,
                           cols = c('Method', 'Train', 'Test', 'All'),
                           theme = ttheme("blank", base_size = 10, padding = unit(c(5, 5), "mm"))) |>
          tab_add_hline(at.row = nrow(datafile_table)/4+1, row.side = "bottom", linewidth = 2) |>
          tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 2) |> 
          tab_add_title(paste0(LETTERS[10], " : MASE of Models"), face = "bold", size = 14) |> 
          tab_add_footnote('*Hybrid: Combined SARIMA, ETS, STL\nand Neural Network model', 
                           just = "left",hjust = 1,size = 10)
     table4 <- ggtexttable(datafile_table[datafile_table$Index == "R_Squared", 1:4],
                           rows = NULL,
                           cols = c('Method', 'Train', 'Test', 'All'),
                           theme = ttheme("blank", base_size = 10, padding = unit(c(5, 5), "mm"))) |>
          tab_add_hline(at.row = nrow(datafile_table)/4+1, row.side = "bottom", linewidth = 2) |>
          tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 2) |> 
          tab_add_title(paste0(LETTERS[11], " : R-squared of Models"), face = "bold", size = 14) |> 
          tab_add_footnote('*Hybrid: Combined SARIMA, ETS, STL\nand Neural Network model', 
                           just = "left",hjust = 1,size = 10)
     
     fig_table <- table1 + table2 + table3 + table4 +
          plot_layout(ncol = 4)
     
     # save --------------------------------------------------------------------
     
     fig_ts <- fig_nnet_1 + fig_prophet_1 + fig_ets_1 + fig_sarima_1 + fig_hyb_1+ fig_baye_1 +
          plot_layout(ncol = 2, guides = 'collect')&
          theme(legend.position = 'bottom',
                plot.margin = margin(5, 15, 5, 5))
     
     fig <- cowplot::plot_grid(fig_ts, fig_table, ncol = 1, rel_heights = c(3, 1))
     
     ggsave(filename = paste0('./outcome/appendix/figure/', disease_name[i],'.pdf'),
            fig,
            width = 14, height = 15, family = "Times New Roman",
            limitsize = FALSE, device = cairo_pdf)
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

clusterExport(cl, c('datafile_analysis', 'disease_name', 
                    'fill_color', 'evaluate_forecast', 'theme_set', 
                    'scientific_10', 'plot_outcome',
                    'split_date', 'train_length', 'test_length', 'forcast_length'), 
              envir = environment())
outcome <- parLapply(cl, 1:24, auto_select_function)
stopCluster(cl)

datafile_outcome <- do.call('rbind', outcome)
write.xlsx(datafile_outcome, './outcome/appendix/model/pre-epidemic.xlsx')
