# packages ----------------------------------------------------------------

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
library(ggh4x)
library(ggpubr)
library(paletteer)
library(doParallel)

Sys.setlocale(locale = "en")
set.seed(202208)

remove(list = ls())

# data load ---------------------------------------------------------------

source("./script/theme_set.R")
source("./script/ggplot.R")

layout <- "
ABCDEFG
HIJKLZZ
MNOPQZZ
RSTVWXY
"

datafile_analysis <- read.xlsx("./data/nation_and_provinces.xlsx", detectDates = T, sheet = "Nation") |>
     filter(date >= as.Date("2008-1-1")) |>
     mutate(value = as.integer(value))

datafile_class <- read.xlsx("./outcome/appendix/Figure Data/Fig.1 data.xlsx", sheet = "panel A") |>
     select(-c(value, label))

datafile_class <- read.xlsx("./outcome/appendix/Figure Data/Fig.3 data.xlsx") |>
     filter(Best == 1) |>
     select(disease, Method) |>
     left_join(datafile_class, by = c(disease = "disease")) |>
     filter(!is.na(class)) |>
     mutate(disease = factor(disease, levels = datafile_class$disease)) |>
     arrange(disease)
datafile_class$id <- 1:nrow(datafile_class)

# data clean --------------------------------------------------------------

auto_analysis_function <- function(i) {
     
     set.seed(202305)
     
     datafile_single <- datafile_analysis |>
          filter(disease_en == datafile_class$disease[i]) |>
          select(date, disease_en, value)
     datafile_rect <- data.frame(start = c(min(datafile_single$date), split_dates),
                                 end = c(split_dates, max(datafile_single$date)),
                                 label = split_periods) |>
          mutate(m = as.Date((as.numeric(start) + as.numeric(end)) / 2, origin = "1970-01-01"),
                 label = factor(label, levels = split_periods))
     
     ## prepare data
     train_length <- 12 * 12
     forcast_length <- 12 * 4
     split_date <- as.Date("2018/1/1")
     
     ## Rubella outbreak from March 2019 to July 2019
     if (datafile_class$disease[i] == "Rubella") {
          train_length <- train_length - 12
          forcast_length <- forcast_length + 12
     }
     
     ## simulate
     df_simu <- datafile_single |>
          arrange(date) |>
          unique() |>
          select(value)
     
     ts_obse <- ts(
          df_simu$value,
          frequency = 12,
          start = c(as.numeric(format(
               min(datafile_single$date), "%Y"
          )),
          as.numeric(format(
               min(datafile_single$date), "%m"
          )))
     )
     
     ts_train <- head(ts_obse, train_length) + add_value
     
     outcome_plot_1 <- datafile_single |>
          filter(date >= split_dates[1]) |>
          as.data.frame()
     max_case <- max(tail(ts_obse, forcast_length + 24))
     
     # Select Method ------------------------------------------------------------
     
     print(datafile_class$disease[i])
     print(datafile_class$Method[i])
     if (datafile_class$Method[i] == "SARIMA") {
          mod <- auto.arima(ts_train, seasonal = T, ic = 'aicc', lambda = 'auto')
          outcome <- forecast(mod, h = forcast_length)
          outcome_plot_2 <- data.frame(
               date = zoo::as.Date(time(outcome$mean)),
               mean = as.matrix(outcome$mean) - add_value,
               lower_80 = as.matrix(outcome$lower[, 1]) - add_value,
               lower_95 = as.matrix(outcome$lower[, 2]) - add_value,
               upper_80 = as.matrix(outcome$upper[, 1]) - add_value,
               upper_95 = as.matrix(outcome$upper[, 2]) - add_value
          )
     }
     
     if (datafile_class$Method[i] == "Prophet") {
          mod <- prophet(data.frame(ds = zoo::as.Date(time(ts_train)),
                                    y = as.numeric(ts_train)),
               interval.width = 0.95,
               weekly.seasonality = FALSE,
               daily.seasonality = FALSE)
          future <-make_future_dataframe(mod, periods = forcast_length, freq = "month")
          outcome <- predict(mod, future)
          outcome_plot_2 <- data.frame(date = as.Date(outcome$ds),
                                       mean = as.numeric(outcome$yhat) - add_value,
                                       lower_80 = as.numeric(outcome$yhat_lower) - add_value,
                                       lower_95 = as.numeric(outcome$yhat_lower) - add_value,
                                       upper_80 = as.numeric(outcome$yhat_upper) - add_value,
                                       upper_95 = as.numeric(outcome$yhat_upper) - add_value) |>
               tail(forcast_length)
     }
     
     if (datafile_class$Method[i] == "ETS") {
          outcome <- forecast(ets(ts_train, ic = "aicc", lambda = "auto"), h = forcast_length)
          outcome_plot_2 <- data.frame(date = zoo::as.Date(time(outcome$mean)),
                                       mean = as.matrix(outcome$mean) - add_value,
                                       lower_80 = as.matrix(outcome$lower[, 1]) - add_value,
                                       lower_95 = as.matrix(outcome$lower[, 2]) - add_value,
                                       upper_80 = as.matrix(outcome$upper[, 1]) - add_value,
                                       upper_95 = as.matrix(outcome$upper[, 2]) - add_value)
     }
     
     if (datafile_class$Method[i] == "Neural Network") {
          mod <- nnetar(ts_train, lambda = "auto")
          outcome_2 <- forecast(mod, h = forcast_length)
          
          outcome_plot_2 <- data.frame(date = zoo::as.Date(time(outcome_2$mean)),
                                       mean = as.matrix(outcome_2$mean) - add_value,
                                       lower_80 = NA,
                                       lower_95 = NA,
                                       upper_80 = NA,
                                       upper_95 = NA)
     }
     
     if (datafile_class$Method[i] == "Hybrid*") {
          mod <- hybridModel(ts_train,
                             lambda = 'auto',
                             models = c("aesn"),
                             a.args = list(seasonal = T),
                             weights = "equal", parallel = TRUE, num.cores = 10,
                             errorMethod = 'MAE')
          outcome <- forecast(mod, h = forcast_length)
          
          outcome_plot_2 <- data.frame(date = zoo::as.Date(time(outcome$mean)),
                                       mean = as.matrix(outcome$mean) - add_value,
                                       lower_80 = as.matrix(outcome$lower[, 1]) - add_value,
                                       lower_95 = as.matrix(outcome$lower[, 2]) - add_value,
                                       upper_80 = as.matrix(outcome$upper[, 1]) - add_value,
                                       upper_95 = as.matrix(outcome$upper[, 2]) - add_value)
     }
     
     
     if (datafile_class$Method[i] == "Bayesian Structural") {
          ss <- AddLocalLinearTrend(list(), ts_train)
          ss <- AddSeasonal(ss, ts_train, nseasons = 12)
          mod <- bsts(ts_train, state.specification = ss, niter = 500, seed = 20231007)
          
          burn <- SuggestBurn(0.1, mod)
          outcome <- predict.bsts(mod, horizon = forcast_length, burn = burn, quantiles = c(0.025, 0.1, 0.9, 0.975))
          
          outcome_plot_2 <- data.frame(date = zoo::as.Date(time(tail(ts_obse, forcast_length))),
                                       mean = outcome$mean - add_value,
                                       lower_80 = outcome$interval[2, ] - add_value,
                                       lower_95 = outcome$interval[1, ] - add_value,
                                       upper_80 = outcome$interval[3, ] - add_value,
                                       upper_95 = outcome$interval[4, ] - add_value)
     }
     
     # correct all negative value into zero
     outcome_plot_2[outcome_plot_2 < 0] <- 0
     max_value <- max(outcome_plot_2[, 2], max_case, na.rm = T)
     min_value <- min(outcome_plot_2[, 2], na.rm = T)
     
     outcome_plot_2 <- outcome_plot_2 |>
          mutate_at(vars(contains("er")), as.numeric)
     outcome_data <- full_join(outcome_plot_2, outcome_plot_1) |>
          mutate(diff = mean - value,
                 color = if_else(diff > 0, "Decrease", "Increase"))
     
     write.xlsx(outcome_data,
                paste0("./outcome/appendix/forecast/", datafile_class$disease[i], ".xlsx"))
     
     outcome_plot_3 <- datafile_single |>
          filter(date >= split_date)
     plot_breaks <- pretty(c(min_value, max_value, 0))
     
     fig1 <- ggplot() +
          geom_vline(
               xintercept = datafile_rect$end,
               show.legend = F,
               color = "grey",
               linetype = "longdash"
          ) +
          geom_rect(
               data = datafile_rect,
               aes(
                    xmin = start,
                    xmax = end,
                    fill = label
               ),
               ymax = 0,
               ymin = max(plot_breaks) / 10,
               alpha = 0.2,
               show.legend = F
          ) +
          geom_line(
               mapping = aes(x = date,
                             y = value,
                             colour = "Observed"),
               linewidth = 0.7,
               data = outcome_plot_3
          ) +
          geom_line(
               mapping = aes(x = date,
                             y = mean,
                             colour = "Forecasted"),
               linewidth = 0.7,
               data = outcome_plot_2
          ) +
          stat_difference(
               mapping = aes(
                    x = date,
                    ymin = value,
                    ymax = mean
               ),
               data = outcome_data,
               alpha = 0.3,
               levels = c("Decreased", "Increased"),
               show.legend = F,
          ) +
          coord_cartesian(ylim = c(0, NA),
                          xlim = c(split_date, NA)) +
          scale_x_date(
               expand = expansion(add = c(0, 0)),
               date_labels = "%Y",
               breaks = seq(min(outcome_plot_3$date), max(outcome_plot_2$date), by = "1 years")
          ) +
          scale_y_continuous(
               expand = c(0, 0),
               label = scientific_10,
               breaks = plot_breaks,
               limits = range(plot_breaks)
          ) +
          scale_color_manual(values = c(
               Forecasted = "#E64B35FF",
               Observed = "#00A087FF"
          )) +
          scale_fill_manual(values = c(
               Decreased = "#00A08750",
               Increased = "#E64B3550",
               back_color
          )) +
          theme_set() +
          theme(legend.position = "bottom") +
          labs(
               x = NULL,
               y = ifelse(i %in% c(1, 8, 13, 18), "Monthly incidence", ""),
               color = "",
               title = paste0(LETTERS[i], ": ", datafile_class$disease[i])
          )
     
     return(fig1)
}

# run model ---------------------------------------------------------------


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
     library(ggh4x)
     library(ggpubr)
     library(paletteer)
     
     Sys.setlocale(locale = "en")
     set.seed(202208)
})

clusterExport(cl, ls()[ls() != "cl"], envir = environment())
outcome <- parLapply(cl, 1:24, auto_analysis_function)
stopCluster(cl)
outcome[[25]] <- guide_area()

plot <- do.call(wrap_plots, outcome) +
     plot_layout(design = layout, guides = "collect") &
     theme(
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA)
     )

ggsave(
     "./outcome/publish/fig4.pdf",
     plot,
     family = "Times New Roman",
     limitsize = FALSE,
     device = cairo_pdf,
     width = 25,
     height = 14
)

# merge data file ---------------------------------------------------------

file_list <- paste0("./outcome/appendix/forecast/",
                    datafile_class$disease,
                    ".xlsx")
data_list <- lapply(file_list, read.xlsx, detectDates = T)
names(data_list) <-
     paste0(LETTERS[1:24], " ", datafile_class$disease)
write.xlsx(data_list,
           file = "./outcome/appendix/Figure Data/Fig.4 data.xlsx")
