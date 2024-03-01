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
fig <- list()

for (a in 1:2) {
     
     i <- 16
     
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
     
     ## including 2019 outbreak or not
     if (a == 2) {
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
     
     mod <- auto.arima(ts_train, seasonal = T, ic = 'aicc', lambda = 'auto')
     outcome <- forecast(mod, h = forcast_length)
     outcome_plot_2 <- data.frame(
          date = zoo::as.Date(time(outcome$mean)),
          mean = as.matrix(outcome$mean) - add_value,
          lower_80 = as.matrix(outcome$lower[, 1]) - add_value,
          lower_95 = as.matrix(outcome$lower[, 2]) - add_value,
          upper_80 = as.matrix(outcome$upper[, 1]) - add_value,
          upper_95 = as.matrix(outcome$upper[, 2]) - add_value,
          model = 'SARIMA'
     )
     
     outcome <- forecast(ets(ts_train, ic = "aicc", lambda = "auto"), h = forcast_length)
     outcome_plot_2 <- data.frame(
          date = zoo::as.Date(time(outcome$mean)),
          mean = as.matrix(outcome$mean) - add_value,
          lower_80 = as.matrix(outcome$lower[, 1]) - add_value,
          lower_95 = as.matrix(outcome$lower[, 2]) - add_value,
          upper_80 = as.matrix(outcome$upper[, 1]) - add_value,
          upper_95 = as.matrix(outcome$upper[, 2]) - add_value,
          model = 'ETS'
     ) |> 
          rbind(outcome_plot_2)
     
     mod <- hybridModel(ts_train,
                        lambda = 'auto',
                        models = c("aesn"),
                        a.args = list(seasonal = T),
                        weights = "equal", parallel = TRUE, num.cores = 10,
                        errorMethod = 'MAE')
     outcome <- forecast(mod, h = forcast_length)
     outcome_plot_2 <- data.frame(
          date = zoo::as.Date(time(outcome$mean)),
          mean = as.matrix(outcome$mean) - add_value,
          lower_80 = as.matrix(outcome$lower[, 1]) - add_value,
          lower_95 = as.matrix(outcome$lower[, 2]) - add_value,
          upper_80 = as.matrix(outcome$upper[, 1]) - add_value,
          upper_95 = as.matrix(outcome$upper[, 2]) - add_value,
          model = 'Hybrid'
     ) |> 
          rbind(outcome_plot_2)
     
     # correct all negative value into zero
     outcome_plot_2[outcome_plot_2 < 0] <- 0
     max_value <- max(outcome_plot_2[, 2], max_case, na.rm = T)
     min_value <- min(outcome_plot_2[, 2], na.rm = T)
     
     outcome_plot_2 <- outcome_plot_2 |>
          mutate_at(vars(contains("er")), as.numeric)
     outcome_data <- full_join(outcome_plot_2, outcome_plot_1) |>
          mutate(diff = mean - value,
                 color = if_else(diff > 0, "Decrease", "Increase"))
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
                             colour = paste0("Forecasted (", model, ")")),
               linewidth = 0.7,
               data = outcome_plot_2
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
          scale_color_manual(values = fill_color_disease) +
          scale_fill_manual(values = c(
               Decreased = "#00A08750",
               Increased = "#E64B3550",
               back_color
          )) +
          theme_set() +
          theme(legend.position = "bottom") +
          labs(
               x = NULL,
               y = "Monthly incidence",
               color = "",
               title = LETTERS[a]
          )
     
     fig[[a]] <- fig1
}

# visualization -----------------------------------------------------------

plot_1 <- do.call(wrap_plots, fig) +
     plot_layout(guides = "collect") &
     theme(legend.position = 'bottom')

outcome_plot_3 <- outcome_plot_2 |> 
     select(date, mean, model) |> 
     left_join(outcome_plot_1,
               by = 'date') |> 
     filter(!is.na(value)) |> 
     mutate(IRR = (value + add_value) / (mean + add_value),
            diff = mean - value,
            phase = case_when(
                 date < split_dates[1] ~ split_periods[1],
                 date >= split_dates[1] &
                      date < split_dates[2] ~ split_periods[2],
                 date >= split_dates[2] &
                      date < split_dates[3] ~ split_periods[3],
                 date >= split_dates[3] &
                      date < split_dates[4] ~ split_periods[4],
                 date >= split_dates[4] ~ split_periods[5]
            ),
            phase = factor(phase, levels = split_periods),
            Date = format(ymd(date), "%Y.%m"))

plot_2 <- ggplot(data = outcome_plot_3) +
     geom_col(mapping = aes(
          x = date,
          y = diff,
          fill = model
     ),
     position = 'dodge') +
     geom_hline(yintercept = 0) +
     scale_x_date(
          expand = expansion(add = c(15, 15)),
          date_breaks = "1 year",
          date_labels = "%Y"
     ) +
     scale_y_continuous(
          expand = expansion(mult = c(0.15, 0.15)),
          labels = scientific_10
     ) +
     scale_fill_manual(values = fill_color_disease) +
     theme_set() +
     theme(
          legend.position = 'bottom',
     ) +
     labs(
          x = "Date",
          y = "Difference",
          fill = NULL,
          title = 'C'
     ) +
     guides(fill = guide_legend(
          ncol = 3,
          byrow = T,
          title = NULL
     ))

outcome_plot_4 <- outcome_plot_3 |> 
     mutate(Periods = if_else(phase == 'Post-epidemic period', 'Post-epidemic period', 'PHSMs and epidemic periods')) |> 
     group_by(model, Periods) |> 
     summarise(q2 = quantile(IRR, 0.5, na.rm = T),
               q1 = quantile(IRR, 0.25, na.rm = T),
               q3 = quantile(IRR, 0.75, na.rm = T),
               .groups = 'drop')

fig1 <- ggplot(data = outcome_plot_4, 
                 mapping = aes(y = model, x = q2, xmin = q1, xmax = q3)) +
     geom_vline(xintercept = 1,
                show.legend = F,
                linetype = "longdash") +
     geom_linerange(aes(color = Periods,
                        group = interaction(model, Periods)),
                    position=position_dodge(width=c(0.6))) +
     geom_point(aes(color = Periods,
                    group = interaction(model, Periods)),
                position=position_dodge(width=c(0.6)),) +
     scale_x_continuous(limits = c(0, 1.1), breaks = seq(0, 1.1, 0.2)) +
     scale_color_manual(values = fill_color_disease[3:4]) +
     theme_bw() +
     theme(axis.text = element_text(size = 10.5, color = "black"),
           plot.title.position = "plot",
           plot.caption.position = "plot",
           plot.title = element_text(face = "bold", size = 14, hjust = 0),
           legend.position = 'bottom')+
     guides(color = guide_legend(
          ncol = 1,
          byrow = T,
          title = NULL
     )) +
     labs(
          x = NULL,
          y = NULL,
          title = 'D',
          color = NULL
     )

fig2 <- ggplot(
     data = outcome_plot_3,
     mapping = aes(
          fill = IRR,
          x = Date,
          y = model
     )) +
     geom_tile() +
     geom_vline(xintercept = c(3.5, 34.5, 37.5)) +
     scale_fill_gradientn(
          colors = paletteer_d("awtools::a_palette"),
          trans = log_fill,
          limits = c(0, 4)
     ) +
     scale_x_discrete(
          breaks = paste(c(2020, 2021, 2022, 2023), "01", sep = "."),
          labels = 2020:2023,
          expand = expansion(add = c(0, 0))
     ) +
     scale_y_discrete(
          expand = c(0, 0)
     ) +
     theme_bw() +
     theme(
          axis.text.y = element_blank(),
          legend.position = "bottom",
          plot.title.position = "plot",
          plot.caption.position = "plot",
          plot.title = element_text(face = "bold", size = 14, hjust = 0)
     ) +
     guides(fill = guide_colourbar(barwidth = 15, barheight = 0.5, color = "black")) +
     labs(
          x = NULL,
          y = NULL,
          fill = "Adjusted IRR",
          title = 'E'
     )

plot_3 <- fig1 + fig2 + plot_layout(widths = c(1.5, 3))

plot <- cowplot::plot_grid(plot_1, plot_2, plot_3, ncol = 1, rel_heights = c(1, 1, 0.7))

ggsave(
     "./outcome/appendix/Supplementary Appendix 1_3.png",
     plot,
     limitsize = FALSE,
     width = 10,
     height = 10
)
