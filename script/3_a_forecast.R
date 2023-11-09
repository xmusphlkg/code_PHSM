
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

Sys.setlocale(locale = 'en')
set.seed(202208)

remove(list = ls())

# data load ---------------------------------------------------------------

source('./script/theme_set.R')
source('./script/ggplot.R')

layout <- '
ABCDEFG
HIJKLMN
OPQRSZZ
TVWXYZZ
'

datafile_analysis <- read.xlsx('./data/Nation.xlsx', 
                               sheet = "Sheet 1",
                               detectDates = T) %>% 
     filter(date >= as.Date('2008/1/1'))

datafile_class <- read.xlsx('./data/disease_class.xlsx')

datafile_class <- read.xlsx('./outcome/appendix/model/select/pre-epidemic.xlsx') |>
     left_join(datafile_class, by = c(disease = 'diseasename')) |> 
     rename(Method = 'Best') |> 
     filter(!is.na(class)) |> 
     mutate(disease = factor(disease, levels = datafile_class$diseasename)) |> 
     arrange(disease)
datafile_class$id <- 1:nrow(datafile_class)

## adjust best model
datafile_class$Method[datafile_class$disease == 'AHC'] <- "Hybrid"

split_date <- as.Date("2019/12/15")
split_date_1 <- as.Date("2022/11/15")

train_length <- 12*12
forcast_length <- 12+12+11

scientific_10 <- function(x) {
     ifelse(x == 0, 0, parse(text = gsub("[+]", "", gsub("e", "%*%10^", scales::scientific_format()(x)))))
}

# data clean --------------------------------------------------------------

auto_analysis_function <- function(i){
     set.seed(202305)
     datafile_single <- datafile_analysis %>% 
          filter(disease_1 == datafile_class$diseaselist[i]) %>% 
          select(date, disease_1, value) %>% 
          complete(
               date = seq.Date(
                    from = min(date),
                    to = max(date),
                    by = 'month'
               ),
               fill = list(value = 0,
                           disease_1 = datafile_class$diseaselist[i])
          )
     
     ## simulate date before 2020
     df_simu <- datafile_single  %>% 
          arrange(date) %>% 
          unique() %>% 
          filter(date <= split_date_1)%>% 
          select(value)
     
     ts_obse_1 <- df_simu %>% 
          ts(frequency = 12,
             start = c(as.numeric(format(min(datafile_single$date), "%Y")),
                       as.numeric(format(min(datafile_single$date), "%m"))))
     
     ts_train_1 <- head(ts_obse_1, train_length)
     
     ## plot data before April 2019
     outcome_plot_1 <- datafile_single |> 
          filter(date > split_date & date < split_date_1) |> 
          as.data.frame()
     max_case <- max(tail(ts_obse_1, 59))
     
     # Select Method ------------------------------------------------------------
     
     print(datafile_class$disease[i])
     print(datafile_class$Method[i])
     if (datafile_class$Method[i] == 'SARIMA'){
          mod <- auto.arima(ts_train_1, seasonal = T)
          outcome <- forecast(mod, h = forcast_length)
          
          outcome_plot_2 <- data.frame(
               date = zoo::as.Date(time(outcome$mean)),
               mean = as.matrix(outcome$mean),
               lower_80 = as.matrix(outcome$lower[,1]),
               lower_95 = as.matrix(outcome$lower[,2]),
               upper_80 = as.matrix(outcome$upper[,1]),
               upper_95 = as.matrix(outcome$upper[,2])
          )
     }
     
     if (datafile_class$Method[i] == 'Prophet'){
          mod <- prophet(data.frame(ds = zoo::as.Date(time(ts_train_1)), y = as.numeric(ts_train_1)),
                         interval.width = 0.95)
          future <- make_future_dataframe(mod, periods = forcast_length, freq = "month")
          outcome <- predict(mod, future)
          
          outcome_plot_2 <- data.frame(
               date = as.Date(outcome$ds),
               mean = as.numeric(outcome$yhat),
               lower_80 = as.numeric(outcome$yhat_lower),
               lower_95 = as.numeric(outcome$yhat_lower),
               upper_80 = as.numeric(outcome$yhat_upper),
               upper_95 = as.numeric(outcome$yhat_upper)
          ) |> 
               tail(forcast_length)
     }
     
     if (datafile_class$Method[i] == 'ETS'){
          outcome <- forecast(ets(ts_train_1), h=forcast_length)
          
          outcome_plot_2 <- data.frame(
               date = zoo::as.Date(time(outcome$mean)),
               mean = as.matrix(outcome$mean),
               lower_80 = as.matrix(outcome$lower[,1]),
               lower_95 = as.matrix(outcome$lower[,2]),
               upper_80 = as.matrix(outcome$upper[,1]),
               upper_95 = as.matrix(outcome$upper[,2])
          )
     }
     
     if (datafile_class$Method[i] == 'Neural Network'){
          mod <- nnetar(ts_train_1)
          
          outcome_2 <- forecast(mod, h = forcast_length)
          
          outcome_plot_2 <- data.frame(
               date = zoo::as.Date(time(outcome_2$mean)),
               mean = as.matrix(outcome_2$mean),
               lower_80 = NA,
               lower_95 = NA,
               upper_80 = NA,
               upper_95 = NA
          )
     }
     
     if (datafile_class$Method[i] == 'Hybrid'){
          mod <- hybridModel(ts_train_1, 
                              models = c('aesn'),
                              a.args = list(seasonal = T),
                              weights="equal", parallel=TRUE, num.cores = 10)
          outcome <- forecast(mod, h = forcast_length)
          
          outcome_plot_2 <- data.frame(
               date = zoo::as.Date(time(outcome$mean)),
               mean = as.matrix(outcome$mean),
               lower_80 = as.matrix(outcome$lower[,1]),
               lower_95 = as.matrix(outcome$lower[,2]),
               upper_80 = as.matrix(outcome$upper[,1]),
               upper_95 = as.matrix(outcome$upper[,2])
          )
     }
     
     
     if (datafile_class$Method[i] == 'Bayesian Structural'){
          ss <- AddLocalLinearTrend(list(), ts_train_1)
          ss <- AddSeasonal(ss, ts_train_1, nseasons = 12)
          mod <- bsts(ts_train_1, state.specification = ss, niter = 500, seed = 20231007)
          
          burn <- SuggestBurn(0.1, mod)
          outcome <- predict.bsts(mod, horizon = forcast_length, burn = burn, quantiles = c(0.025, 0.1, 0.9, 0.975))
          
          outcome_plot_2 <- data.frame(
               date = zoo::as.Date(time(tail(ts_obse_1, forcast_length))),
               mean = outcome$mean,
               lower_80 = outcome$interval[2,],
               lower_95 = outcome$interval[1,],
               upper_80 = outcome$interval[3,],
               upper_95 = outcome$interval[4,]
          )
     }
     
     max_value <- max(outcome_plot_2[,2], max_case, na.rm = T)
     min_value <- min(outcome_plot_2[,2], na.rm = T)
     
     outcome_plot_2 <- outcome_plot_2 |> 
          mutate_at(vars(contains('er')), as.numeric)
     outcome_data <- full_join(outcome_plot_2, outcome_plot_1) |> 
          mutate(diff = mean - value,
                 color = if_else(diff > 0, 'Decrease', 'Increase'))
     
     write.xlsx(outcome_data,
                paste0('./outcome/appendix/data/PHSMs/', datafile_class$disease[i], '.xlsx'))
     
     outcome_plot_3 <- datafile_single |> 
          filter(date < split_date_1)
     
     fig1 <- ggplot()+
          geom_line(mapping = aes(x = date,
                                  y = value,
                                  colour = 'Observed'), 
                    linewidth = 0.7, data = outcome_plot_3)+
          annotate('text', 
                   x = median(c(as.Date('2020/1/1'), as.Date('2018/1/1'))),
                   y = Inf, 
                   label = 'Pre-epidemic\nPeriods', 
                   vjust = 1)+
          geom_line(mapping = aes(x = date, 
                                  y = mean,
                                  colour = 'Forecasted'),
                    linewidth = 0.7, data = outcome_plot_2)+
          stat_difference(mapping = aes(x = date, 
                                        ymin = mean, 
                                        ymax = value),
                          data = outcome_data, 
                          alpha = 0.3,
                          levels = c('Decreased', 'Increased'))+
          geom_vline(xintercept = c(as.Date('2020/1/1'), max(outcome_plot_2$date)),
                     show.legend = F,
                     linetype = 'longdash')+
          annotate('text', 
                   x = median(c(as.Date('2020/1/1'), max(outcome_plot_2$date))),
                   y = Inf, 
                   label = 'PHSMs\nPeriods', 
                   vjust = 1)+
          coord_cartesian(ylim = c(0, NA),
                          xlim = c(as.Date('2018-01-01'), NA))+
          scale_x_date(expand = expansion(add = c(0, 62)),
                       date_labels = '%Y',
                       breaks = seq(min(outcome_plot_3$date), max(outcome_plot_2$date)+62, by="1 years"))+
          scale_y_continuous(expand = c(0, 0),
                             label = scientific_10,
                             breaks = pretty(c(min_value, max_value, 0)),
                             limits = range(pretty(c(min_value, max_value, 0))))+
          scale_color_manual(values = c(Forecasted = "#E64B35FF", Observed = '#00A087FF'))+
          scale_fill_manual(values = c(Decreased = "#E64B3550", Increased = '#00A08750'))+
          theme_set()+
          theme(legend.position = 'bottom')+
          labs(x = NULL,
               y = ifelse(i %in% c(1, 6, 13, 20),'Cases', ''),
               color = '',
               title = paste0(LETTERS[i], ': ', datafile_class$disease[i]))
     
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
     
     Sys.setlocale(locale = 'en')
     set.seed(202208)
})

clusterExport(cl, ls()[ls() != "cl"], 
              envir = environment())
outcome <- parLapply(cl, 1:24, auto_analysis_function)
stopCluster(cl)
outcome[[25]] <- guide_area()

plot <- do.call(wrap_plots, outcome) +
     plot_layout(design = layout, guides = 'collect') &
     theme(panel.background = element_rect(fill='transparent'),
           plot.background = element_rect(fill='transparent', color=NA))

ggsave('./outcome/publish/fig3.pdf',
       plot,
       family = "Times New Roman",
       limitsize = FALSE, device = cairo_pdf,
       width = 25, height = 14)

ggsave('./outcome/publish/fig3.png',
       plot,
       limitsize = FALSE,
       width = 25, height = 14)

