
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
library(caret)
library(bsts)
library(patchwork)
library(Cairo)
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
ABCDE##
FGHIJKL
MNOPQRS
TVWXY##
'

datafile_analysis <- read.xlsx('./data/Nation.xlsx', 
                               sheet = "Sheet 1",
                               detectDates = T) %>% 
     filter(date >= as.Date('2008/1/1'))

datafile_class <- read.xlsx('./data/disease_class.xlsx')

datafile_class <- read.xlsx('./outcome/appendix/model/select/pre-epidemic.xlsx') |>
     left_join(datafile_class, by = c(disease = 'diseasename')) |> 
     rename(Method = 'Best') |> 
     filter(!is.na(class))
datafile_class$id <- 1:nrow(datafile_class)

split_date <- as.Date("2019/12/15")
split_date_1 <- as.Date("2022/11/15")

train_length <- 12*12
forcast_length <- 12+12+11

disease_list <- c('百日咳', '丙肝', '戊肝', '布病', '登革热', 
                  '肺结核', '风疹', '急性出血性结膜炎', '甲肝', 
                  '痢疾', '淋病', '流行性出血热', '艾滋病',
                  '流行性腮腺炎', '梅毒', '疟疾', '其它感染性腹泻病',
                  '伤寒+副伤寒', '乙肝', '手足口病', '猩红热',
                  '乙型脑炎', '包虫病', '斑疹伤寒')
disease_name <- c('Pertussis', 'HCV', 'HEV',
                  'Brucellosis', 'Dengue fever', 'Tuberculosis',
                  'Rubella', 'Acute hemorrhagic conjunctivitis', 'HAV',
                  'Dysentery', 'Gonorrhea', 'HFRS',
                  'AIDS', 'Mumps',
                  'Syphilis', 'Malaria', 'Other infectious diarrhea',
                  'Typhoid fever and paratyphoid fever', 'HBV', 'HFMD',
                  'Scarlet fever', 'Japanese encephalitis', 'Hydatidosis', 'Typhus')

scientific_10 <- function(x) {
     ifelse(x == 0, 0, parse(text = gsub("[+]", "", gsub("e", "%*%10^", scales::scientific_format()(x)))))
}

datafile_class <- data.frame(disease_list = disease_list,
                             disease_name = disease_name) |> 
     right_join(datafile_class, by = c('disease_name' = 'disease')) |> 
     mutate(disease_name = factor(disease_name,
                                  levels = c('HBV', 'HCV', 'Syphilis', 'AIDS', 'Gonorrhea',
                                             'HAV', 'HFMD', 'HEV', 'Other infectious diarrhea', 'Typhoid fever and paratyphoid fever', 'Acute hemorrhagic conjunctivitis', 'Dysentery',
                                             'Dengue fever', 'Brucellosis', 'Malaria', 'Japanese encephalitis', 'HFRS', 'Hydatidosis', 'Typhus',
                                             'Rubella', 'Mumps', 'Pertussis', 'Tuberculosis', 'Scarlet fever'))) |> 
     arrange(class, disease_name)

# data clean --------------------------------------------------------------

i <- 20

auto_analysis_function <- function(i){
     set.seed(202305)
     datafile_single <- datafile_analysis %>% 
          filter(disease_1 == datafile_class$disease_list[i]) %>% 
          select(date, disease_1, value) %>% 
          complete(
               date = seq.Date(
                    from = min(date),
                    to = max(date),
                    by = 'month'
               ),
               fill = list(value = 0,
                           disease_1 = datafile_class$disease_list[i])
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
     max_case <- max(ts_obse_1)
     
     # Select Method ------------------------------------------------------------
     
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
     
     if (datafile_class$Method[i] == 'ARIMA'){
          mod <- auto.arima(ts_train_1, seasonal = F)
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
     
     max_value <- max(outcome_plot_2[,-1], max_case, na.rm = T)
     min_value <- min(outcome_plot_2[,-1], na.rm = T)
     diff_value_1 <- round(sum(outcome_data$mean, na.rm = T) - sum(outcome_data$value, na.rm = T))
     diff_color_1 <- ifelse(diff_value_1 > 0, "#00A08750", "#DC000050")
     
     outcome_plot_2 <- outcome_plot_2 |> 
          mutate_at(vars(contains('er')), as.numeric)
     outcome_data <- full_join(outcome_plot_2, outcome_plot_1)
     
     write.xlsx(outcome_data,
                paste0('./outcome/appendix/data/PHSMs/', datafile_class$disease_name[i], '.xlsx'))
     
     outcome_plot_3 <- datafile_single |> 
          filter(date < split_date_1)
     
     fig1 <- ggplot()+
          geom_line(mapping = aes(x = date,
                                  y = value,
                                  colour = 'Observed'), 
                    linewidth = 0.7, data = outcome_plot_3)+
          geom_line(mapping = aes(x = date, 
                                  y = mean,
                                  colour = 'Forecasted'),
                    linewidth = 0.7, data = outcome_plot_2)+
          geom_ribbon(mapping = aes(x = date, 
                                    ymin = mean, 
                                    ymax = value),
                      data = outcome_data, 
                      alpha = 0.3, 
                      fill = diff_color_1, 
                      show.legend = F)+
          geom_vline(xintercept = as.Date('2019/11/15'), show.legend = F,
                     linetype = 'longdash')+
          coord_cartesian(ylim = c(0, NA))+
          scale_x_date(expand = expansion(add = c(0, 31)),
                       date_labels = '%Y',
                       breaks = seq(min(outcome_plot_3$date), max(outcome_plot_2$date)+31, by="2 years"))+
          scale_y_continuous(expand = c(0, 0),
                             label = scientific_10,
                             breaks = pretty(c(min_value, max_value, 0)),
                             limits = range(pretty(c(min_value, max_value, 0))))+
          scale_color_manual(values = c(Forecasted = "#E64B35FF", Observed = '#00A087FF'))+
          theme_set()+
          theme(legend.position = 'bottom')+
          labs(x = NULL,
               y = ifelse(i %in% c(1, 6, 13, 20),'Cases', ''),
               color = '',
               title = paste0(LETTERS[i], ': ', datafile_class$disease_name[i]))
     
     return(fig1)
}

# run model ---------------------------------------------------------------

i <- 20
# lapply(1:26, auto_select_function)
auto_analysis_function(i)

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
     library(caret)
     library(bsts)
     library(patchwork)
     library(Cairo)
     library(ggpubr)
})

clusterExport(cl, c('datafile_analysis', 'datafile_class',
                    'forcast_length', 'split_date', 'train_length', 'split_date_2',
                    'fill_color', 'func_rmse', 'theme_set', 'scientific_10'), 
              envir = environment())
outcome <- parLapply(cl, 1:24, auto_analysis_function)

stopCluster(cl)

plot <- do.call(wrap_plots, outcome)

ggsave('./outcome/publish/fig3.pdf',
       plot + plot_layout(design = layout, guides = 'collect')&
            theme(legend.position = 'bottom'),
       family = "Times New Roman",
       limitsize = FALSE, device = cairo_pdf,
       width = 25, height = 14)

