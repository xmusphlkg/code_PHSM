#####################################
## @Description:
## @version:
## @Author: Li Kangguo
## @Date: 2023-10-21 14:52:14
## @LastEditors: Li Kangguo
## @LastEditTime: 2023-10-21 15:08:20
#####################################

# packages ----------------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(ggsci)
library(paletteer)
library(patchwork)
library(Cairo)
library(lubridate)
library(scales)
library(factoextra)
library(ggdendroplot)
library(paletteer)

remove(list = ls())

source("./script/theme_set.R")

select_disease <- c('HFMD', 'Dengue fever', 'Japanese encephalitis',
                    'Malaria', 'Pertussis', 'Scarlet fever',
                    'Mumps', 'Rubella')

# data --------------------------------------------------------------------

DataAll <- list.files(
  path = "./outcome/appendix/data/PHSMs/",
  pattern = "*.xlsx",
  full.names = TRUE
) |>
  lapply(read.xlsx, detectDates = T) |>
  bind_rows()

datafile_class <- read.xlsx('./data/disease_class.xlsx')
datafile_class$diseasename <- factor(datafile_class$diseasename, levels = datafile_class$diseasename)
datafile_class$class <- factor(datafile_class$class, levels = unique(datafile_class$class))

DataAll <- DataAll |>
  left_join(datafile_class,
    by = c("disease_1" = "diseaselist")
  ) |>
  mutate(
    RR = value / mean
  ) |> 
     filter(diseasename %in% select_disease)

# cross-correlation analysis ------------------------------------------------------

DataSI <- read.csv('./data/owid-covid-data.csv') |> 
     filter(iso_code == "CHN")

DataSIm <- DataSI |> 
     select(date, stringency_index) |> 
     mutate(year = year(date),
            month = month(date)) |> 
     group_by(year, month) |> 
     summarise(index = mean(stringency_index),
               .groups = 'drop') |> 
     mutate(date = as.Date(paste(year, month, 01, sep = '-'))) |> 
     filter(date <= max(DataAll$date))

DataAll <- DataAll |> 
     left_join(DataSIm, by = 'date')

perform_cross_correlation <- function(diseasename) {
     data <- DataAll[DataAll$diseasename == diseasename,]
     ccf_result <- ccf(data$index, data$diff, lag.max = 6, plot = F)
     lag_time <- ccf_result$lag[7:13]
     lag_cor <- ccf_result$acf[7:13]
     return(data.frame(diseasename = diseasename, correlation = lag_cor, lag_time = lag_time))
}

cross_correlation_results <- lapply(select_disease,
                                    perform_cross_correlation)
cross_correlation_results <- do.call('rbind', cross_correlation_results) |> 
     group_by(diseasename) |> 
     mutate(higher = correlation == max(abs(correlation))|correlation == -max(abs(correlation)))

diseases <- datafile_class$diseasename

layout <- '
ABCD
EFGH
'

DataRelation <- data.frame(
     level = c("No Association", "Weak", "Moderate", "Strong"),
     pl = c(0, 0.2, 0.4, 0.6),
     ph = c(0.2, 0.4, 0.6, 1)
)
DataRelation$level <- factor(DataRelation$level,
                             levels = DataRelation$level)

plot_function <- function(i, diseases = select_disease) {
     
     Data <- cross_correlation_results |> 
          filter(diseasename == diseases[i])
     
     fig <- ggplot(data = Data) +
          geom_rect(data = DataRelation,
                    mapping = aes(xmin = -Inf,
                                  xmax = Inf,
                                  ymin = pl,
                                  ymax =ph,
                                  fill = level),
                    alpha = 0.7)+
          geom_rect(data = DataRelation,
                    mapping = aes(xmin = -Inf,
                                  xmax = Inf,
                                  ymin = -pl,
                                  ymax = -ph,
                                  fill = level),
                    alpha = 0.7)+
          geom_hline(yintercept = 0,
                     show.legend = F,
                     linetype = 'longdash')+
          geom_line(mapping = aes(x = lag_time,
                                  y = correlation),
                    color = "#79AF97FF")+
          geom_point(mapping = aes(x = lag_time,
                                   y = correlation,
                                   color = higher),
                     show.legend = F) +
          coord_cartesian(ylim = c(-0.4, 0.8))+
          scale_fill_manual(values = c("#E0F7FAFF", "#80DEEAFF", "#00BCD4FF", "#006064FF"))+
          scale_color_manual(values = c('#79AF97FF', '#B24745FF'))+
          scale_y_continuous(breaks = seq(-0.4, 0.8, 0.2))+
          theme_bw() +
          labs(
               x = ifelse(i %in% 5:8, 'Lag Time (Month)', ""),
               y = ifelse(i %in% c(1, 5), "ACF", ""),
               title = paste0(LETTERS[i], ': ', diseases[i]),
               fill = "Correlation"
          )
     if (i %in% c(1, 5)) {
          fig <- fig +
               theme(
                    legend.position = c(0.9, 0.1),
                    axis.text = element_text(color = "black"),
                    panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor.y = element_blank()
               )
     } else {
          fig <- fig +
               theme(
                    legend.position = c(0.9, 0.1),
                    axis.text = element_text(color = "black"),
                    panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    axis.text.y = element_blank(),
                    axis.title.y = element_blank(),
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor.y = element_blank()
               )
     }
     return(fig)
}

outcome <- lapply(1:8, plot_function, diseases = select_disease)

plot <- do.call(wrap_plots, outcome) +
     plot_layout(design = layout, guides = 'collect')&
     theme(
          title = element_text(size = 8)
     )

ggsave("./outcome/publish/fig5.pdf",
       plot,
       family = "Times New Roman",
       limitsize = FALSE, device = cairo_pdf,
       width = 8, height = 4)

