
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

select_disease <- c('Dengue fever',
                    'Rubella', 'Scarlet fever',
                    'Pertussis', 'HFMD',  'Mumps',
                    'Malaria')

# data --------------------------------------------------------------------

DataAll <- list.files(
  path = "./outcome/appendix/forecast/",
  pattern = "*.xlsx",
  full.names = TRUE
) |>
  lapply(read.xlsx, detectDates = T) |>
  bind_rows()

datafile_class <- read.xlsx("./outcome/appendix/Figure Data/Fig.1 data.xlsx",
                            sheet = "panel A"
) |>
  select(-c(value, label))

DataAll <- DataAll |>
  left_join(datafile_class,
    by = c("disease_en" = "disease")
  ) |>
  mutate(
    IRR = (value + 1) / (mean + 1),
    diff = mean - value
  ) |> 
     filter(disease_en %in% select_disease)

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
     data <- DataAll[DataAll$disease_en == diseasename,]
     ccf_result <- ccf(data$index, data$diff, lag.max = 6, plot = F, na.action = na.pass)
     lag_time <- ccf_result$lag[7:13]
     lag_cor <- ccf_result$acf[7:13]
     return(data.frame(diseasename = diseasename, correlation = lag_cor, lag_time = lag_time))
}

cross_correlation_results <- lapply(select_disease,
                                    perform_cross_correlation)
cross_correlation_results <- do.call('rbind', cross_correlation_results) |> 
     group_by(diseasename) |> 
     mutate(higher = correlation == max(abs(correlation))|correlation == -max(abs(correlation)))

diseases <- datafile_class$disease

DataRelation <- data.frame(
     level = c("No Association", "Weak", "Moderate", "Strong"),
     pl = c(0, 0.2, 0.4, 0.6),
     ph = c(0.2, 0.4, 0.6, 1)
)
DataRelation$level <- factor(DataRelation$level,
                             levels = rev(DataRelation$level))

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
          scale_fill_manual(values = rev(c("#E0F7FAFF", "#80DEEAFF", "#00BCD4FF", "#006064FF")))+
          scale_color_manual(values = c('#79AF97FF', '#B24745FF'))+
          scale_y_continuous(breaks = seq(-0.4, 0.8, 0.2))+
          theme_bw() +
          labs(
               x = 'Lag Time (Month)',
               y = ifelse(i %in% c(1, 5), "Correlation Coefficient", ""),
               title = paste0(LETTERS[i], ': ', diseases[i]),
               fill = "Correlation"
          )
     if (i %in% c(1, 5)) {
          fig <- fig +
               theme(
                    axis.text = element_text(color = "black"),
                    panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor.y = element_blank()
               )
     } else {
          fig <- fig +
               theme(
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

outcome <- lapply(1:length(select_disease), plot_function, diseases = select_disease)
outcome[[length(outcome)+1]] <- guide_area()

plot <- do.call(wrap_plots, outcome) +
     plot_layout(ncol = 4, guides = 'collect')&
     theme(
          title = element_text(size = 8),
          legend.position = 'right'
     )

ggsave("./outcome/publish/fig7.pdf",
       plot,
       family = "Times New Roman",
       limitsize = FALSE, device = cairo_pdf,
       width = 8, height = 5)

write.xlsx(data_fig,
           file = './outcome/appendix/Figure Data/Fig.7 data.xlsx')
