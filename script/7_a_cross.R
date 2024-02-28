
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

select_disease <- c('Dengue fever', 'Rubella',
                    'Scarlet fever', 'Pertussis',
                    'HFMD',  'Mumps',
                    'Malaria', 'JE')

# incidence cluster -------------------------------------------------------

set.seed(20240218)

datafile_class <- read.xlsx("./outcome/appendix/Figure Data/Fig.1 data.xlsx", sheet = "panel A") |>
     select(-c(value, label))

datafile_analysis <- read.xlsx("./data/nation_and_provinces.xlsx", detectDates = T, sheet = "Nation") |>
     filter(date >= as.Date("2008-1-1") & date < split_dates[1])

DataMatInci <- datafile_analysis |>
     filter(disease_en %in% datafile_class$disease) |>
     select(date, disease_en, value) |>
     rename(c(disease = "disease_en")) |>
     mutate(
          disease = factor(
               disease,
               levels = datafile_class$disease,
               labels = datafile_class$disease
          ),
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
          value = as.integer(value)
     ) |>
     select(value, date, disease) |>
     pivot_wider(
          names_from = date,
          values_from = value
     )

diseasename <- DataMatInci$disease
DataMatInci <- DataMatInci |>
     select(-disease) |>
     as.matrix()
rownames(DataMatInci) <- diseasename
DataMatInci <- scale(DataMatInci)

hcdata <- hkmeans(DataMatInci, 2)
fig1 <- fviz_dend(hcdata,
                  cex = 0.6,
                  k_colors = fill_color_disease[9:8],
                  rect = TRUE,
                  rect_fill = TRUE,
                  horiz = TRUE,
                  # type = "circular",
                  main = LETTERS[1]
) +
     theme(
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(size = 12, color = "black"),
          plot.title.position = "plot",
          plot.caption.position = "plot",
          plot.title = element_text(face = "bold", size = 14, hjust = 0)
     ) +
     scale_y_continuous(trans = scales::pseudo_log_trans(base = 10))

data_fig <- list()

data_fig[[paste("panel", LETTERS[1])]] <- data.frame(
     disease = names(hcdata$cluster),
     cluster = as.integer(hcdata$cluster)
) |>
     left_join(
          data.frame(
               disease = rownames(hcdata[["data"]]),
               as.data.frame(hcdata[["data"]])
          ),
          by = 'disease'
     )

# IRR cluster -------------------------------------------------------------

DataAll <- list.files(path = "./outcome/appendix/forecast/",
                      pattern = "*.xlsx",
                      full.names = TRUE) |>
     lapply(read.xlsx, detectDates = T) |>
     bind_rows() |>
     left_join(datafile_class,
               by = c("disease_en" = "disease")) |>
     mutate(IRR = (value + add_value) / (mean + add_value),
            diff = mean - value) |> 
     filter(date >= split_dates[1])

# cluster for IRR
DataMatRR <- DataAll |>
     select(IRR, date, disease_en) |>
     filter(date < split_dates[4]) |> 
     pivot_wider(
          names_from = date,
          values_from = IRR
     )

diseasename <- DataMatRR$disease_en
DataMatRR <- DataMatRR |>
     select(-disease_en) |>
     as.matrix()
rownames(DataMatRR) <- diseasename
DataMatRR <- log(DataMatRR)
DataMatRR <- scale(DataMatRR)

## PHSMs period I, II, epidemic period
hcdata <- hkmeans(DataMatRR, 4)
fig2 <- fviz_cluster(hcdata,
                     data = DataMatRR,
                     main = LETTERS[2],
                     ggtheme = theme_set(),
                     repel = TRUE,
                     k_colors = fill_color_disease[5:3],
                     palette = "npg"
) +
     theme(legend.position = "none")

data_fig[[paste("panel", LETTERS[2])]] <- data.frame(
     disease = names(hcdata$cluster),
     cluster = as.integer(hcdata$cluster)
) |>
     left_join(
          data.frame(
               disease = rownames(hcdata[["data"]]),
               as.data.frame(hcdata[["data"]])
          ),
          by = 'disease'
     )

fig3 <- fviz_cluster(hcdata,
                     data = DataMatRR[, 1:37],
                     main = LETTERS[3],
                     ggtheme = theme_set(),
                     repel = TRUE,
                     k_colors = fill_color_disease[5:3],
                     palette = "npg"
) +
     theme(
          legend.position = "none",
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = "black")
     ) +
     labs(color = "Cluster") +
     coord_cartesian(
          xlim = c(0, 5),
          ylim = c(-1, 1.5)
     )

data_fig[[paste("panel", LETTERS[3])]] <- data.frame(
     disease = names(hcdata$cluster),
     cluster = as.integer(hcdata$cluster)
) |>
     left_join(
          data.frame(
               disease = rownames(hcdata[["data"]]),
               as.data.frame(hcdata[["data"]])
          ),
          by = 'disease'
     )

fig23 <- fig2 + inset_element(fig3, left = 0.01, bottom = 0.1, right = 0.53, top = 0.9)


# cross-correlation analysis ------------------------------------------------------

DataAll <- DataAll |> 
     filter(disease_en %in% select_disease)

DataSI <- read.csv('./data/owid-covid-data.csv') |> 
     filter(iso_code == "CHN") |> 
     select(date, stringency_index) |> 
     mutate(year = year(date),
            month = month(date)) |> 
     group_by(year, month) |> 
     summarise(index = mean(stringency_index),
               .groups = 'drop') |> 
     mutate(date = as.Date(paste(year, month, 01, sep = '-'))) |> 
     filter(date <= max(DataAll$date))

DataAll <- DataAll |> 
     left_join(DataSI, by = 'date')

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
               title = paste0(LETTERS[i+3], ': ', diseases[i]),
               fill = "Correlation group"
          )
     if (i %in% c(1, 5)) {
          fig <- fig +
               theme(
                    plot.title = element_text(face = "bold", size = 14, hjust = 0),
                    legend.text = element_text(size = 12),
                    legend.title = element_text(face = "bold", size = 12),
                    axis.text = element_text(size = 10.5, color = "black"),
                    panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor.y = element_blank()
               )
     } else {
          fig <- fig +
               theme(
                    plot.title = element_text(face = "bold", size = 14, hjust = 0),
                    legend.text = element_text(size = 12),
                    legend.title = element_text(face = "bold", size = 12),
                    axis.text = element_text(size = 10.5, color = "black"),
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

fig4 <- do.call(wrap_plots, outcome) +
     plot_layout(ncol = 4, guides = 'collect')&
     theme(
          legend.position = 'right'
     )

fig123 <- cowplot::plot_grid(fig1, fig23, ncol = 2, rel_widths = c(1, 2.5))

ggsave("./outcome/publish/fig7.pdf",
       cowplot::plot_grid(fig123, fig4, ncol = 1, rel_heights = c(1, 1.2)),
       family = "Times New Roman",
       limitsize = FALSE, device = cairo_pdf,
       width = 12, height = 10)

for (i in 1:length(select_disease)) {
     data_fig[[paste('panel', LETTERS[i+3])]] <- cross_correlation_results |> 
          filter(diseasename == select_disease[i])
}

write.xlsx(data_fig,
           file = './outcome/appendix/Figure Data/Fig.7 data.xlsx')
