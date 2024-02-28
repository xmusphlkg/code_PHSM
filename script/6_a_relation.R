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
library(forecast)

remove(list = ls())

source("./script/theme_set.R")

# data --------------------------------------------------------------------

datafile_class <- read.xlsx("./outcome/appendix/Figure Data/Fig.1 data.xlsx", sheet = "panel A") |>
  select(-c(value, label))

DataAll <- list.files(
  path = "./outcome/appendix/forecast/",
  pattern = "*.xlsx",
  full.names = TRUE
) |>
  lapply(read.xlsx, detectDates = T) |>
  bind_rows() |>
  filter(date >= split_dates[1])

datafile_class$disease <- factor(datafile_class$disease,
  levels = datafile_class$disease
)
datafile_class$class <- factor(datafile_class$class,
  levels = unique(datafile_class$class)
)

DataAll <- DataAll |>
     left_join(datafile_class, by = c("disease_en" = "disease")) |>
     mutate(IRR = (value + add_value) / (mean + add_value),
            Periods = case_when(
                 date < split_dates[1] ~ split_periods[1],
                 date >= split_dates[1] & date < split_dates[2] ~ split_periods[2],
                 date >= split_dates[2] & date < split_dates[3] ~ split_periods[3],
                 date >= split_dates[3] & date < split_dates[4] ~ split_periods[4],
                 date >= split_dates[4] ~ split_periods[5]))

# summary -----------------------------------------------------------------

Data <- DataAll |>
  group_by(disease_en, class, Periods) |>
  summarise(
    diff = round(sum(diff), 2),
    percnet = round(sum(diff) / sum(mean), 4),
    IRR_1 = round(quantile(IRR, 0.25, na.rm = T), 4),
    IRR_2 = round(quantile(IRR, 0.5, na.rm = T), 4),
    IRR_3 = round(quantile(IRR, 0.75, na.rm = T), 4),
    .groups = "drop"
  ) |>
  arrange(Periods)

write.xlsx(
  Data,
  "./outcome/appendix/Supplementary Appendix 4.xlsx"
)

# plot --------------------------------------------------------------------

# figure data
data_fig <- list()

for (i in 1:4) {
  Class <- disease_groups[i]
  # save figure data
  DataTable1 <- DataAll |>
    filter(class == Class) |>
    mutate(Periods = if_else(Periods == 'Post-epidemic period', 'Post-epidemic period', 'PHSMs and epidemic periods')) |> 
    group_by(class, disease_en, Periods) |>
    summarise(
      q2 = quantile(IRR, 0.5, na.rm = T),
      q1 = quantile(IRR, 0.25, na.rm = T),
      q3 = quantile(IRR, 0.75, na.rm = T),
      s = wilcox.test(IRR, mu = 1)$statistic,
      P = round(wilcox.test(IRR, mu = 1)$p.value, 4),
      .groups = "drop"
    ) |>
    as.data.frame()
  data_fig[[paste("panel", LETTERS[i * 2 - 1])]] <- DataTable1


  DataTable2 <- DataAll |>
    filter(class == Class) |>
    mutate(
      date = format(ymd(date), "%Y.%m"),
      label = if_else(IRR > 4, "*", "")
    ) |>
    select(disease_en, class, Periods, date, value, mean, IRR, label) |>
    as.data.frame()
  data_fig[[paste("panel", LETTERS[i * 2])]] <- DataTable2

  remove(DataTable1, DataTable2)
}

plot_rr <- function(i) {
  Class <- disease_groups[i]
  fig1 <- ggplot(data = data_fig[[paste("panel", LETTERS[i * 2 - 1])]], 
         mapping = aes(y = disease_en, x = q2, xmin = q1, xmax = q3)) +
    geom_vline(xintercept = 1,
               show.legend = F,
               linetype = "longdash") +
    geom_linerange(aes(color = Periods,
                       group = interaction(disease_en, Periods)),
                   position=position_dodge(width=c(0.6))) +
    geom_point(aes(color = Periods,
                   group = interaction(disease_en, Periods)),
               position=position_dodge(width=c(0.6)),) +
    scale_y_discrete(limits = rev(datafile_class$disease[datafile_class$class == Class])) +
    scale_x_continuous(limits = c(0, 4), breaks = 0:4) +
    scale_color_manual(values = fill_color_disease[3:4]) +
    theme_bw() +
    theme(axis.text = element_text(size = 10.5, color = "black"),
          axis.title = element_text(size = 10.5, color = "black", face = "bold"))+
    labs(
      x = NULL,
      y = NULL,
      title = paste0(LETTERS[2 * i - 1]),
      fill = NULL
    )
  
  fig2 <- ggplot(
    data = data_fig[[paste("panel", LETTERS[i * 2])]],
    mapping = aes(
      fill = IRR,
      x = date,
      y = disease_en
    )
  ) +
    geom_tile() +
    geom_vline(xintercept = c(3.5, 34.5, 37.5)) +
    geom_text(
      mapping = aes(
        label = label
      ),
      vjust = 0.5
    ) +
    scale_fill_gradientn(
      colors = paletteer_d("awtools::a_palette"),
      limits = c(0, 4)
    ) +
    scale_x_discrete(
      breaks = paste(c(2020, 2021, 2022, 2023), "01", sep = "."),
      labels = 2020:2023,
      expand = expansion(add = c(0, 0))
    ) +
    scale_y_discrete(
      limits = rev(datafile_class$disease[datafile_class$class == Class]),
      expand = c(0, 0)
    ) +
    theme_bw() +
    theme(
      axis.text.y = element_blank(),
      legend.position = "bottom",
      plot.title.position = "plot"
    ) +
    guides(fill = guide_colourbar(barwidth = 15, barheight = 0.5, color = "black")) +
    labs(
      x = NULL,
      y = NULL,
      fill = "Adjusted IRR",
      title = paste0(LETTERS[2 * i])
    )
  fig1 + fig2 + plot_layout(widths = c(1.5, 3))
}

outcome <- lapply(1:4, plot_rr)

plot <- do.call(wrap_plots, c(outcome, ncol = 1, byrow = FALSE)) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave("./outcome/publish/fig6.pdf",
  plot,
  family = "Times New Roman",
  limitsize = FALSE, device = cairo_pdf,
  width = 10, height = 8
)

write.xlsx(data_fig,
  file = "./outcome/appendix/Figure Data/Fig.6 data.xlsx"
)
