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

remove(list = ls())

source("./script/theme_set.R")

# data --------------------------------------------------------------------

# left border
split_date_0 <- as.Date("2020/1/1")
split_date_1 <- as.Date("2020/4/1")
split_date_2 <- as.Date("2022/11/1")
split_date_3 <- as.Date("2023/4/1")

datafile_class <- read.xlsx("./outcome/appendix/Figure Data/Fig.1 data.xlsx",
  sheet = "panel A"
) |>
  select(-c(value, label))

DataAll <- list.files(
  path = "./outcome/appendix/forecast/",
  pattern = "*.xlsx",
  full.names = TRUE
) |>
  lapply(read.xlsx, detectDates = T) |>
  bind_rows()

datafile_class$disease <- factor(datafile_class$disease,
  levels = datafile_class$disease
)
datafile_class$class <- factor(datafile_class$class,
  levels = unique(datafile_class$class)
)

DataAll <- DataAll |>
  left_join(datafile_class,
    by = c("disease_en" = "disease")
  ) |>
  mutate(
    RR = value / mean,
    Periods = case_when(
      date < split_date_1 ~ "Pre-epidemic Periods",
      date >= split_date_1 & date < split_date_2 ~ "PHSMs Periods",
      date >= split_date_2 & date < split_date_3 ~ "Epidemic Periods",
      date >= split_date_3 ~ "Post-epidemic Period"
    )
  )

# summary -----------------------------------------------------------------

Data <- DataAll |>
  group_by(disease_en, class, Periods) |>
  summarise(
    diff = round(sum(diff), 2),
    percnet = round(sum(diff) / sum(mean), 4),
    .groups = "drop"
  ) |>
  arrange(Periods)

write.xlsx(
  Data,
  "./outcome/appendix/Table S1.xlsx"
)

Data <- DataAll |>
  group_by(class, disease_en, Periods) |>
  summarise(
    q2 = quantile(RR, 0.5, na.rm = T),
    q1 = quantile(RR, 0.25, na.rm = T),
    q3 = quantile(RR, 0.75, na.rm = T),
    s = wilcox.test(RR, mu = 1)$statistic,
    P = round(wilcox.test(RR, mu = 1)$p.value, 4),
    .groups = "drop"
  )

write.xlsx(
  Data,
  "./outcome/appendix/Table S2.xlsx"
)

# plot --------------------------------------------------------------------


plot_rr <- function(i) {
  Class <- levels(datafile_class$class)[i]
  Data <- DataAll |>
    filter(class == Class) |>
    mutate(date = format(ymd(date), "%Y.%m"))
  fill_value <- fill_color[i]

  fig1 <- ggplot(data = Data) +
    geom_vline(
      xintercept = 1,
      show.legend = F,
      linetype = "longdash"
    ) +
    geom_boxplot(mapping = aes(
      y = disease_en,
      x = RR,
      fill = class
    ),
    show.legend = F) +
    scale_y_discrete(limits = rev(datafile_class$disease[datafile_class$class == Class])) +
    scale_x_continuous(limits = c(0, 4), breaks = 0:4) +
    scale_fill_manual(values = fill_value) +
    theme_bw() +
    labs(
      x = NULL,
      y = NULL,
      title = paste0(LETTERS[2 * i - 1]),
      fill = NULL
    )
  fig2 <- ggplot(
    data = Data,
    mapping = aes(
      fill = RR,
      x = date,
      y = disease_en
    )
  ) +
    geom_tile() +
    geom_vline(xintercept = c(3.5, 34.5)) +
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
      legend.position = "bottom"
    ) +
    guides(fill = guide_colourbar(barwidth = 20, barheight = 0.5, color = "black")) +
    labs(
      x = NULL,
      y = NULL,
      title = paste0(LETTERS[2 * i])
    )
  fig1 + fig2 + plot_layout(widths = c(1, 3))
}

outcome <- lapply(1:4, plot_rr)

plot <- do.call(wrap_plots, c(outcome, ncol = 1, byrow = FALSE)) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# plot cluster ----------------------------------------------------------------

set.seed(20231021)

# cluster for incidence
datafile_analysis <- read.xlsx('./data/nation_and_provinces.xlsx',
                               detectDates = T, sheet = 'Nation') |>
  filter(date >= as.Date("2008-1-1") & date < split_date_0)

DataMatInci <- datafile_analysis |> 
  filter(disease_en %in% datafile_class$disease) |> 
  select(date, disease_en, value) |> 
  rename(c(disease = 'disease_en')) |> 
  mutate(disease = factor(disease,
                          levels = datafile_class$disease,
                          labels = datafile_class$disease),
         phase = case_when(date < split_date_1 ~ 'Pre-epidemic Periods',
                           date >= split_date_1 & date < split_date_2 ~ 'PHSMs Periods',
                           date >= split_date_2 & date < split_date_3 ~ 'Epidemic Periods',
                           date >= split_date_3 ~ 'Post-epidemic Period'),
         phase = factor(phase,
                        levels = c('Pre-epidemic Periods', 'PHSMs Periods', 'Epidemic Periods', 'Post-epidemic Period')),
         value = as.integer(value)) |> 
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
# DataMatInci <- scale(DataMatInci)

# cluster for report case
DataMatRR <- DataAll |>
  select(RR, date, disease_en) |>
  pivot_wider(
    names_from = date,
    values_from = RR
  )

diseasename <- DataMatRR$disease_en
DataMatRR <- DataMatRR |>
  select(-disease_en) |>
  as.matrix()
rownames(DataMatRR) <- diseasename
# DataMatRR <- scale(DataMatRR)

## Pre-epidemic incidence
hcdata <- hkmeans(DataMatInci, 2)
fig1 <- fviz_dend(hcdata,
  cex = 0.6,
  k_colors = fill_color_disease[2:1],
  rect = TRUE,
  rect_fill = TRUE,
  main = LETTERS[9]
) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )
hcdata <- hkmeans(DataMatRR[, 1:3], 2)
fig2 <- fviz_dend(hcdata,
  cex = 0.6,
  k_colors = fill_color_disease[5:4],
  rect = TRUE,
  rect_fill = TRUE,
  main = LETTERS[10]
) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

## PHSMs period II
hcdata <- hkmeans(DataMatRR[, 4:34], 2)
fig3 <- fviz_dend(hcdata,
  cex = 0.6,
  k_colors = fill_color_disease[5:4],
  rect = TRUE,
  rect_fill = TRUE,
  main = LETTERS[11]
) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

## Epidemic period
hcdata <- hkmeans(DataMatRR[, 35:39], 2)
fig4 <- fviz_dend(hcdata,
  cex = 0.6,
  k_colors = fill_color_disease[5:4],
  rect = TRUE,
  rect_fill = TRUE,
  main = LETTERS[12]
) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

plot1 <- fig1 + fig2 + fig3 + fig4 +
  plot_layout(ncol = 2, byrow = T)


ggsave("./outcome/publish/fig5.pdf",
  cowplot::plot_grid(plot, plot1, ncol = 1, rel_heights = c(2.2, 2)),
  family = "Times New Roman",
  limitsize = FALSE, device = cairo_pdf,
  width = 14, height = 14
)
