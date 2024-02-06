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
  path = "./outcome/appendix/data/forecast/",
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
  "./outcome/appendix/data/Table S3.xlsx"
)

Data <- DataAll |>
  group_by(class, disease_en, Periods) |>
  summarise(
    q2 = quantile(RR, 0.5),
    q1 = quantile(RR, 0.25),
    q3 = quantile(RR, 0.75),
    s = wilcox.test(RR, mu = 1)$statistic,
    P = round(wilcox.test(RR, mu = 1)$p.value, 4),
    .groups = "drop"
  )
write.xlsx(
  Data,
  "./outcome/appendix/data/Table S4.xlsx"
)

# plot --------------------------------------------------------------------

i <- 1

plot_rr <- function(i) {
  Class <- unique(datafile_class$class)[i]
  Data <- DataAll |>
    filter(class == Class) |>
    mutate(date = format(ymd(date), "%Y.%m"))
  fill_value <- fill_color[1:length(unique(Data$diseasename))]
  names(fill_value) <- unique(Data$diseasename)

  fig1 <- ggplot(data = Data) +
    geom_vline(
      xintercept = 1,
      show.legend = F,
      linetype = "longdash"
    ) +
    geom_boxplot(mapping = aes(
      y = diseasename,
      x = RR,
      fill = class
    )) +
    scale_y_discrete(limits = datafile_class$diseasename[datafile_class$class == Class]) +
    scale_x_continuous(limits = c(0, 3), breaks = 0:3) +
    scale_fill_manual(values = fill_value[i]) +
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
      y = diseasename
    )
  ) +
    geom_tile() +
    geom_vline(xintercept = c(3.5, 34.5)) +
    scale_fill_gradientn(
      colors = paletteer_d("awtools::a_palette"),
      limits = c(0, 3)
    ) +
    scale_x_discrete(
      breaks = paste(c(2020, 2021, 2022, 2023), "01", sep = "."),
      labels = 2020:2023,
      expand = expansion(add = c(0, 0))
    ) +
    scale_y_discrete(
      limits = datafile_class$diseasename[datafile_class$class == Class],
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
datafile_analysis <- read.xlsx("./data/Nation.xlsx", detectDates = T) |>
  filter(date >= as.Date("2008-1-1") & date < split_date_0)
datafile_class <- read.xlsx("./data/disease_class.xlsx", detectDates = T)

DataMatInci <- datafile_analysis |>
  filter(disease_1 %in% datafile_class$diseaselist) |>
  select(date, disease_1, value) |>
  complete(
    date = seq.Date(min(datafile_analysis$date), max(datafile_analysis$date), by = "month"),
    disease_1 = unique(datafile_class$diseaselist),
    fill = list(value = 0)
  ) |>
  mutate(year = year(date)) |>
  mutate(
    disease = factor(disease_1,
      levels = datafile_class$diseaselist,
      labels = datafile_class$diseasename
    ),
    phase = case_when(
      date < split_date_1 ~ "Pre-epidemic Periods",
      date > split_date_1 & date < split_date_2 ~ "PHSMs Periods",
      date > split_date_2 ~ "Epidemic Periods",
    ),
    phase = factor(phase,
      levels = c("Pre-epidemic Periods", "PHSMs Periods", "Epidemic Periods")
    )
  ) |>
  left_join(datafile_class, by = c("disease" = "diseasename")) |>
  mutate(class = factor(class,
    levels = c(
      "Blood borne and sexually transmitted diseases",
      "Intestinal infectious diseases",
      "Respiratory infectious diseases",
      "Natural focal diseases"
    )
  )) |>
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
# cluster for report case
DataMatRR <- DataAll |>
  select(RR, date, diseasename) |>
  pivot_wider(
    names_from = date,
    values_from = RR
  )
diseasename <- DataMatRR$diseasename
DataMatRR <- DataMatRR |>
  select(-diseasename) |>
  as.matrix()
rownames(DataMatRR) <- diseasename
DataMatRR <- scale(DataMatRR)

## PHSMs period I
hcdata <- hkmeans(DataMatInci, 2)
fig1 <- fviz_dend(hcdata,
  cex = 0.6,
  k_colors = fill_color[3:2],
  rect = TRUE,
  rect_fill = TRUE,
  main = "I"
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
  k_colors = fill_color[5:4],
  rect = TRUE,
  rect_fill = TRUE,
  main = "J"
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
  k_colors = fill_color[5:4],
  rect = TRUE,
  rect_fill = TRUE,
  main = "K"
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
  k_colors = fill_color[4:5],
  rect = TRUE,
  rect_fill = TRUE,
  main = "L"
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
