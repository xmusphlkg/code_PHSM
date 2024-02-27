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
  bind_rows() |> 
  filter(date >= split_date_0)

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
    IRR = (value + 1) / (mean + 1),
    Periods = case_when(
      date < split_date_0 ~ "Pre-epidemic period",
      date >= split_date_0 & date < split_date_1 ~ "PHSMs period I",
      date >= split_date_1 & date < split_date_2 ~ "PHSMs period II",
      date >= split_date_2 & date < split_date_3 ~ "Epidemic period",
      date >= split_date_3 ~ "Post-epidemic period"
    )
  )

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
  Class <- levels(datafile_class$class)[i]
  # save figure data
  DataTable1 <- DataAll |>
    filter(class == Class) |>
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
  data_fig[[paste('panel', LETTERS[i*2-1])]] <- DataTable1
  
  
  DataTable2 <- DataAll |>
    filter(class == Class) |>
    mutate(date = format(ymd(date), "%Y.%m"),
           label = if_else(IRR > 4, '*', '')) |> 
    select(disease_en, class, Periods, date, value, mean, IRR, label) |> 
    as.data.frame()
  data_fig[[paste('panel', LETTERS[i*2])]] <- DataTable2
  
  remove(DataTable1, DataTable2)
}

plot_rr <- function(i) {
  Class <- levels(datafile_class$class)[i]
  Data <- DataAll |>
    filter(class == Class) |>
    mutate(date = format(ymd(date), "%Y.%m"),
           label = if_else(IRR > 4, '*', ''))
  fill_value <- fill_color[i]

  fig1 <- ggplot(data = filter(Data, Periods %in% c("PHSMs period I", "PHSMs period II", "Epidemic period"))) +
    geom_vline(
      xintercept = 1,
      show.legend = F,
      linetype = "longdash"
    ) +
    geom_boxplot(mapping = aes(
      y = disease_en,
      x = IRR,
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
      fill = IRR,
      x = date,
      y = disease_en
    )
  ) +
    geom_tile() +
    geom_vline(xintercept = c(3.5, 34.5, 39.5)) +
    geom_text(
      mapping = aes(
        label = label
        ),
      vjust = 0.5
    )+
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
    guides(fill = guide_colourbar(barwidth = 20, barheight = 0.5, color = "black")) +
    labs(
      x = NULL,
      y = NULL,
      fill = 'Adjusted IRR',
      title = paste0(LETTERS[2 * i])
    )
  fig1 + fig2 + plot_layout(widths = c(1, 3))
}

outcome <- lapply(1:4, plot_rr)

plot <- do.call(wrap_plots, c(outcome, ncol = 1, byrow = FALSE)) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# incidence cluster -------------------------------------------------------

set.seed(20240218)

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
DataMatInci <- scale(DataMatInci)

log_trans_with_zero <- function() {
  trans_new(name = 'log_with_zero',
            trans = function(x) log10(ifelse(x == 0, 0.05, x)),
            inverse = function(x) exp(x))
}

hcdata <- hkmeans(DataMatInci, 2)
fig1 <- fviz_dend(hcdata,
                  cex = 0.6,
                  k_colors = fill_color_disease[9:8],
                  rect = TRUE,
                  rect_fill = TRUE,
                  horiz = TRUE,
                  # type = "circular",
                  main = LETTERS[9]
) +
  theme(
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 12, color = "black"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(face = "bold", size = 14, hjust = 0)
  )+
  scale_y_continuous(trans = scales::pseudo_log_trans(base = 10))

data_fig[[paste0(LETTERS[9])]] <- data.frame(
  disease = names(hcdata$cluster),
  cluster = as.integer(hcdata$cluster)
) |> 
  left_join(
    data.frame(
      disease = rownames(hcdata[["data"]]),
      as.data.frame(hcdata[["data"]])
    )
  )

# IRR cluster -------------------------------------------------------------

set.seed(20240218)

# cluster for IRR
DataMatRR <- DataAll |>
  select(IRR, date, disease_en) |>
  pivot_wider(
    names_from = date,
    values_from = IRR
  )

diseasename <- DataMatRR$disease_en
DataMatRR <- DataMatRR |>
  select(-disease_en) |>
  as.matrix()
rownames(DataMatRR) <- diseasename
# lambda <- BoxCox.lambda(DataMatRR)
# DataMatRR <- BoxCox(DataMatRR, lambda)
DataMatRR <- log(DataMatRR)
DataMatRR <- scale(DataMatRR)

## PHSMs period I
hcdata <- hkmeans(DataMatRR[, 1:40], 4)
fig2 <- fviz_cluster(hcdata,
             data = DataMatRR[, 1:40],
             main = LETTERS[10],
             ggtheme = theme_set(),
             repel = TRUE,
             k_colors = fill_color_disease[5:3],
             palette = "npg"
             )+
  theme(legend.position = 'none')

data_fig[[paste0(LETTERS[10])]] <- data.frame(
  disease = names(hcdata$cluster),
  cluster = as.integer(hcdata$cluster)
) |>
  left_join(
    data.frame(
      disease = rownames(hcdata[["data"]]),
      as.data.frame(hcdata[["data"]])
    )
  )

## PHSMs period II
fig3 <- fviz_cluster(hcdata,
                     data = DataMatRR[, 1:40],
                     main = LETTERS[11],
                     ggtheme = theme_set(),
                     repel = TRUE,
                     k_colors = fill_color_disease[5:3],
                     palette = "npg"
)+
  theme(legend.position = 'none',
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color = 'black'))+
  labs(color = 'Cluster')+
  coord_cartesian(xlim = c(0, 5),
                  ylim = c(-1, 2))

data_fig[[paste0(LETTERS[11])]] <- data.frame(
  disease = names(hcdata$cluster),
  cluster = as.integer(hcdata$cluster)
) |>
  left_join(
    data.frame(
      disease = rownames(hcdata[["data"]]),
      as.data.frame(hcdata[["data"]])
    )
  )

fig2 <- fig2 + inset_element(fig3, left = 0.05, bottom = 0.2, right = 0.55, top = 1)

layout <- "
ACC
ABB
ABB
ABB
"

plot1 <- fig1 + fig2 + guide_area()+
  plot_layout(design = layout)


ggsave("./outcome/publish/fig6.pdf",
  cowplot::plot_grid(plot, plot1, ncol = 1, rel_heights = c(2.2, 2)),
  family = "Times New Roman",
  limitsize = FALSE, device = cairo_pdf,
  width = 14, height = 14
)

write.xlsx(data_fig,
           file = './outcome/appendix/Figure Data/Fig.6 data.xlsx')
