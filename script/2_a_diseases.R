# packages ----------------------------------------------------------------

library(openxlsx)
library(tidyverse)
library(ggsci)
library(paletteer)
library(patchwork)
library(tseries)
library(scales)

# data --------------------------------------------------------------------

source("./script/theme_set.R")

datafile_analysis <- read.xlsx("./data/nation_and_provinces.xlsx",
  detectDates = T, sheet = "Nation"
) |>
  filter(date >= as.Date("2008-1-1"))

datafile_class <- read.xlsx("./outcome/appendix/Figure Data/Fig.1 data.xlsx",
  sheet = "panel A"
) |>
  select(-c(value, label))


# left border
split_date_0 <- as.Date("2020/1/1")
split_date_1 <- as.Date("2020/4/1")
split_date_2 <- as.Date("2022/11/1")
split_date_3 <- as.Date("2023/4/1")

# group plot -------------------------------------------------------------

datafile_plot <- datafile_analysis |>
  filter(disease_en %in% datafile_class$disease) |>
  select(date, disease_en, value) |>
  rename(c(disease = "disease_en")) |>
  mutate(
    disease = factor(disease,
      levels = datafile_class$disease,
      labels = datafile_class$disease
    ),
    phase = case_when(
      date < split_date_1 ~ "Pre-epidemic Periods",
      date >= split_date_1 & date < split_date_2 ~ "PHSMs Periods",
      date >= split_date_2 & date < split_date_3 ~ "Epidemic Periods",
      date >= split_date_3 ~ "Post-epidemic Period"
    ),
    phase = factor(phase,
      levels = c("Pre-epidemic Periods", "PHSMs Periods", "Epidemic Periods", "Post-epidemic Period")
    ),
    value = as.integer(value)
  ) |>
  left_join(datafile_class, by = c("disease" = "disease")) |>
  mutate(class = factor(class,
    levels = c(
      "Intestinal infectious diseases",
      "Blood borne and sexually transmitted diseases",
      "Respiratory infectious diseases",
      "Zoonotic infectious diseases"
    )
  ))

# background rect ---------------------------------------------------------

datafile_rect <- data.frame(
  start = c(min(datafile_plot$date), split_date_0, split_date_1, split_date_2, split_date_3),
  end = c(split_date_0, split_date_1, split_date_2, split_date_3, max(datafile_plot$date)),
  label = c("Pre-epidemic Period", "PHSMs Period I", "PHSMs Period II", "Epidemic Period", "Post-epidemic Period")
) |>
  mutate(m = as.Date((as.numeric(start) + as.numeric(end)) / 2, origin = "1970-01-01"))

datafile_group <- datafile_plot |>
  group_by(phase, date, class) |>
  summarise(
    value = sum(value),
    .groups = "drop"
  ) |>
  mutate(class = factor(class,
    levels = unique(datafile_class$class)
  ))

# complete missing data
table(datafile_plot[, "disease"])

# lineplot ----------------------------------------------------------------

data_fig <- list()

group_lists <- levels(datafile_group$class)

for (i in 1:4) {
  group_list <- group_lists[i]
  data_fig[[LETTERS[i*2-1]]] <- datafile_group |>
       filter(class == group_list) |>
       mutate(
            year = year(date),
            month = month(date),
            date = format(ymd(date), "%Y.%m")
       )
  data_fig[[LETTERS[i*2]]] <- datafile_plot |>
       filter(class == group_list) |>
       group_by(disease) |>
       mutate(
            year = year(date),
            month = month(date),
            value_norm = (value - mean(value, na.rm = T)) / sd(value, na.rm = T),
            date = format(ymd(date), "%Y.%m")
       )
}

plot_single <- function(i) {
  group_list <- group_lists[i]
  datafile_group_single <- datafile_group |>
    filter(class == group_list)
  datafile_plot_single <- data_fig[[LETTERS[i*2]]] |> 
    mutate(out_label = if_else(value_norm > 10,
                               '*',
                               ''))

  fig1 <- ggplot(data = datafile_group_single) +
    geom_rect(
      data = datafile_rect,
      aes(
        xmin = start,
        xmax = end,
        fill = label
      ),
      ymin = -Inf,
      ymax = Inf,
      alpha = 0.2,
      show.legend = F
    ) +
    geom_line(
      mapping = aes(
        x = date,
        y = value
      ),
      color = fill_color[i]
    ) +
    scale_x_date(
      expand = expansion(add = c(15, 15)),
      date_breaks = "1 year",
      date_labels = "%Y"
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0.15, 0.25)),
      breaks = pretty(datafile_group_single$value),
      labels = scientific_10
    ) +
    scale_fill_manual(values = back_color) +
    theme_bw() +
    theme(
      legend.position = "none",
      axis.text.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.text = element_text(size = 10.5, color = "black"),
      axis.title.y = element_text(size = 11, color = "black", face = 'bold'),
      plot.title = element_text(face = "bold", size = 12, color = "black"),
      plot.title.position = 'plot'
    ) +
    labs(
      x = NULL,
      y = "Monthly incidence",
      color = NULL,
      title = LETTERS[i*2-1]
    )

  fig2 <- ggplot(
    data = datafile_plot_single,
    mapping = aes(
      fill = value_norm,
      x = date,
      y = disease
    )
  ) +
    geom_tile() +
    geom_text(
      mapping = aes(
        label = out_label
      ),
      vjust = 0.5
    )+
    coord_equal(3) +
    scale_fill_gradientn(
      colors = paletteer_d("awtools::a_palette"),
      trans = log_fill,
      limits = c(-5, 10)
    ) +
    scale_x_discrete(
      breaks = paste(seq(2008, 2023), "01", sep = "."),
      labels = 2008:2023,
      expand = expansion(add = c(0, 0))
    ) +
    scale_y_discrete(
      limits = rev(datafile_class$disease[datafile_class$class == group_list]),
      expand = c(0, 0)
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      panel.grid = element_blank(),
      axis.text = element_text(size = 10.5, color = "black"),
      plot.title = element_text(face = "bold", size = 12, color = "black"),
      plot.title.position = 'plot'
    ) +
    guides(fill = guide_colourbar(barwidth = 20, barheight = 0.5, color = "black")) +
    labs(
      x = "Date",
      y = NULL,
      fill = "Normalized monthly incidence",
      title = LETTERS[i*2]
    )

  return(fig1 + fig2 + plot_layout(ncol = 1, heights = c(1, 1)))
}

## create figure panel for all class
plot_list <- lapply(1:length(group_lists), plot_single)
fig <- wrap_plots(plotlist = plot_list, ncol = 1) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave(
  filename = "./outcome/publish/fig2.pdf",
  plot = fig,
  width = 12,
  height = 16,
  device = cairo_pdf,
  family = "Times New Roman"
)

# Seasonal Decomposition --------------------------------------------------

# figure data
write.xlsx(data_fig,
  file = "./outcome/appendix/Figure Data/Fig.2 data.xlsx"
)

((587402/272938)^(1/11) - 1) * 100
((260704/118201)^(1/11) - 1) * 100
((72630/12409)^(1/11) - 1) * 100
72630/12409
