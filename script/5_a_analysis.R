library(tidyverse)
library(patchwork)
library(openxlsx)

source("./script/theme_set.R")
source("./script/ggplot.R")

scientific_10 <- function(x) {
  ifelse(x == 0, 0, parse(text = gsub("[+]", "", gsub("e", "%*%10^", scales::scientific_format()(x)))))
}

# read data ---------------------------------------------------------------
datafile_class <- read.xlsx("./outcome/appendix/Figure Data/Fig.1 data.xlsx",
  sheet = "panel A"
) |>
  select(-c(value, label))
file_list <- paste0(
  "./outcome/appendix/forecast/",
  datafile_class$disease,
  ".xlsx"
)
data_list <- lapply(file_list, read.xlsx, detectDates = T)
names(data_list) <- paste0(LETTERS[1:24], " ", datafile_class$disease)

data_list <- do.call("rbind", data_list) |>
  left_join(datafile_class, by = c(disease_en = "disease")) |>
  mutate(disease_en = factor(disease_en,
    levels = datafile_class$disease
  )) |> 
  filter(!is.na(class))

group_lists <- unique(datafile_class$class)
data_scale <- data_list |>
  group_by(date, class) |>
  summarise(
    diff = sum(diff),
    .groups = "drop"
  ) |>
  group_by(class) |>
  summarise(
    diff = max(diff),
    .groups = "drop"
  )

# plot --------------------------------------------------------------------

for (i in 1:4) {
  data_single_group <- data_list |>
    filter(class == group_lists[i])

  fig <- ggplot(data = data_single_group) +
    geom_col(mapping = aes(
      x = date,
      y = diff,
      fill = disease_en
    )) +
    geom_hline(yintercept = 0) +
    scale_x_date(
      expand = expansion(add = c(15, 15)),
      date_breaks = "1 year",
      date_labels = "%Y"
    ) +
    scale_y_continuous(
      limits = c(-max(data_scale$diff), max(data_scale$diff)),
      expand = expansion(mult = c(0.15, 0.15)),
      labels = scientific_10
    ) +
    scale_fill_manual(values = fill_color_disease) +
    theme_set() +
    theme(
      legend.position = c(0.01, 0.01),
      legend.justification = c(0, 0)
    ) +
    labs(
      x = "Date",
      y = "Difference",
      fill = NULL,
      title = LETTERS[i]
    ) +
    guides(fill = guide_legend(
      ncol = 3,
      byrow = T,
      title = NULL
    ))


  assign(paste0("fig", i), fig)
}

plot <- fig1 + fig2 + fig3 + fig4

ggsave("./outcome/publish/fig5.pdf",
  plot,
  family = "Times New Roman",
  limitsize = FALSE, device = cairo_pdf,
  width = 12, height = 7
)

fig <- ggplot(data = data_single_group) +
  geom_col(mapping = aes(
    x = date,
    y = diff,
    fill = disease_en
  )) +
  geom_hline(yintercept = 0) +
  theme_set() +
  scale_x_date(
    expand = expansion(add = c(15, 15)),
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.15, 0.15)),
    labels = scientific_10
  ) +
  scale_fill_manual(values = fill_color_disease) +
  theme(legend.position = "none") +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL,
    title = NULL
  ) +
  guides(fill = guide_legend(
    ncol = 3,
    byrow = T,
    title = LETTERS[5]
  ))

ggsave("./outcome/publish/fig5_1.pdf",
  fig,
  family = "Times New Roman",
  limitsize = FALSE, device = cairo_pdf,
  width = 6, height = 3
)

data_fig <- list()
for (i in 1:length(group_lists)) {
     data_fig[[paste('panel', LETTERS[i])]] <- data_list |>
          filter(class == group_lists[i])
}
data_fig[[paste('panel', LETTERS[5])]] <- data_list |>
     filter(class == group_lists[4])

write.xlsx(data_fig,
           file = './outcome/appendix/Figure Data/Fig.5 data.xlsx')

