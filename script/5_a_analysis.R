library(tidyverse)
library(patchwork)
library(openxlsx)
library(ggtext)

source("./script/theme_set.R")
source("./script/ggplot.R")

# read data ---------------------------------------------------------------
datafile_class <- read.xlsx("./outcome/appendix/Figure Data/Fig.1 data.xlsx", sheet = "panel A") |>
     select(-c(value, label))
file_list <- paste0("./outcome/appendix/forecast/", datafile_class$disease, ".xlsx")
data_list <- lapply(file_list, read.xlsx, detectDates = T)
names(data_list) <- paste0(LETTERS[1:24], " ", datafile_class$disease)

data_list <- do.call("rbind", data_list) |>
     rename(disease = 'disease_en') |> 
     left_join(datafile_class, by = "disease") |>
     mutate(disease = factor(disease, levels = datafile_class$disease, labels = datafile_class$disease),
            phase = case_when(
                 date < split_dates[1] ~ split_periods[1],
                 date >= split_dates[1] &
                      date < split_dates[2] ~ split_periods[2],
                 date >= split_dates[2] &
                      date < split_dates[3] ~ split_periods[3],
                 date >= split_dates[3] &
                      date < split_dates[4] ~ split_periods[4],
                 date >= split_dates[4] ~ split_periods[5]),
            phase = factor(phase, levels = split_periods)) |>
     filter(!is.na(class))

data_scale <- data_list |>
     group_by(date, class) |>
     summarise(diff = sum(diff),
               .groups = "drop") |>
     group_by(class) |>
     summarise(diff = max(diff),
               .groups = "drop")

# plot --------------------------------------------------------------------

data_fig <- list()

for (i in 1:4) {
     data_single_group <- data_list |>
          filter(class == disease_groups[i])
     
     data_fig[[paste("panel", LETTERS[i])]] <- data_single_group
     
     fig <- ggplot(data = data_single_group) +
          geom_col(mapping = aes(
               x = date,
               y = diff,
               fill = disease
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
               legend.position = c(0.01, 0.42),
               legend.justification = c(0, 1)
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

fig5 <- ggplot(data = data_single_group) +
     geom_col(mapping = aes(
          x = date,
          y = diff,
          fill = disease
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
     theme(
          legend.position = "none",
          plot.background = element_rect(color = "black")
     ) +
     labs(
          x = NULL,
          y = NULL,
          fill = NULL,
          title = LETTERS[5]
     )
data_fig[[paste("panel", LETTERS[5])]] <- data_list |>
     filter(class == disease_groups[4])

fig4 <- fig4 + inset_element(fig5, left = 0.1, bottom = 0.55, right = 1, top = 1)

plot1 <- fig1 + fig2 + fig3 + fig4

ggsave("./outcome/publish/fig5.pdf",
       plot1,
       family = "Times New Roman",
       limitsize = FALSE, device = cairo_pdf,
       width = 12, height = 7
)

write.xlsx(data_fig,
           file = "./outcome/appendix/Figure Data/Fig.5 data.xlsx"
)
