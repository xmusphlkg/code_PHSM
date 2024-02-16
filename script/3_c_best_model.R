# packages ----------------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(ggsci)
library(Cairo)
library(patchwork)
library(paletteer)

remove(list = ls())

source("./script/theme_set.R")
source("./script/ggplot.R")

# data --------------------------------------------------------------------

DataRaw <- read.xlsx("./outcome/appendix/Supplementary Appendix 2.xlsx")
datafile_class <- read.xlsx("./outcome/appendix/Figure Data/Fig.1 data.xlsx",
  sheet = "panel A"
) |>
  select(-c(value, label))

DataRaw$disease <- factor(DataRaw$disease, levels = datafile_class$disease)

# best model --------------------------------------------------------------

DataClean <- DataRaw |>
  select(disease, Index, Method, Test) |>
  pivot_wider(names_from = Index, values_from = Test)

## z-normalization for each disease
DataClean <- DataClean |>
  group_by(disease) |>
  mutate(
    # norR2 = (R_Squared - mean(R_Squared, na.rm = T)) / sd(R_Squared, na.rm = T),
    norSMAPE = (SMAPE - mean(SMAPE, na.rm = T)) / sd(SMAPE, na.rm = T),
    norRMSE = (RMSE - mean(RMSE, na.rm = T)) / sd(RMSE, na.rm = T),
    norMASE = (MASE - mean(MASE, na.rm = T)) / sd(MASE, na.rm = T),
    Index = sum(norSMAPE, norRMSE, norMASE, na.rm = T)
  ) |>
  rowwise() |>
  mutate(
    Index = sum(c_across(norSMAPE:norMASE), na.rm = T)
  ) |>
  # mutate(Index = SMAPE) |> 
  ungroup()

## find the best method for each disease based on the maximum index
DataClean <- DataClean |>
  group_by(disease) |>
  mutate(
    Best = Method[which.min(Index)]
  ) |>
  ungroup()
DataClean$Best <- as.numeric(DataClean$Method == DataClean$Best)
DataClean$Method <- factor(DataClean$Method,
                           levels = c(
                                "Neural Network", "Prophet",
                                "ETS", "SARIMA", "Hybrid", "Bayesian Structural"
                           ),
                           labels = c(
                                "Neural Network", "Prophet",
                                "ETS", "SARIMA", "Hybrid*", "Bayesian Structural"
                           )
)
diseases <- datafile_class$disease

## save normalized composite index
DataTable <- DataClean |>
  mutate(across(where(is.numeric), ~ round(., 2))) |>
  select(disease, Method, Best, Index, contains("nor"))
write.xlsx(
  DataTable,
  "./outcome/appendix/Figure Data/Fig.3 data.xlsx"
)

# plot for each model -----------------------------------------------------

layout <- "
ABCDEFG
HIJKLZZ
MNOPQZZ
RSTVWXY
"

plot_function <- function(i, diseases) {
  Data <- DataClean |>
    filter(disease == diseases[i])

  fig <- ggplot(
    data = Data,
    mapping = aes(
      y = Method,
      x = Index,
      fill = as.factor(Best)
    )
  ) +
    geom_col() +
    # geom_text(mapping = aes(
    #      label = sprintf("%.2f", Index),
    #      hjust = Index >= 0
    # ))+
    scale_y_discrete(limits = rev(levels(Data$Method))) +
    scale_x_continuous(limits = c(-6, 6))+
    scale_fill_manual(
      values = c("#E64B35FF", "#00A087FF"),
      labels = c("Alternative Models", "Best Model")
    ) +
    theme_bw() +
    labs(
      x = NULL,
      y = NULL,
      fill = NULL,
      title = paste0(LETTERS[i], ": ", diseases[i])
    )
  if (i %in% c(1, 8, 13, 18)) {
    fig <- fig +
      theme(
        legend.position = c(0.9, 0.1),
        axis.text = element_text(color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
      )
  } else {
    fig <- fig +
      theme(
        legend.position = c(0.9, 0.1),
        axis.text = element_text(color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_blank()
      )
  }
  return(fig)
}

outcome <- lapply(1:24, plot_function, diseases = diseases)
outcome[[25]] <- guide_area()

plot <- do.call(wrap_plots, outcome) +
  plot_layout(design = layout, guides = "collect") &
  theme(
    plot.title = element_text(face = "bold", size = 10, hjust = 0),
    legend.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.box.background = element_rect(fill = "transparent", colour = "transparent"),
    legend.background = element_rect(fill = "transparent", colour = "transparent"),
    axis.title.x = element_text(face = "bold", color = "black"),
    axis.text.x = element_text(color = "black")
  )

ggsave("./outcome/publish/fig3.pdf",
  plot,
  family = "Times New Roman",
  limitsize = FALSE, device = cairo_pdf,
  width = 12, height = 6
)
