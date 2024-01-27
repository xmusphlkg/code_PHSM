
# packages ----------------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(ggsci)
library(Cairo)
library(patchwork)
library(paletteer)

remove(list = ls())

source('./script/theme_set.R')
source('./script/ggplot.R')

# data --------------------------------------------------------------------

DataRaw <- read.xlsx("./outcome/appendix/model/pre-epidemic.xlsx")
datafile_class <- read.xlsx('./outcome/appendix/data/Fig.1 data.xlsx',
                            sheet = 'panel A') |> 
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
          norR2 = (R_Squared - mean(R_Squared, na.rm =T)) / sd(R_Squared, na.rm =T),
          norSMAPE = -(SMAPE - mean(SMAPE, na.rm =T)) / sd(SMAPE, na.rm =T),  # 取负值
          norRMSE = -(RMSE - mean(RMSE, na.rm =T)) / sd(RMSE, na.rm =T),  # 取负值
          norMASE = -(MASE - mean(MASE, na.rm =T)) / sd(MASE, na.rm =T),  # 取负值
          Index = sum(norR2, norSMAPE, norRMSE, norMASE, na.rm = T)
     ) |> 
     rowwise() |>
     mutate(
          Index = sum(c_across(norR2:norMASE), na.rm = T)
     )|>
     ungroup()

## find the best method for each disease based on the maximum index
DataClean <- DataClean |>
  group_by(disease) |>
  mutate(
    Best = Method[which.max(Index)],
  ) |>
  ungroup()

## save select
DataSelect <- DataClean |>
  select(disease, Best) |>
  distinct()

write.xlsx(DataSelect, "./outcome/appendix/model/select.xlsx")

## save normalized composite index
DataTable <- DataClean |> 
     mutate(across(where(is.numeric), ~round(., 2))) |>
     select(disease, Method, Best, Index, contains('nor'))
write.xlsx(DataTable, 
           "./outcome/appendix/data/Fig.3 data.xlsx")

# plot for each model -----------------------------------------------------

DataClean$Best <- as.numeric(DataClean$Method == DataClean$Best)
DataClean$Method <- factor(DataClean$Method,
                           levels = c('Neural Network', 'Prophet', 
                                      'ETS', 'SARIMA', 'Hybrid', 'Bayesian Structural'),
                           labels = c('Neural Network', 'Prophet', 
                                      'ETS', 'SARIMA', 'Hybrid*', 'Bayesian Structural')
)

diseases <- datafile_class$disease

layout <- '
ABCDEFG
HIJKLZZ
MNOPQZZ
RSTVWXY
'

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
          scale_fill_manual(
               values = fill_color[c(1, 3)],
               labels = c("Alternative Models", "Best Model")
          ) +
          theme_bw() +
          labs(
               x = NULL,
               y = NULL,
               fill = NULL,
               title = paste0(LETTERS[i], ': ', diseases[i])
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
     plot_layout(design = layout, guides = 'collect')&
     theme(plot.title = element_text(face = "bold", size = 14, hjust = 0),
           legend.text = element_text(face = 'bold', size = 12),
           legend.title = element_text(face = 'bold', size = 12),
           legend.box.background = element_rect(fill = "transparent", colour = 'transparent'),
           legend.background = element_rect(fill = "transparent", colour = 'transparent'),
           axis.title.x = element_text(face = 'bold', color = 'black'),
           axis.text.x = element_text(color = 'black'))

ggsave('./outcome/publish/fig3.pdf',
       plot,
       family = "Times New Roman",
       limitsize = FALSE, device = cairo_pdf,
       width = 15, height = 9)
