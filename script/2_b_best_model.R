#####################################
## @Description:
## @version:
## @Author: Li Kangguo
## @Date: 2023-10-19 10:21:17
## @LastEditors: Li Kangguo
## @LastEditTime: 2023-10-19 12:07:03
#####################################

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

DataRaw <- read.xlsx("./outcome/appendix/model/index/pre-epidemic.xlsx")
DataRaw$Index <- rep(c("RMSE", "R-squared", "MAE"), times = nrow(DataRaw) / 3)
datafile_class <- read.xlsx("./data/disease_class.xlsx")

DataRaw$disease <- factor(DataRaw$disease, levels = datafile_class$diseasename)

# best model --------------------------------------------------------------

DataClean <- DataRaw |>
  select(disease, Index, Method, Test) |>
  pivot_wider(names_from = Index, values_from = Test)

## reverse the sign of rmse and mae
DataClean$`R-squared`[is.na(DataClean$`R-squared`)] <- 0
DataClean$RMSE <- -DataClean$RMSE
DataClean$MAE <- -DataClean$MAE

## z-normalization for each disease
DataClean <- DataClean |>
  group_by(disease) |>
  mutate(
    norRMSE = (RMSE - mean(RMSE)) / sd(RMSE),
    norR2 = (`R-squared` - mean(`R-squared`)) / sd(`R-squared`),
    norMAE = (MAE - mean(MAE)) / sd(MAE),
    Index = norRMSE + norR2 + norMAE
  ) |>
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

write.xlsx(DataSelect, "./outcome/appendix/model/select/pre-epidemic.xlsx")

## save normalized composite index
DataTable <- DataClean |> 
     mutate(across(where(is.numeric), ~round(., 2)))
write.xlsx(DataTable, 
           "./outcome/appendix/data/Table S1.xlsx")

# plot for each model -----------------------------------------------------

DataClean$Best <- as.numeric(DataClean$Method == DataClean$Best)
DataClean$Method <- factor(DataClean$Method,
                           levels = c('Neural Network', 'Prophet', 
                                      'ETS', 'SARIMA', 'Hybrid', 'Bayesian Structural'),
                           labels = c('Neural Network', 'Prophet', 
                                      'ETS', 'SARIMA', 'Hybrid*', 'Bayesian Structural')
)

diseases <- datafile_class$diseasename

layout <- '
ABCDEFG
HIJKLMN
OPQRSZZ
TVWXYZZ
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
          geom_text(mapping = aes(
               label = sprintf("%.2f", Index),
               hjust = Index >= 0
          ))+
          scale_y_discrete(limits = rev(levels(Data$Method))) +
          scale_fill_manual(
               values = fill_color[5:4],
               labels = c("Alternative Models", "Best Model")
          ) +
          theme_bw() +
          labs(
               x = NULL,
               y = NULL,
               fill = NULL,
               title = paste0(LETTERS[i], ': ', diseases[i])
          )
     if (i %in% c(1, 8, 15, 20)) {
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
     theme(
          title = element_text(size = 8)
     )

ggsave('./outcome/publish/fig2.pdf',
       plot,
       family = "Times New Roman",
       limitsize = FALSE, device = cairo_pdf,
       width = 14, height = 8)

ggsave('./outcome/publish/fig2.png',
       plot,
       limitsize = FALSE,
       width = 14, height = 8)
