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

# data --------------------------------------------------------------------

DataRaw <- read.xlsx("./outcome/appendix/model/index/pre-epidemic.xlsx")
DataRaw$Index <- rep(c("RMSE", "R-squared", "MAE"), times = nrow(DataRaw) / 3)
DataRaw$disease <- factor(
  DataRaw$disease,
  levels = c(
    "HBV", "HCV", "Syphilis", "AIDS", "Gonorrhea",
    "HAV", "HFMD", "HEV", "Other infectious diarrhea", "Typhoid fever and paratyphoid fever", "Acute hemorrhagic conjunctivitis", "Dysentery",
    "Dengue fever", "Brucellosis", "Malaria", "Japanese encephalitis", "HFRS", "Hydatidosis", "Typhus",
    "Rubella", "Mumps", "Pertussis", "Tuberculosis", "Scarlet fever"
  )
)

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

## save to select
DataSelect <- DataClean |>
  select(disease, Best) |>
  distinct()

write.xlsx(DataSelect, "./outcome/appendix/model/select/pre-epidemic.xlsx")

# plot for each model -----------------------------------------------------

DataClean$Best <- as.numeric(DataClean$Method == DataClean$Best)
DataClean$Method <- factor(DataClean$Method,
  levels = c(
    "Neural Network", "ETS",
    "ARIMA", "SARIMA", "Hybrid", "Bayesian Structural"
  ),
  labels = c(
    "Neural Network", "ETS",
    "ARIMA", "SARIMA", "Hybrid*", "Bayesian Structural"
  )
)
fig <- ggplot(
  data = DataClean,
  mapping = aes(
    y = Method,
    x = Index,
    fill = as.factor(Best)
  )
) +
  geom_col() +
  # reverse y axis
  scale_y_discrete(limits = rev(levels(DataClean$Method))) +
  scale_fill_manual(
    values = c("#00A087B2", "#DC0000B2"),
    labels = c("Alternative Models", "Best Model")
  ) +
  facet_wrap(. ~ disease) +
  theme_bw() +
  theme(
    legend.position = c(0.9, 0.1),
    axis.text = element_text(color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  labs(
    x = "Composite Normalized Index",
    y = NULL,
    fill = "Best Model"
  )

ggsave(
  filename = paste0("./outcome/publish/fig2.pdf"),
  fig,
  width = 12, height = 8, family = "Times New Roman",
  limitsize = FALSE, device = cairo_pdf
)
