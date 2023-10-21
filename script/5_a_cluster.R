#####################################
## @Description:
## @version:
## @Author: Li Kangguo
## @Date: 2023-10-21 14:52:14
## @LastEditors: Li Kangguo
## @LastEditTime: 2023-10-21 15:08:20
#####################################

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

source("./script/theme_set.R")

# data --------------------------------------------------------------------

DataAll <- list.files(
  path = "./outcome/appendix/data/PHSMs/",
  pattern = "*.xlsx",
  full.names = TRUE
) |>
  lapply(read.xlsx, detectDates = T) |>
  bind_rows()

disease_list <- c(
  "百日咳", "丙肝", "戊肝", "布病", "登革热",
  "肺结核", "风疹", "急性出血性结膜炎", "甲肝",
  "痢疾", "淋病", "流行性出血热", "艾滋病",
  "流行性腮腺炎", "梅毒", "疟疾", "其它感染性腹泻病",
  "伤寒+副伤寒", "乙肝", "手足口病", "猩红热",
  "乙型脑炎", "包虫病", "斑疹伤寒"
)
disease_name <- c(
  "Pertussis", "HCV", "HEV",
  "Brucellosis", "Dengue fever", "Tuberculosis",
  "Rubella", "Acute hemorrhagic conjunctivitis", "HAV",
  "Dysentery", "Gonorrhea", "HFRS",
  "AIDS", "Mumps",
  "Syphilis", "Malaria", "Other infectious diarrhea",
  "Typhoid fever and paratyphoid fever", "HBV", "HFMD",
  "Scarlet fever", "Japanese encephalitis", "Hydatidosis", "Typhus"
)

datafile_class <- read.xlsx("./data/disease_class.xlsx") |>
  left_join(
    data.frame(
      disease_list = disease_list,
      disease_name = disease_name
    ),
    by = c(diseasename = "disease_name")
  )

DataAll <- DataAll |>
  left_join(datafile_class,
    by = c("disease_1" = "disease_list")
  ) |>
  mutate(
    RR = value / mean
  )

# cross-correlation analysis ------------------------------------------------------

perform_cross_correlation <- function(data) {
     ccf_result <- ccf(data$index, data$diff)
     max_correlation <- max(ccf_result$acf)
     lag_time <- ccf_result$lag[which.max(ccf_result$acf)]
     return(data.frame(max_correlation = max_correlation, lag_time = lag_time))
}

DataSI <- read.csv('./data/owid-covid-data.csv') |> 
     filter(iso_code == "CHN")

DataSIm <- DataSI |> 
     select(date, stringency_index) |> 
     mutate(year = year(date),
            month = month(date)) |> 
     group_by(year, month) |> 
     summarise(index = mean(stringency_index),
               .groups = 'drop') |> 
     mutate(date = as.Date(paste(year, month, 01, sep = '-'))) |> 
     filter(date <= max(DataAll$date))

DataAll <- DataAll |> 
     left_join(DataSIm, by = 'date')

cross_correlation_results <- DataAll |> 
     group_by(diseasename) %>%
     summarize(correlation_results = list(perform_cross_correlation(.))) %>%
     unnest(correlation_results)

summary_table <- cross_correlation_results %>%
     pivot_wider(names_from = lag_time, values_from = max_correlation, names_prefix = "lag") %>%
     select(diseasename, starts_with("lag"))

ggsave("./outcome/appendix/figure/PHSMs_cluster",
       plot,
       family = "Times New Roman",
       limitsize = FALSE, device = cairo_pdf,
       width = 14, height = 8
)

