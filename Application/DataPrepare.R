#####################################
## @Description: 
## @version: 
## @Author: Li Kangguo
## @Date: 2024-02-16 12:57:28
## @LastEditors: Li Kangguo
## @LastEditTime: 2024-02-27 13:51:56
#####################################

library(openxlsx)
library(tidyverse)

# Data prepare
## left border
split_date_0 <- as.Date("2020/1/1")
split_date_1 <- as.Date("2020/4/1")
split_date_2 <- as.Date("2022/11/1")
split_date_3 <- as.Date("2023/2/1")

## class data
# Data Structure
# disease name | class
DataClass <- read.xlsx("./data/disease_class.xlsx")
DataClass$diseasename <- factor(DataClass$diseasename, levels = DataClass$diseasename)
DataClass$class <- factor(DataClass$class, levels = unique(DataClass$class))

## model data
DataModel <- read.xlsx("./data/pre-epidemic.xlsx")
DataModel$Index <- rep(c("RMSE", "R-squared", "MAE"), times = nrow(DataModel) / 3)
DataModel$diseasename <- factor(DataModel$disease, levels = DataClass$diseasename)
DataModel <- DataModel[,-5]

## forecast data
DataForecast <- list.files(
     path = "./data/forecast/",
     pattern = "*.xlsx",
     full.names = TRUE
) |>
     lapply(read.xlsx, detectDates = T) |>
     bind_rows() |> 
     left_join(DataClass,
               by = c(disease_1 = 'diseaselist')) |> 
     select(-disease_1)

## actual data
DataActual <- read.xlsx("./data/Nation.xlsx",
                        sheet = "Sheet 1",
                        detectDates = T) |> 
     filter(date >= as.Date("2008/1/1")) |> 
     left_join(DataClass,
               by = c(disease = 'diseaselist')) |> 
     select(-disease) |> 
     drop_na()

save.image()


