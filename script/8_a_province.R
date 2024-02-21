# packages ----------------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(lubridate)
library(sf)

# data --------------------------------------------------------------------

datafile_class <- read.xlsx("./outcome/appendix/Figure Data/Fig.1 data.xlsx",
  sheet = "panel A"
) |>
  select(-c(value, label))

# all sheet in the file
data_nation <- read.xlsx("./data/nation_and_provinces.xlsx",
                               detectDates = T, sheet = "Nation"
) |>
     filter(date >= as.Date("2008-1-1")) |> 
     mutate(value = as.integer(value))
province_sheet <- getSheetNames("./data/nation_and_provinces.xlsx")[-c(1:4)]

# read all sheet
data_province <- lapply(province_sheet, function(x) {
  data <- read.xlsx("./data/nation_and_provinces.xlsx", sheet = x, detectDates = T)
  data$province <- x
  data
}) |>
  bind_rows() |> 
  filter(disease_en %in% datafile_class$disease & date >= as.Date("2008-1-1"))

# check data
datafile_check <- province_data |> 
  group_by(province, year, month) |> 
  count()
print(datafile_check[datafile_check$n != 24,],
      n = Inf)

# summary data
province_data |> 
     group_by(province) |> 
     summarise(date_min = min(date),
               date_max = max(date))

# plot --------------------------------------------------------------------

data_province <- data_province |> 
     left_join(data_nation[,c('date', 'disease_en', 'value')],
               by = c('date', 'disease_en'))
data_clean <- data_province |> 
     select(date, province, disease_en, value.x, value.y) |> 
     arrange(disease_en, date) |> 
     group_by(date, disease_en) |> 
     summarise(value.x = unique(value.y) - sum(value.x),
               .groups = 'drop') |> 
     mutate(province = 'Other')
data_clean <- data_province |> 
     select(date, disease_en, value.x, province) |> 
     rbind(data_clean) |> 
     mutate(month = month(date),
            year = month(date))






