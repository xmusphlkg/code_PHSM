
library(openxlsx)
library(tidyverse)

datafile_class <- read.xlsx("./outcome/appendix/Figure Data/Fig.1 data.xlsx",
                            sheet = "panel A"
) |>
     select(-c(value, label))

NID_report_China <- read.xlsx("./data/nation_and_provinces.xlsx", detectDates = T, sheet = "Nation") |>
     filter(disease_en %in% datafile_class$disease & date >= as.Date("2008-1-1")) |> 
     mutate(value = as.integer(value)) |> 
     filter(source != 'DataCenter')

# read all sheet
data_province_dc <- read.xlsx("./data/nation_and_provinces.xlsx", detectDates = T, sheet = "ProvinceCenter")
data_province_dc <- data_province_dc |> 
     select(year, month, disease_en, province, value, source, url) |>
     complete(year, month, disease_en, province, fill = list(value = 0, source = 'Complete', url = '')) |>
     mutate(date = make_date(year, month, 1)) |> 
     distinct()

# split data by province
for (i in unique(data_province_dc$province)) {
     assign(paste0("NID_onset_", i), filter(data_province_dc, province == i))
}


remove(datafile_class, data_province_dc, i)


save.image(file = "./ShinyWeb/data.RData")
