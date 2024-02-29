# packages ----------------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(lubridate)
library(patchwork)
library(sf)
library(doParallel)

source('./script/theme_set.R')

# data --------------------------------------------------------------------

datafile_class <- read.xlsx("./outcome/appendix/Figure Data/Fig.1 data.xlsx",
  sheet = "panel A"
) |>
  select(-c(value, label))

# all sheet in the file
data_nation <- read.xlsx("./data/nation_and_provinces.xlsx", detectDates = T, sheet = "Nation") |>
     filter(disease_en %in% datafile_class$disease & date >= as.Date("2008-1-1")) |> 
     mutate(value = as.integer(value))

# read all sheet
data_province_dc <- read.xlsx("./data/nation_and_provinces.xlsx", detectDates = T, sheet = "ProvinceCenter")
data_province_dc <- data_province_dc |> 
  select(year, month, disease_en, province, value) |>
  complete(year, month, disease_en, province, fill = list(value = 0)) |>
  mutate(date = make_date(year, month, 1)) |> 
  distinct()

data_province_re <- read.xlsx("./data/nation_and_provinces.xlsx", detectDates = T, sheet = "ProvinceReport")
data_province_re <- data_province_re |> 
  select(year, month, disease_en, province, value) |>
  complete(year, month, disease_en, province, fill = list(value = NA)) |>
  mutate(date = make_date(year, month, 1))

data_province <- rbind(data_province_dc, data_province_re) |> 
     filter(disease_en %in% datafile_class$disease & date >= as.Date("2008-1-1")) |> 
     select(date, year, month, disease_en, value, province)

# add column to find data is duplicated
datafile_check <- data_province |> 
     group_by(date, disease_en, province) |> 
     summarise(count = n(),
               value = list(value),
               equal = n_distinct(value) == 1,
               .groups = 'drop') |> 
     filter(count > 1)
datafile_check <- data_province |> 
     group_by(province) |> 
     summarise(date_min = min(date),
               date_max = max(date),
               date_num = n_distinct(date),
               date_miss = paste(format.Date(seq.Date(min(date), max(date), by = '1 month')[!seq.Date(min(date), max(date), by = '1 month') %in% unique(date)],
                                '%Y-%m'),
                                collapse = ' '),
               count = n(),
               .groups = 'drop')

# clean data
data_nation <- data_nation |> 
     mutate(month = month(date),
            year = year(date),
            province = 'Nation',
            date = make_date(year, month, 1),
            disease_en = factor(disease_en,
                                levels = datafile_class$disease)) |> 
     select(year, month, disease_en, province, value, date)

data_all <- bind_rows(data_province, data_nation) |> 
     group_by(disease_en, province) |>
     mutate(value_norm = (value - mean(value, na.rm = T)) / sd(value, na.rm = T),
            date = format(ymd(date), "%Y.%m"),
            out_label = if_else(value_norm > 10, '*', ''))
province_sheet <- unique(data_province_dc$province)
province_sheet <- province_sheet[province_sheet != 'Total']

# read sf file
data_map <- st_read("./data/map_data/province.shp")
ggplot(data = data_map)+
     geom_sf()+
     theme_minimal()+
     theme(legend.position = "none")

data_year <- data_all |> 
     group_by(year, disease_en, province) |>
     summarise(value = sum(value, na.rm = T),
               .groups = 'drop') |> 
     filter(province != "Total") |> 
     pivot_wider(names_from = province, values_from = value)
write.xlsx(data_year, "./outcome/appendix/Supplementary Appendix 2_2.xlsx")

# plot --------------------------------------------------------------------

years <- 2008:2023

auto_plot_function <- function(disease) {
     # map for province
     data <- data_all |> 
          filter(disease_en == disease) |> 
          group_by(year, province) |>
          summarise(value = mean(value, na.rm = T),
                    .groups = 'drop') |> 
          filter(province != "Total")
     
     data_maps <- lapply(years, function(x) {
          data_map |> 
               left_join(filter(data, year == x),
                         by = c('Yname' = 'province')) |> 
               mutate(year = x)
     }) |> 
          bind_rows()
     
     plot_breaks <- pretty(data_maps$value)
     
     fig_a <- ggplot(data = data_maps)+
          geom_sf(mapping = aes(fill = value))+
          theme_bw()+
          theme(legend.position = "right",
                plot.title.position = 'plot',
                axis.text = element_blank(),
                axis.title = element_text(size = 10.5, color = "black", face = "bold"),
                axis.ticks = element_blank(),
                strip.text = element_text(size = 9, color = "black", hjust = 0),
                strip.background = element_blank(),
                legend.justification = c(0, 0.5),
                legend.text = element_text(hjust = 0))+
          scale_fill_gradientn(colors = paletteer_d("awtools::a_palette"),
                               breaks = plot_breaks,
                               limits = range(plot_breaks),
                               labels = scientific_10,
                               na.value = 'white')+
          facet_wrap(~year, nrow = 4)+
          guides(fill = guide_colourbar(barwidth = 1, barheight = 40, color = "black")) +
          labs(fill = 'Average\nmonthly\nincidence',
               title = 'A')
     
     data <- data_all |> 
          filter(disease_en == disease)
     fig_b <- ggplot(data = data,
                     mapping = aes(fill = value_norm,
                                   x = date,
                                   y = province)) +
          geom_tile() +
          geom_text(mapping = aes(label = out_label),
                    vjust = 0.5)+
          scale_fill_gradientn(colors = paletteer_d("awtools::a_palette"),
                               trans = log_fill,
                               limits = c(-6, 10),
                               na.value = 'white') +
          scale_x_discrete(breaks = paste(seq(2008, 2023), "01", sep = "."),
                           labels = 2008:2023,
                           expand = expansion(add = c(0, 0))) +
          scale_y_discrete(limits = c(province_sheet, 'Nation'),
                           expand = c(0, 0.5)) +
          theme_bw() +
          theme(legend.position = "right",
                panel.grid = element_blank(),
                plot.title.position = 'plot',
                legend.justification = c(0, 0.5),
                axis.text = element_text(size = 10.5, color = "black"),
                axis.title = element_text(size = 10.5, color = "black", face = "bold")) +
          guides(fill = guide_colourbar(barwidth = 1, barheight = 20, color = "black")) +
          labs(x = "Date",
               y = NULL,
               title = 'B',
               fill = "Normalized\nmonthly\nincidence")
     
     fig <- fig_a + fig_b + 
          plot_layout(ncol = 1, heights = c(1.7, 1))
     
     ggsave(filename = paste0("./outcome/appendix/Supplementary Appendix 1_1/", disease, ".png"),
            fig,
            device = 'png',
            width = 14, height = 16,
            limitsize = FALSE,
            dpi = 300)
     
     write.csv(data, paste0("./outcome/appendix/Application/", disease, ".csv"), row.names = F)
     
     remove(fig, fig_a, fig_b, fig_c, fig_d, data, data_maps, plot_breaks)
}


cl <- makeCluster(length(province_sheet))
registerDoParallel(cl)
clusterEvalQ(cl, {
  library(tidyverse)
  library(openxlsx)
  library(lubridate)
  library(patchwork)
  library(sf)
  library(paletteer)
  
  Sys.setlocale(locale = "en")
})

clusterExport(cl, ls()[ls() != "cl"], envir = environment())
outcome <- parLapply(cl, datafile_class$disease, auto_plot_function)
stopCluster(cl)

