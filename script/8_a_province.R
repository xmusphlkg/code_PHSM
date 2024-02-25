# packages ----------------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(lubridate)
library(patchwork)
library(sf)

source('./script/theme_set.R')

# data --------------------------------------------------------------------

datafile_class <- read.xlsx("./outcome/appendix/Figure Data/Fig.1 data.xlsx",
  sheet = "panel A"
) |>
  select(-c(value, label))

# all sheet in the file
data_nation <- read.xlsx("./data/nation_and_provinces.xlsx",
                               detectDates = T, sheet = "Nation") |>
     filter(disease_en %in% datafile_class$disease & date >= as.Date("2008-1-1")) |> 
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
datafile_check <- data_province |> 
  group_by(province, year, month) |> 
  count()
print(datafile_check[datafile_check$n != 24,],
      n = Inf)
datafile_check <- data_province |> 
     group_by(province) |> 
     summarise(date_min = min(date),
               date_max = max(date),
               count = n())
write.xlsx(datafile_check, "./outcome/appendix/Table S3.xlsx")

# summary data
data_province |> 
     group_by(province) |> 
     summarise(date_min = min(date),
               date_max = max(date),
               count = n())

# read sf file
data_map <- st_read("./data/map_data/province.shp")
ggplot(data = data_map)+
     geom_sf()+
     theme_minimal()+
     theme(legend.position = "none")

# plot --------------------------------------------------------------------

data_province <- data_province |> 
     left_join(data_nation[,c('date', 'disease_en', 'value')],
               by = c('date', 'disease_en'))
data_other <- data_province |> 
     select(date, province, disease_en, value.x, value.y) |> 
     arrange(disease_en, date) |> 
     group_by(date, disease_en) |> 
     summarise(value.x = unique(value.y) - sum(value.x),
               .groups = 'drop') |> 
     mutate(province = 'Other')
data_province <- data_province |> 
     select(date, disease_en, value.x, province) |> 
     rbind(data_other) |> 
     mutate(month = month(date),
            year = year(date),
            disease_en = factor(disease_en,
                                levels = datafile_class$disease),
            province = factor(province,
                              levels = c('Other', province_sheet))) |> 
     group_by(province, disease_en) |> 
     rename(value = value.x) |>
     mutate(value_norm = (value - mean(value, na.rm = T)) / sd(value, na.rm = T))

data_nation <- data_nation |> 
     mutate(month = month(date),
            year = year(date),
            disease_en = factor(disease_en,
                                levels = datafile_class$disease)) |> 
     group_by(disease_en) |>
     mutate(value_norm = (value - mean(value, na.rm = T)) / sd(value, na.rm = T))

## setting fill color
fill_color_province <- c('grey', brewer.pal(length(province_sheet), 'Spectral'))
names(fill_color_province) <- c('Other', province_sheet)
years <- 2008:2023

for (disease in datafile_class$disease) {
     # epidemic curve
     data <- filter(data_province, disease_en == disease)
     fig_c <- ggplot(data = data)+
          geom_col(mapping = aes(x = date,
                                 y = value,
                                 fill = province),
                   show.legend = T)+
          scale_y_continuous(expand = expansion(mult = c(0, 0.1)),
                             label = scientific_10)+
          scale_x_date(expand = expansion(add = c(15, 15)),
                       date_breaks = '1 years',
                       date_labels = '%Y')+
          scale_fill_manual(values = fill_color_province)+
          theme_bw() +
          theme(legend.position = "right",
                panel.grid = element_blank(),
                axis.text = element_text(size = 10.5, color = "black"),
                axis.title = element_text(size = 10.5, color = "black", face = "bold"),
                legend.justification = c(0, -0.2)) +
          labs(x = 'Date',
               y = 'Monthly incidence',
               fill = 'Province',
               title = 'C')
     
     # map for province
     # repeat the data
     data <- data |> 
          group_by(year, province) |>
          summarise(value = sum(value),
                    .groups = 'drop') |> 
          filter(province != "Other")
     plot_breaks <- pretty(data$value)
     
     data_maps <- lapply(years, function(x) {
          data_map |> 
               left_join(filter(data, year == x),
                         by = c('Yname' = 'province')) |> 
               mutate(year = x)
     }) |> 
          bind_rows()
     
     fig_a <- ggplot(data = data_maps)+
          geom_sf(mapping = aes(fill = value))+
          theme_bw()+
          theme(legend.position = "right",
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
                               na.value = 'grey')+
          facet_wrap(~year, nrow = 4)+
          guides(fill = guide_colourbar(barwidth = 1, barheight = 30, color = "black")) +
          labs(fill = 'Yearly\nincidence',
               title = 'A')
     
     data <- data_province |> 
          filter(disease_en == disease) |> 
          group_by(province) |>
          summarise(value = sum(value))
     fig_b <- ggplot(data = data)+
          geom_col(mapping = aes(x = 1,
                                 y = value,
                                 fill = province),
                   width = 1,
                   position = 'fill',
                   show.legend = F)+
          scale_x_continuous(expand = c(0, 0),
                             limits = c(0.5, 1.5))+
          scale_y_continuous(expand = c(0, 0),
                             labels = scales::percent)+
          coord_flip(ylim = c(0, 1))+
          scale_fill_manual(values = fill_color_province,
                            limits = rev(c('Other', province_sheet)))+
          theme_bw()+
          theme(axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.title = element_text(size = 10.5, color = "black", face = "bold"),
                panel.grid = element_blank())+
          labs(x = NULL,
               y = NULL,
               fill = 'Province',
               title = 'B')
     
     # heatmap for normal value
     data <- data_nation |> 
          filter(disease_en == disease) |> 
          select(date, disease_en, value, month, year, value_norm) |>
          mutate(province = 'Nation') |> 
          rbind(filter(data_province, disease_en == disease)) |> 
          mutate(date = format(ymd(date), "%Y.%m"),
                 out_label = if_else(value_norm > 10, '*', ''))
     
     fig_d <- ggplot(data = data,
                     mapping = aes(fill = value_norm,
                                   x = date,
                                   y = province)) +
          geom_tile() +
          geom_text(mapping = aes(label = out_label),
                    vjust = 0.5)+
          scale_fill_gradientn(colors = paletteer_d("awtools::a_palette"),
                               trans = log_fill,
                               limits = c(-5, 10)) +
          scale_x_discrete(breaks = paste(seq(2008, 2023), "01", sep = "."),
                           labels = 2008:2023,
                           expand = expansion(add = c(0, 0))) +
          scale_y_discrete(limits = c('Other', province_sheet, 'Nation'),
                           expand = c(0, 0.5)) +
          theme_bw() +
          theme(legend.position = "right",
                panel.grid = element_blank(),
                legend.justification = c(0, 0),
                axis.text = element_text(size = 10.5, color = "black"),
                axis.title = element_text(size = 10.5, color = "black", face = "bold")) +
          guides(fill = guide_colourbar(barwidth = 1, barheight = 10, color = "black")) +
          labs(x = "Date",
               y = NULL,
               title = 'D',
               fill = "Normalized\nmonthly\nincidence")
     
     fig <- fig_a + fig_b + fig_c + fig_d + 
          plot_layout(ncol = 1, heights = c(3, 0.1, 1, 1))&
          theme(axis.title.y = element_text(vjust = -5))
     
     ggsave(filename = paste0("./outcome/appendix/Supplementary_1/", disease, ".png"),
            fig,
            device = 'png',
            width = 14, height = 15,
            limitsize = FALSE,
            dpi = 300)
     
     remove(fig, fig_a, fig_b, fig_c, fig_d, data, data_maps, plot_breaks)
}
