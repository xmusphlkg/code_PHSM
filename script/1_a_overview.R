
# packages ----------------------------------------------------------------

library(openxlsx)
library(tidyverse)
library(ggsci)
library(paletteer)
library(patchwork)

# data --------------------------------------------------------------------

source('./script/theme_set.R')

datafile_analysis <- read.xlsx('./data/nation_and_provinces.xlsx',
                               detectDates = T, sheet = 'Nation')
datafile_analysis$date <- as.Date(datafile_analysis$date)
datafile_class <- read.xlsx('./data/nation_and_provinces.xlsx', sheet = 'Class')

# left border
split_date_0 <- as.Date("2020/1/1")
split_date_1 <- as.Date("2020/4/1")
split_date_2 <- as.Date("2022/11/1")
split_date_3 <- as.Date("2023/4/1")

# bubble plot -------------------------------------------------------------

datafile_plot <- datafile_analysis |> 
     filter(disease_en %in% datafile_class$diseasename) |> 
     select(date, disease_en, value) |> 
     rename(c(disease = 'disease_en')) |> 
     mutate(disease = factor(disease,
                             levels = datafile_class$diseasename,
                             labels = datafile_class$diseasename),
            phase = case_when(date < split_date_1 ~ 'Pre-epidemic Periods',
                              date >= split_date_1 & date < split_date_2 ~ 'PHSMs Periods',
                              date >= split_date_2 & date < split_date_3 ~ 'Epidemic Periods',
                              date >= split_date_3 ~ 'Post-epidemic Period'),
            phase = factor(phase,
                           levels = c('Pre-epidemic Periods', 'PHSMs Periods', 'Epidemic Periods', 'Post-epidemic Period')),
            value = as.integer(value)) |> 
     left_join(datafile_class, by = c('disease' = 'diseasename')) |> 
     mutate(class = factor(class,
                           levels = c("Intestinal infectious diseases",
                                      "Blood borne and sexually transmitted diseases",
                                      "Respiratory infectious diseases",
                                      "Zoonotic infectious diseases")))

table(datafile_plot$disease)

# data check --------------------------------------------------------------

date_range <- seq.Date(min(datafile_analysis$date),
                       max(datafile_analysis$date),
                       by='month')
for (d in datafile_class$diseasename) {
     data <- datafile_plot |> 
          filter(disease == d)
     if(all(date_range %in% data$date)){
          
     }else{
          print(d)
     }
}

data_fig_A <- datafile_plot |> 
     group_by(disease, class, level) |> 
     summarise(value = sum(value),
               .groups = 'drop') |> 
     arrange(class, desc(value)) |> 
     mutate(label = formatC(value, format = "f", big.mark = ",", digits = 0))

# summary of NID ----------------------------------------------------------

## disease list
print('The disease list:')
print(table(datafile_plot$disease))

## total
print('The total number:')
print(sum(datafile_plot$value))

## disease number
print('The disease number:')
print(aggregate(value ~ disease, data = datafile_plot, sum))

## each group
datafile_plot |> 
     group_by(class, disease) |> 
     summarise(count = sum(value),
               .groups = 'drop') |> 
     mutate(percent = round(count/sum(count), 4)) |> 
     arrange(desc(count))

datafile_plot |> 
     group_by(class) |> 
     summarise(count = sum(value),
               .groups = 'drop') |> 
     mutate(percent = round(count/sum(count), 4)) |> 
     arrange(desc(count))

## natural focal disease in 2014.9-10
datafile_plot |> 
     filter(class == "Zoonotic infectious diseases" &
                 date %in% as.Date(c('2014-08-01', '2014-09-01', "2014-10-01", "2014-11-01"))) |> 
     ggplot()+
     geom_line(mapping = aes(x = date,
                             y = value,
                             color = disease))
datafile_plot |> 
     filter(disease == "Dengue fever" &
                 date <= as.Date('2014-12-01') &
                 date >= as.Date('2013-01-01')) |> 
     ggplot()+
     geom_line(mapping = aes(x = date,
                             y = value,
                             color = disease))

# background rect ---------------------------------------------------------

datafile_rect <- data.frame(
     start = c(min(datafile_plot$date), split_date_0, split_date_1, split_date_2, split_date_3),
     end = c(split_date_0, split_date_1, split_date_2, split_date_3, max(datafile_plot$date)),
     label = c('Pre-epidemic Period', 'PHSMs Period I', 'PHSMs Period II', 'Epidemic Period', 'Post-epidemic Period')
) |> 
     mutate(m = as.Date((as.numeric(start)+as.numeric(end))/2, origin = "1970-01-01"))

# lineplot ----------------------------------------------------------------

datafile_plot <- datafile_plot  |> 
     group_by(phase, date, class) |> 
     summarise(value = sum(value),
               .groups = 'drop') |>
     group_by(date) |> 
     mutate(percent = value/sum(value))

fig2 <- ggplot(data = datafile_plot)+
     geom_col(mapping = aes(x = date,
                            y = value,
                            fill = class),
              position = 'fill')+
     scale_fill_manual(values = fill_color)+
     scale_y_continuous(expand = c(0, 0),
                        labels = scales::percent)+
     scale_x_date(expand = expansion(add = c(15, 15)),
                  date_breaks = '1 years',
                  date_labels = '%Y')+
     theme_plot()+
     theme(legend.position = 'bottom')+
     labs(x = 'Date',
          y = 'Percentage of diseases',
          title = 'C',
          fill = NULL)

fig1 <- ggplot(data = datafile_plot)+
     geom_rect(data = datafile_rect, 
               aes(xmin = start, 
                   xmax = end,
                   fill = label), 
               ymin = -Inf, 
               ymax = Inf, 
               alpha = 0.2,
               show.legend = F)+
     geom_text(data = datafile_rect, 
               aes(x = m, 
                   y = 1e6,
                   label = label),
               vjust = 1,
               hjust = 0.5,
               show.legend = F)+
     geom_line(mapping = aes(x = date,
                             y = value,
                             color = class))+
     scale_color_manual(values = fill_color)+
     scale_fill_manual(values = back_color)+
     scale_y_continuous(expand = c(0, 0),
                        trans = 'log10',
                        label = scientific_10,
                        limits = c(1e3, 1e6),
                        breaks = c(1e3, 1e4, 1e5, 1e6))+
     scale_x_date(expand = expansion(add = c(15, 15)),
                  date_breaks = '1 years',
                  date_labels = '%Y')+
     theme_plot()+
     theme(legend.position = 'none')+
     labs(x = NULL,
          y = "Monthly incidence",
          color = NULL,
          title = 'B')

ggsave(filename = './outcome/publish/fig1_2.pdf',
       fig1 + fig2 + plot_layout(ncol = 1),
       width = 14,
       height = 8,
       device = cairo_pdf,
       family = "Times New Roman")

# figure data
data_fig <- list(
     'panel A' = data_fig_A,
     'panel B' = datafile_plot[,1:4],
     'panel C' = datafile_plot[,c(1:3, 5)]
)

write.xlsx(data_fig,
           file = './outcome/appendix/Figure Data/Fig.1 data.xlsx')

