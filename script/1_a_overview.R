
# packages ----------------------------------------------------------------

library(openxlsx)
library(tidyverse)
library(ggsci)
library(paletteer)
library(patchwork)

# data --------------------------------------------------------------------

source('./script/theme_set.R')

scientific_10 <- function(x) {
     parse(text = gsub("[+]", "", gsub("1e", "10^", scales::scientific_format()(x))))
}

datafile_analysis <- read.xlsx('./data/Nation.xlsx', detectDates = T) |> 
     filter(date >= as.Date('2008-1-1'))
datafile_class <- read.xlsx("./data/disease_class.xlsx", detectDates = T)


split_date_1 <- as.Date("2019/12/15")
split_date_2 <- as.Date("2022/11/15")
split_date_3 <- as.Date("2023/3/15")

# bubble plot -------------------------------------------------------------

datafile_plot <- datafile_analysis |> 
     filter(disease_1 %in% datafile_class$diseaselist) |> 
     select(date, disease_1, value) |> 
     mutate(disease = factor(disease_1,
                             levels = datafile_class$diseaselist,
                             labels = datafile_class$diseasename),
            phase = case_when(date < split_date_1 ~ 'Pre-epidemic Periods',
                              date > split_date_1 & date < split_date_2 ~ 'PHSMs Periods',
                              date > split_date_2 ~ 'Epidemic Periods',),
            phase = factor(phase,
                           levels = c('Pre-epidemic Periods', 'PHSMs Periods', 'Epidemic Periods'))) |> 
     left_join(datafile_class, by = c('disease' = 'diseasename')) |> 
     mutate(class = factor(class,
                           levels = c("Blood borne and sexually transmitted diseases",
                                      "Intestinal infectious diseases",
                                      "Respiratory infectious disease",
                                      "Natural focal disease")))

datafile_bubble <- datafile_plot |> 
     group_by(disease, class, level) |> 
     summarise(value = sum(value),
               .groups = 'drop')
datafile_legend <- data.frame(
     disease = LETTERS[1:4],
     class = 'legend',
     level = 'A',
     value = c(2e4, 2e5, 2e6, 2.6e7)
)

write.csv(rbind(datafile_bubble, datafile_legend),
          './outcome/publish/fig1.csv',
          quote = F,
          row.names = F)

# lineplot ----------------------------------------------------------------

datafile_plot <- datafile_plot  |> 
     group_by(phase, date, class) |> 
     summarise(value = sum(value),
               .groups = 'drop') |> 
     mutate(class = factor(class,
                           levels = unique(datafile_class$class)))

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
     geom_rect(data = data.frame(start_date = split_date_2,
                                 end_date = split_date_3), 
               aes(xmin = start_date, 
                   xmax = end_date), 
               ymin = -Inf, 
               ymax = Inf, 
               fill = "#E9E29CFF",
               alpha = 0.2,
               show.legend = F)+
     annotate('text',
              x = median(c(split_date_2, split_date_3)),
              y = 9e5,
              label = "Epidemic\nPeriods",
              family = "Times New Roman",
              vjust = 1,
              hjust = 0.5)+
     geom_rect(data = data.frame(start_date = c(split_date_1),
                                 end_date = c(split_date_2)), 
               aes(xmin = start_date, 
                   xmax = end_date), 
               ymin = -Inf, 
               ymax = Inf, 
               fill = "#EEB479FF",
               alpha = 0.2,
               show.legend = F)+
     annotate('text',
              x = median(c(split_date_1, split_date_2)),
              y = 9e5,
              label = "PHSMs Periods",
              family = "Times New Roman",
              vjust = 1,
              hjust = 0.5)+
     geom_rect(data = data.frame(start_date = c(min(datafile_plot$date)),
                                 end_date = c(split_date_1)), 
               aes(xmin = start_date, 
                   xmax = end_date), 
               ymin = -Inf, 
               ymax = Inf, 
               fill = "#91D1C2FF",
               alpha = 0.2,
               show.legend = F)+
     annotate('text',
              x = median(c(split_date_1, min(datafile_plot$date))),
              y = 9e5,
              label = "Pre-epidemic Periods",
              family = "Times New Roman",
              vjust = 1,
              hjust = 0.5)+
     geom_line(mapping = aes(x = date,
                             y = value,
                             color = class))+
     scale_color_manual(values = fill_color)+
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
fig1

fig1 + fig2 + plot_layout(ncol = 1)

ggsave(filename = './outcome/publish/fig1.pdf',
       width = 14,
       height = 8,
       device = cairo_pdf,
       family = "Times New Roman")

ggsave(filename = './outcome/publish/fig1.png',
       width = 14,
       height = 8)
