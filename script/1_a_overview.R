
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

# left border
split_date_0 <- as.Date("2020/1/1")
split_date_1 <- as.Date("2020/4/1")
split_date_2 <- as.Date("2022/11/1")
split_date_3 <- as.Date("2023/4/1")

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
                                      "Respiratory infectious diseases",
                                      "Natural focal diseases")))
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
          './outcome/appendix/data/fig1_1.csv',
          quote = F,
          row.names = F)

# summary of NID ----------------------------------------------------------

## disease list
unique(datafile_plot$disease)

## total
sum(datafile_plot$value)

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
     filter(class == "Natural focal disease" &
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
19178/1480 - 1
23525/1665 - 1


# background rect ---------------------------------------------------------

datafile_rect <- data.frame(
     start = c(as.Date('2008/1/1'), split_date_0, split_date_1, split_date_2),
     end = c(split_date_0, split_date_1, split_date_2, split_date_3),
     label = c('Pre-epidemic Period', 'PHSMs Period I', 'PHSMs Period II', 'Epidemic Period')
) |> 
     mutate(m = as.Date((as.numeric(start)+as.numeric(end))/2, origin = "1970-01-01"))

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
     scale_fill_manual(values = c("#05215D50", "#E6383350", "#5E954650", "#3381A850"))+
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

ggsave(filename = './outcome/publish/fig1_2.pdf',
       width = 14,
       height = 8,
       device = cairo_pdf,
       family = "Times New Roman")

ggsave(filename = './outcome/publish/fig1_2.png',
       width = 14,
       height = 8)
