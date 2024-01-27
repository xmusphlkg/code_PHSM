
library(tidyverse)
library(patchwork)
library(openxlsx)

# read data ---------------------------------------------------------------

file_list <- paste0('./outcome/appendix/data/forecast/',
                    datafile_class$disease,
                    '.xlsx')
data_list <- lapply(file_list, read.xlsx, detectDates = T)
names(data_list) <- paste0(LETTERS[1:24], ' ', datafile_class$disease)

data_list <- do.call('rbind', data_list) |> 
     left_join(datafile_class, by = c(disease_en = "disease")) |> 
     mutate(disease_en = factor(disease_en,
                                levels = datafile_class$disease))
group_lists <- unique(datafile_class$class)
data_scale <- data_list |> 
     group_by(date, class) |> 
     summarise(diff = sum(diff),
               .groups = 'drop') |> 
     group_by(class) |> 
     summarise(diff = max(diff),
               .groups = 'drop')

# plot --------------------------------------------------------------------

for (i in 1:4) {
     data_single_group <- data_list |> 
          filter(class == group_lists[i])
     
     fig <- ggplot(data = data_single_group)+
          geom_col(mapping = aes(x = date,
                                 y = diff,
                                 fill = disease_en))+
          geom_hline(yintercept = 0)+
          theme_set()+
          scale_x_date(
               expand = expansion(add = c(15, 15)),
               date_breaks = "1 year",
               date_labels = "%Y"
          ) +
          scale_y_continuous(
               limits = c(-max(data_scale$diff), max(data_scale$diff)),
               expand = expansion(mult = c(0.15, 0.15)),
               labels = scientific_10
          ) +
          scale_fill_manual(values = fill_color_disease)+
          theme(legend.position = c(0.01, 0.01),
                legend.justification = c(0, 0))+
          labs(x = 'Date',
               y = 'Difference',
               fill = NULL,
               title = LETTERS[i])+
          guides(fill = guide_legend(ncol = 3,
                                     byrow = T,
                                     title = NULL))
     
     
     assign(paste0('fig', i), fig)
}

plot <- fig1 + fig2 + fig3 + fig4

ggsave('./outcome/publish/fig5.pdf',
       plot,
       family = "Times New Roman",
       limitsize = FALSE, device = cairo_pdf,
       width = 12, height = 7)

write.xlsx(data_list,
           file = './outcome/appendix/data/Fig.5 data.xlsx')
