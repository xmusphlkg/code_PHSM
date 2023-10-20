

# packages ----------------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(ggsci)
library(paletteer)
library(patchwork)
library(Cairo)

source('./script/theme_set.R')

# data --------------------------------------------------------------------

DataAll <- list.files(path = "./outcome/appendix/data/PHSMs/",     
                      pattern = "*.xlsx", 
                      full.names = TRUE)|>
     lapply(read.xlsx, detectDates = T) |> 
     bind_rows()

disease_list <- c('百日咳', '丙肝', '戊肝', '布病', '登革热', 
                  '肺结核', '风疹', '急性出血性结膜炎', '甲肝', 
                  '痢疾', '淋病', '流行性出血热', '艾滋病',
                  '流行性腮腺炎', '梅毒', '疟疾', '其它感染性腹泻病',
                  '伤寒+副伤寒', '乙肝', '手足口病', '猩红热',
                  '乙型脑炎', '包虫病', '斑疹伤寒')
disease_name <- c('Pertussis', 'HCV', 'HEV',
                  'Brucellosis', 'Dengue fever', 'Tuberculosis',
                  'Rubella', 'Acute hemorrhagic conjunctivitis', 'HAV',
                  'Dysentery', 'Gonorrhea', 'HFRS',
                  'AIDS', 'Mumps',
                  'Syphilis', 'Malaria', 'Other infectious diarrhea',
                  'Typhoid fever and paratyphoid fever', 'HBV', 'HFMD',
                  'Scarlet fever', 'Japanese encephalitis', 'Hydatidosis', 'Typhus')

datafile_class <- read.xlsx('./data/disease_class.xlsx') |> 
     left_join(data.frame(disease_list = disease_list,
                          disease_name = disease_name),
               by = c(diseasename = 'disease_name')) |> 
     mutate(diseasename = factor(diseasename,
                                  levels = c('HBV', 'HCV', 'Syphilis', 'AIDS', 'Gonorrhea',
                                             'HAV', 'HFMD', 'HEV', 'Other infectious diarrhea', 'Typhoid fever and paratyphoid fever', 'Acute hemorrhagic conjunctivitis', 'Dysentery',
                                             'Dengue fever', 'Brucellosis', 'Malaria', 'Japanese encephalitis', 'HFRS', 'Hydatidosis', 'Typhus',
                                             'Rubella', 'Mumps', 'Pertussis', 'Tuberculosis', 'Scarlet fever'))) |> 
     arrange(class, disease_name)

DataAll <- DataAll |> 
     left_join(datafile_class,
               by = c('disease_1' = 'disease_list')) |> 
     mutate(
          RR = value/mean
     )

# plot --------------------------------------------------------------------

i <- 1

plot_rr <- function(i) {
     Class <- unique(datafile_class$class)[i]
     Data <- DataAll |> 
          filter(class == Class)
     fill_value <- fill_color[1:length(unique(Data$diseasename))]
     names(fill_value) <- unique(Data$diseasename)
     
     fig1 <- ggplot(data = Data)+
          geom_vline(xintercept = 1,
                     show.legend = F,
                     linetype = 'longdash')+
          geom_boxplot(mapping = aes(y = diseasename,
                                     x = RR,
                                     fill = diseasename),
                       show.legend = F)+
          scale_y_discrete(limits = datafile_class$diseasename[datafile_class$class == Class])+
          scale_x_continuous(limits = c(0, 3), breaks = 0:3)+
          scale_fill_manual(values = fill_value)+
          theme_bw()+
          labs(x = "Relative Risk",
               y = NULL,
               title = paste0(LETTERS[2*i-1], ': ', Class))
     fig2 <- ggplot(data = Data,
                    mapping = aes(color = diseasename,
                                  fill = diseasename,
                                  x = date,
                                  y = RR))+
          geom_hline(yintercept = 1,
                     show.legend = F,
                     linetype = 'longdash')+
          geom_point(show.legend = F)+
          geom_smooth(show.legend = F)+
          scale_x_date(expand = expansion(add = c(0, 62)),
                       date_labels = '%Y',
                       breaks = seq(as.Date('2020/1/1'), max(Data$date)+62, by="1 years"))+
          scale_fill_manual(values = fill_value)+
          scale_color_manual(values = fill_value)+
          coord_cartesian(ylim = c(0, 3))+
          theme_bw()+
          labs(x = "Date",
               y = "Relative Risk",
               title = paste0(LETTERS[2*i], ': ', Class))
     fig1 + fig2 + plot_layout(widths = c(1, 3))
}

outcome <- lapply(1:4, plot_rr)

plot <- do.call(wrap_plots, c(outcome, ncol = 1, byrow = FALSE))

ggsave('./outcome/publish/fig4.pdf',
       plot,
       family = "Times New Roman",
       limitsize = FALSE, device = cairo_pdf,
       width = 14, height = 8)
