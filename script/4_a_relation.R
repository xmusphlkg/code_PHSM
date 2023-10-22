

# packages ----------------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(ggsci)
library(paletteer)
library(patchwork)
library(Cairo)
library(lubridate)
library(scales)
library(factoextra)
library(ggdendroplot)

remove(list = ls())

source('./script/theme_set.R')

# data --------------------------------------------------------------------

DataAll <- list.files(path = "./outcome/appendix/data/PHSMs/",     
                      pattern = "*.xlsx", 
                      full.names = TRUE)|>
     lapply(read.xlsx, detectDates = T) |> 
     bind_rows()

datafile_class <- read.xlsx('./data/disease_class.xlsx')
datafile_class$diseasename <- factor(datafile_class$diseasename, levels = datafile_class$diseasename)
datafile_class$class <- factor(datafile_class$class, levels = unique(datafile_class$class))

DataAll <- DataAll |> 
     left_join(datafile_class,
               by = c('disease_1' = 'diseaselist')) |> 
     mutate(
          RR = value/mean
     )

# plot --------------------------------------------------------------------

i <- 1

plot_rr <- function(i) {
     Class <- unique(datafile_class$class)[i]
     Data <- DataAll |> 
          filter(class == Class) |> 
          mutate(date = format(ymd(date), "%Y.%m"))
     fill_value <- fill_color[1:length(unique(Data$diseasename))]
     names(fill_value) <- unique(Data$diseasename)
     
     fig1 <- ggplot(data = Data)+
          geom_vline(xintercept = 1,
                     show.legend = F,
                     linetype = 'longdash')+
          geom_boxplot(mapping = aes(y = diseasename,
                                     x = RR,
                                     fill = class))+
          scale_y_discrete(limits = datafile_class$diseasename[datafile_class$class == Class])+
          scale_x_continuous(limits = c(0, 3), breaks = 0:3)+
          scale_fill_manual(values = fill_value[i])+
          theme_bw()+
          labs(x = NULL,
               y = NULL,
               title = paste0(LETTERS[2*i-1]),
               fill = NULL)
     fig2 <- ggplot(data = Data,
                    mapping = aes(fill = RR,
                                  x = date,
                                  y = diseasename))+
          geom_tile()+
          scale_fill_gradientn(colors = paletteer_d("awtools::a_palette"),
                               limits = c(0, 3))+
          scale_x_discrete(breaks = paste(c(2020, 2021, 2022, 2023), "01", sep = '.'),
                           labels = 2020:2023,
                           expand = expansion(add = c(0, 0)))+
          scale_y_discrete(limits = datafile_class$diseasename[datafile_class$class == Class],
                           expand = c(0, 0))+
          theme_bw()+
          theme(axis.text.y = element_blank(),
                legend.position = 'bottom')+
          guides(fill = guide_colourbar(barwidth = 20, barheight = 0.5, color = "black"))+
          labs(x = NULL,
               y = NULL,
               title = paste0(LETTERS[2*i]))
     fig1 + fig2 + plot_layout(widths = c(1, 3))
}

outcome <- lapply(1:4, plot_rr)

plot <- do.call(wrap_plots, c(outcome, ncol = 1, byrow = FALSE)) +
     plot_layout(guides = 'collect') &
     theme(legend.position = 'bottom')

# plot cluster ----------------------------------------------------------------

# cluster for RR
set.seed(20231021)

DataMat <- DataAll |> 
     select(value, date, diseasename) |> 
     pivot_wider(names_from = date,
                 values_from = value)
diseasename <- DataMat$diseasename
DataMat <- DataMat |> 
     select(-diseasename) |> 
     as.matrix()
rownames(DataMat) <- diseasename
# DataMat <- scale(DataMat)
hcdata <- hkmeans(DataMat, 2)
fig1 <- ggplot()+
     geom_dendro(hcdata$hclust,
                 dendrocut=max(hcdata[["hclust"]][["merge"]]),
                 groupCols = c(fill_color[2:1], 'grey'))+
     scale_y_continuous(trans = 'log10')+
     coord_flip()+
     theme_hm()+
     theme(axis.text.x = element_blank(),
           axis.text.y = element_text (colour = c(rep(fill_color[2], table(hcdata[["cluster"]])[1]),
                                                  rep(fill_color[1], table(hcdata[["cluster"]])[2]))),
           axis.ticks = element_blank(),
           axis.title = element_blank())+
     labs(title = 'I')

# cluster for report case
DataMat <- DataAll |> 
     select(RR, date, diseasename) |> 
     pivot_wider(names_from = date,
                 values_from = RR)
diseasename <- DataMat$diseasename
DataMat <- DataMat |> 
     select(-diseasename) |> 
     as.matrix()
rownames(DataMat) <- diseasename
# DataMat <- scale(DataMat)

hcdata <- hkmeans(DataMat, 2)
fig2 <- ggplot()+
     geom_dendro(hcdata$hclust,
                 dendrocut=max(hcdata[["hclust"]][["merge"]]),
                 groupCols = c(fill_color[3:4], 'grey'))+
     scale_y_continuous(trans = 'log10')+
     coord_flip()+
     theme_hm()+
     theme(axis.text.x = element_blank(),
           axis.text.y = element_text (colour = c(rep(fill_color[4], table(hcdata[["cluster"]])[2]),
                                                  rep(fill_color[3], table(hcdata[["cluster"]])[1]))),
           axis.ticks = element_blank(),
           axis.title = element_blank())+
     labs(title = 'J')
plot1 <- fig1 | fig2

ggsave('./outcome/publish/fig4.pdf',
       cowplot::plot_grid(plot, plot1, ncol = 1, rel_heights = c(2.5, 1)),
       family = "Times New Roman",
       limitsize = FALSE, device = cairo_pdf,
       width = 14, height = 16)
