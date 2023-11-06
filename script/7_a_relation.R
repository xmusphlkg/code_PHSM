

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

Sys.setlocale(locale = 'en')
set.seed(202310)
source('./script/theme_set.R')

# data --------------------------------------------------------------------

split_date_1 <- as.Date("2022/11/15")
split_date_2 <- as.Date("2023/3/1")

DataAll <- list.files(path = "./outcome/appendix/data/Epidemic/",     
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
     ) |> 
     filter(date >= split_date_1)

# plot --------------------------------------------------------------------

i <- 1

plot_rr <- function(i) {
     Class <- unique(datafile_class$class)[i]
     Data <- DataAll |> 
          filter(class == Class) |> 
          mutate(date = format(ymd(date), "%b\n%Y"))
     fill_value <- fill_color[1:length(unique(Data$diseasename))]
     names(fill_value) <- unique(Data$diseasename)
     
     fig1 <- ggplot(data = Data,
                    mapping = aes(fill = RR,
                                  x = date,
                                  y = diseasename))+
          geom_tile()+
          scale_fill_gradientn(colors = paletteer_d("awtools::a_palette"),
                               limits = c(0, 3))+
          scale_x_discrete(expand = expansion(add = c(0, 0)))+
          scale_y_discrete(limits = datafile_class$diseasename[datafile_class$class == Class],
                           expand = c(0, 0))+
          theme_bw()+
          theme(legend.position = 'left')+
          guides(fill = guide_colourbar(barwidth = 1.5, barheight = 15, color = "black"))+
          labs(x = NULL,
               y = NULL,
               title = paste0(LETTERS[i]))
     fig1
}

outcome <- lapply(1:4, plot_rr)

plot <- do.call(wrap_plots, c(outcome, ncol = 2, byrow = T)) +
     plot_layout(guides = 'collect') &
     theme(legend.position = 'left')

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
DataMat <- scale(DataMat)
hcdata <- hkmeans(DataMat, 2)
fig1 <- fviz_dend(hcdata,
          cex = 0.6,
          palette = "jama", 
          rect = TRUE,
          rect_border = "jama",
          rect_fill = TRUE,
          main = 'E')+
     # scale_y_continuous(trans = 'log10')+
     theme(axis.text.x = element_blank(),
           axis.text.y = element_blank(),
           axis.ticks = element_blank(),
           axis.title = element_blank())

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
DataMat <- scale(DataMat)
hcdata <- hkmeans(DataMat, 2)
fig2 <- fviz_dend(hcdata,
                  cex = 0.6,
                  palette = "jama", 
                  rect = TRUE,
                  rect_border = "jama",
                  rect_fill = TRUE,
                  main = 'F')+
     # scale_y_continuous(trans = 'log10')+
     theme(axis.text.x = element_blank(),
           axis.text.y = element_blank(),
           axis.ticks = element_blank(),
           axis.title = element_blank())

plot1 <- fig1 | fig2

ggsave('./outcome/publish/fig7.pdf',
       cowplot::plot_grid(plot, plot1, ncol = 2, rel_widths = c(1.5, 2), nrow = 1),
       family = "Times New Roman",
       limitsize = FALSE, device = cairo_pdf,
       width = 18, height = 7)
