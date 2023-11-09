# packages ----------------------------------------------------------------

library(openxlsx)
library(tidyverse)
library(ggsci)
library(paletteer)
library(patchwork)
library(tseries)

# data --------------------------------------------------------------------

source("./script/theme_set.R")

scientific_10 <- function(x) {
     ifelse(x == 0, 0, parse(text = gsub("[+]", "", gsub("e", "%*%10^", scales::scientific_format()(x)))))
}

library(scales)
log_fill <- trans_new(
     name = "log_fill",
     transform = function(x) sign(x) * log1p(abs(x)),
     inverse = function(x) sign(x) * (exp(abs(x)) - 1)
)

datafile_analysis <- read.xlsx("./data/Nation.xlsx", detectDates = T) |>
     filter(date >= as.Date("2008-1-1"))

datafile_class <- read.xlsx("./data/disease_class.xlsx", detectDates = T)


# left border
split_date_0 <- as.Date("2020/1/1")
split_date_1 <- as.Date("2020/4/1")
split_date_2 <- as.Date("2022/11/1")
split_date_3 <- as.Date("2023/4/1")


# background rect ---------------------------------------------------------

datafile_rect <- data.frame(
     start = c(as.Date('2008/1/1'), split_date_0, split_date_1, split_date_2),
     end = c(split_date_0, split_date_1, split_date_2, split_date_3),
     label = c('Pre-epidemic Period', 'PHSMs Period I', 'PHSMs Period II', 'Epidemic Period')
) |> 
     mutate(m = as.Date((as.numeric(start)+as.numeric(end))/2, origin = "1970-01-01"))

# group plot -------------------------------------------------------------

datafile_plot <- datafile_analysis |>
     filter(disease_1 %in% datafile_class$diseaselist) |>
     select(date, disease_1, value) |> 
     complete(date = seq.Date(min(datafile_analysis$date), max(datafile_analysis$date), by = "month"),
              disease_1 = unique(datafile_class$diseaselist),
              fill = list(value = 0)) |> 
     mutate(year = year(date)) |>
     mutate(
          disease = factor(disease_1,
                           levels = datafile_class$diseaselist,
                           labels = datafile_class$diseasename
          ),
          phase = case_when(
               date < split_date_1 ~ "Pre-epidemic Periods",
               date > split_date_1 & date < split_date_2 ~ "PHSMs Periods",
               date > split_date_2 ~ "Epidemic Periods",
          ),
          phase = factor(phase,
                         levels = c("Pre-epidemic Periods", "PHSMs Periods", "Epidemic Periods")
          )
     ) |>
     left_join(datafile_class, by = c("disease" = "diseasename")) |>
     mutate(class = factor(class,
                           levels = c(
                                "Blood borne and sexually transmitted diseases",
                                "Intestinal infectious diseases",
                                "Respiratory infectious disease",
                                "Natural focal disease"
                           )
     ))

datafile_group <- datafile_plot |>
     group_by(phase, date, class) |>
     summarise(
          value = sum(value),
          .groups = "drop"
     ) |>
     mutate(class = factor(class,
                           levels = unique(datafile_class$class)
     ))

# complete missing data
table(datafile_plot[, "disease"])

# lineplot ----------------------------------------------------------------

group_lists <- levels(datafile_group$class)

plot_single <- function(i) {
     group_list <- group_lists[i]
     datafile_plot_single <- datafile_plot |>
          filter(class == group_list) |>
          group_by(disease) |>
          mutate(
               value_norm = (value - mean(value, na.rm = T)) / sd(value, na.rm = T),
               date = format(ymd(date), "%Y.%m")
          )
     datafile_group_single <- datafile_group |>
          filter(class == group_list)
     
     fig1 <- ggplot(data = datafile_group_single) +
          geom_rect(data = datafile_rect, 
                    aes(xmin = start, 
                        xmax = end,
                        fill = label), 
                    ymin = -Inf, 
                    ymax = Inf, 
                    alpha = 0.2,
                    show.legend = F)+
          geom_line(
               mapping = aes(
                    x = date,
                    y = value
               ),
               color = fill_color[i]
          ) +
          scale_x_date(
               expand = expansion(add = c(15, 15)),
               date_breaks = "1 year",
               date_labels = "%Y"
          ) +
          scale_y_continuous(
               expand = expansion(mult = c(0.15, 0.25)),
               breaks = pretty(datafile_group_single$value),
               labels = scientific_10
          ) +
          scale_fill_manual(values = c("#05215D50", "#E6383350", "#5E954650", "#3381A850"))+
          theme_bw() +
          theme(
               legend.position = "none",
               axis.text.x = element_blank(),
               panel.grid.major.y = element_blank(),
               panel.grid.minor.y = element_blank()
          ) +
          labs(
               x = NULL,
               y = "Monthly incidence",
               color = NULL,
               title = LETTERS[i]
          )
     
     fig2 <- ggplot(
          data = datafile_plot_single,
          mapping = aes(
               fill = value_norm,
               x = date,
               y = disease
          )
     ) +
          geom_tile() +
          coord_equal(3) +
          scale_fill_gradientn(
               colors = paletteer_d("awtools::a_palette"),
               trans = log_fill,
               limits = c(-5, 10),
               na.value = "black"
          ) +
          scale_x_discrete(
               breaks = paste(seq(2008, 2023), "01", sep = "."),
               labels = 2008:2023,
               expand = expansion(add = c(0, 0))
          ) +
          scale_y_discrete(
               limits = datafile_class$diseasename[datafile_class$class == group_list],
               expand = c(0, 0)
          ) +
          theme_bw() +
          theme(
               legend.position = "bottom",
               panel.grid = element_blank()
          ) +
          guides(fill = guide_colourbar(barwidth = 20, barheight = 0.5, color = "black")) +
          labs(
               x = NULL,
               y = NULL,
               fill = "Monthly Incidence (Normalize)"
          )
     
     return(fig1 + fig2 + plot_layout(ncol = 1, heights = c(1, 1)))
}

## create figure panel for all class
plot_list <- lapply(1:length(group_lists), plot_single)
fig <- wrap_plots(plotlist = plot_list, ncol = 1) + 
     plot_layout(guides = 'collect') &
     theme(legend.position = 'bottom')

ggsave(
     filename = "./outcome/publish/fig1p.pdf",
     plot = fig,
     width = 14,
     height = 20,
     device = cairo_pdf,
     family = "Times New Roman"
)

# Seasonal Decomposition --------------------------------------------------

disease_lists <- datafile_class$diseasename

seasonal_test <- function(i) {
     disease_list <- disease_lists[i]
     datafile_plot_single <- datafile_plot |>
          filter(disease == disease_list & date <= split_date_1)
     ts_data <- ts(datafile_plot_single$value, frequency = 12, start = c(2008, 1))
     diff_series <- diff(ts_data, lag = 12)
     adf_test <- adf.test(diff_series)
     
     return(c(disease = disease_list, 
              Dickey_Fuller = round(adf_test$statistic, 2), 
              p_value = round(adf_test$p.value, 4)))
}

results <- lapply(1:length(disease_lists), seasonal_test)
results <- as.data.frame(do.call('rbind', results))

write.xlsx(file = './outcome/appendix/data/Table S2.xlsx',
           x = results)
