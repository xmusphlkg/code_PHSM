
plot_outcome <- function(outcome_plot_1,
                         outcome_plot_2,
                         datafile_single,
                         split_date,
                         max_case,
                         n,
                         inter,
                         title) {
     
     outcome_plot_1_2_link <- data.frame(
          date = c(max(outcome_plot_1$date), min(outcome_plot_2$date)),
          value = c(outcome_plot_1[nrow(outcome_plot_1), 'fit'],
                    outcome_plot_2[1, 'mean'])
     )
     
     max_value <- max(c(max(outcome_plot_1[,-1], na.rm = T), max(outcome_plot_2[,'mean'], na.rm = T)), max_case)
     min_value <- min(c(min(outcome_plot_1[,-1], na.rm = T), min(outcome_plot_2[,'mean'], na.rm = T)))
     
     fig1 <- ggplot()+
          geom_line(mapping = aes(x = date, y = value, colour = 'Observed'), 
                    linewidth = 0.7, data = filter(datafile_single, date <= split_date))+
          geom_line(mapping = aes(x = date, y = fit, colour = 'Fitted'), 
                    linewidth = 0.7, data = outcome_plot_1)+
          geom_line(mapping = aes(x = date, y = value), color = '#DC0000B2', 
                    linewidth = 0.7, data = outcome_plot_1_2_link, show.legend = F)+
          geom_line(mapping = aes(x = date, y = mean, colour = 'Forecasted'),
                    linewidth = 0.7, data = outcome_plot_2)
     
     if (inter) {
          fig1 <- fig1 +
               geom_ribbon(mapping = aes(x = date, ymin = lower_80, ymax = upper_80, fill = 'red'),
                           data = outcome_plot_2, alpha = 0.3, show.legend = F)+
               geom_ribbon(mapping = aes(x = date, ymin = lower_95, ymax = upper_95, fill = 'red'),
                           data = outcome_plot_2, alpha = 0.3, show.legend = F)
     }
     
     fig1 <- fig1 +
          geom_vline(xintercept = max(outcome_plot_1_2_link$date), show.legend = F,
                     linetype = 'longdash')+
          geom_hline(yintercept = 0, show.legend = F)+
          annotate('text', x = median(outcome_plot_1$date), y = Inf, label = 'Train Database', vjust = 1)+
          annotate('text', x = median(outcome_plot_2$date), y = Inf, label = 'Test Database', vjust = 1)+
          coord_cartesian(ylim = c(0, range(pretty(c(min_value, max_value, 0)))[2]))+
          scale_x_date(expand = expansion(add = c(0, 31)),
                       date_labels = '%Y',
                       breaks = seq(min(outcome_plot_1$date), max(outcome_plot_2$date)+31, by="2 years"))+
          scale_y_continuous(expand = c(0, 0),
                             label = scientific_10,
                             breaks = pretty(c(min_value, max_value, 0)))+
          scale_color_manual(
               values = c(Fitted = "#00A087B2",
                          Forecasted = "#DC0000B2",
                          Observed = '#3C5488B2')
          )+
          theme_set()+
          theme(legend.position = 'bottom')+
          labs(x = "Date",
               y = 'Cases',
               color = '',
               title = paste0(LETTERS[n], ': ', title))
     
     return(fig1)
}

evaluate_forecast <- function(actual, forecast) {
     
     smape <- mean(200 * abs(actual - forecast) / (abs(actual) + abs(forecast)))
     rmse <- sqrt(mean((actual - forecast) ^ 2))
     mase <- mean(abs(actual - forecast)) / mean(abs(diff(actual)))
     
     correlation <- cor(actual, forecast)
     r_squared <- correlation^2
     
     return(c('SMAPE' = smape, 'RMSE' = rmse, 'MASE' = mase, 'R_Squared' = r_squared))
}

