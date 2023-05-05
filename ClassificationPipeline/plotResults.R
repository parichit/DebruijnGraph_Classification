plot_on_grid <- function(trainPlotData, testPlotData, title1, subtitle1,
                         title2, subtitle2, metric_type, my_label, col, 
                         plot_title_size, axis_label_size, axis_tick_size){
  
  x_axis_label = my_label
  y_axis_label = "Model"
  
  
  # Box plot on train data

  p1 <- ggplot(data=trainPlotData, aes(x=trainPlotData[, col], y=reorder(Model, -trainPlotData[, col]),
                                         fill = Model)) +
    geom_jitter(aes(color=Model), show.legend = FALSE, size=0.6, alpha=0.6)+
    geom_boxplot(alpha = 0.7, show.legend = FALSE, color="grey60", fatten=1.5, outlier.shape = NA)
    

  p1 <- p1 + labs(x = x_axis_label, y = y_axis_label,
                  title = title1, subtitle = "") + 
    theme(plot.title = element_text(size=plot_title_size,
                                    face="bold.italic", colour = legend_title_color),
          plot.subtitle = element_text(margin = margin(5, 0, 0, 10)),
          axis.title.x = element_text(face="bold.italic", 
                                      size = axis_label_size),
          axis.title.y = element_text(face="bold.italic",  
                                      size = axis_label_size),
          axis.text.x = element_text(face="bold", size = axis_tick_size),
          axis.text.y = element_text(face="bold", size = axis_tick_size),
          panel.background = element_rect(fill = "white"))
  
  
  y_axis_label = "Model"
  x_axis_label = my_label
  
  # Plot title, axis and subtitle setting
  # title = "Prediction error on test data"
  # subtitle = "RMSE values are shown for top 30 models"
  
  # Bar plot on test data
  p2 <- ggplot(data=testPlotData, aes(x=testPlotData[, col], y=reorder(Model, -testPlotData[, col]),
                                        fill = Model)) + 
    geom_bar(alpha = 0.6, show.legend = FALSE, stat = "identity", 
               color="black", width=0.7, position = position_dodge(width=0.7), linewidth=0.2) +
    geom_text(label=sprintf("%0.3f", testPlotData[, col]),
                hjust="inward", vjust="inward", color = "grey30", size=5)

  
  
  p2 <- p2 + labs(x = x_axis_label, y = y_axis_label,
                  title = title2, subtitle = "") + 
    theme(plot.title = element_text(size=plot_title_size,
                                    face="bold.italic", colour = legend_title_color),
          # plot.margin = margin(10, 5, -35, 0),
          plot.subtitle = element_text(margin = margin(5, 0, 0, 10)),
          axis.title.x = element_text(face="bold.italic", size = axis_label_size),
          axis.title.y = element_text(face="bold.italic", size = axis_label_size),
          axis.text.x = element_text(face="bold", size = axis_tick_size),
          axis.text.y = element_text(face="bold", size = axis_tick_size),
          panel.background = element_rect(fill = "white"))
  
  #final <- grid.arrange(p1, p2)
  
  return(list(p1, p2))
}