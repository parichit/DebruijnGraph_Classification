plot_on_grid <- function(trainPlotData, testPlotData, title1, subtitle1,
                         title2, subtitle2, metric_type, my_label, col, 
                         plot_title_size, axis_label_size, axis_tick_size){
  
  x_axis_label = my_label
  # x_axis_label = "Hello"
  # title1 = "Hello Woirld"
  y_axis_label = "Model"
  # col = 2
  
  palate = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a')
  
  
  # Box plot on train data

  # p1 <- ggplot(data=trainPlotData, aes(x=trainPlotData[, col], y=reorder(Model, -trainPlotData[, col]),
  #                                        fill = Model)) +
  #   geom_boxplot(show.legend = FALSE, fatten=0.5)
  
  p1 <- ggplot(data=debrujin_trainDataBacc, aes(x=debrujin_trainDataBacc[, col], y=reorder(Model, -debrujin_trainDataBacc[, col]),
                                       fill = Model)) +
    geom_boxplot(show.legend = FALSE, fatten=0.5)


  p1 <- p1 + labs(x = x_axis_label, y = y_axis_label,
                  title = title1, subtitle = "") + 
    theme(plot.title = element_text(size=plot_title_size, family="Futura",
                                    face="bold.italic", colour = legend_title_color),
          plot.subtitle = element_text(margin = margin(5, 0, 0, 10)),
          axis.title.x = element_text(face="bold.italic", , family="Geneva", 
                                      size = axis_label_size),
          axis.title.y = element_text(face="bold.italic", family="Geneva",  
                                      size = axis_label_size),
          axis.text.x = element_text(face="bold", family="Lucida Grande", size = axis_tick_size),
          axis.text.y = element_text(face="bold", family="Lucida Grande", size = axis_tick_size),
          panel.background = element_rect(fill = "white"))
  
  
  y_axis_label = "Model"
  x_axis_label = my_label
  
  # Plot title, axis and subtitle setting
  # title = "Prediction error on test data"
  # subtitle = "RMSE values are shown for top 10 models"
  
  # Bar plot on test data
  p2 <- ggplot(data=testPlotData, aes(x=testPlotData[, col], y=reorder(Model, -testPlotData[, col]),
                                        fill = Model)) + 
    geom_bar(show.legend = FALSE, stat = "identity", 
               color="black", width=0.7, position = position_dodge(width=0.7), linewidth=0.2) +
    geom_text(label=sprintf("%0.3f", testPlotData[, col]),
                hjust="inward", vjust="inward", color = "grey15", size=5)

  
  
  p2 <- p2 + labs(x = x_axis_label, y = y_axis_label,
                  title = title2, subtitle = "") + 
    theme(plot.title = element_text(size=plot_title_size, family="Futura",
                                    face="bold.italic", colour = legend_title_color),
          # plot.margin = margin(10, 5, -35, 0),
          plot.subtitle = element_text(margin = margin(5, 0, 0, 10)),
          axis.title.x = element_text(face="bold.italic", family="Geneva", size = axis_label_size),
          axis.title.y = element_text(face="bold.italic", family="Geneva", size = axis_label_size),
          axis.text.x = element_text(face="bold", family="Lucida Grande", size = axis_tick_size),
          axis.text.y = element_text(face="bold", family="Lucida Grande", size = axis_tick_size),
          panel.background = element_rect(fill = "white"))
  
  p2 <- p2 + scale_fill_manual(values=palate)
  
  return(list(p1, p2))
}