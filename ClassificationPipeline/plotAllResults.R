

plot_individual_results <- function(trainPlotData, testPlotData, title1,
                         title2, metric_type, col, plot_title_size, axis_tick_size){
  

  # Box plot on train data
    p1 <- ggplot(data=trainPlotData, aes(y=trainPlotData[, col], x=reorder(Model, -trainPlotData[, col]), fill = Model)) +
      geom_boxplot(alpha = 0.6, show.legend = FALSE, color="grey60", outlier.size = 1, fatten=0.5)

   p1 <- p1 + labs(title = title1, x="", y ="") + 
    theme(plot.title = element_text(size=plot_title_size, hjust=0.0),
          axis.text.x = element_text(size = axis_tick_size, angle=90),
          axis.text.y = element_text(face="bold", size = axis_tick_size),
          panel.background = element_rect(fill = "white"))

  
  # Bar plot on test data
    p2 <- ggplot(data=testPlotData, aes(y=testPlotData[, col], x=reorder(Model, -testPlotData[, col]), fill = Model)) + 
      geom_bar(alpha = 0.6, show.legend = FALSE, stat = "identity", 
               color="black", width=0.6, position = position_dodge(width=0.7), size=0.2) 
      
    # geom_text(label=sprintf("%0.3f", testPlotData[, col]),
    #             hjust="inward", vjust="inward", color = "grey30", size=1.5, angle=90, fontface = "bold")
    
  
    p2 <- p2 + labs(title = title2, x="", y ="") + 
      theme(plot.title = element_text(size=plot_title_size, hjust=0),
          axis.text.x = element_text(size = axis_tick_size, angle=90),
          axis.text.y = element_text(face="bold", size = axis_tick_size),
          panel.background = element_rect(fill = "white"))
  
    
  return(list(p1, p2))
}