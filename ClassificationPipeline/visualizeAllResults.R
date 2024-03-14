library(ggplot2)
library(magrittr)
library(dplyr)
library(grid)
library(gridExtra)

source("plotAllResults.R")

# Set plot label and size parameters
plot_title_size = 13
subplot_title_size = 11
axis_label_size = 7
axis_tick_size = 7
legend_size = 3
legend_title_color = "Black"


drawAllPlots <- function(trainData, testData, typeData, out_dir) {
  
  # Process raw data
  process_data <- function(rawData){
    
    rawData$Model <- factor(rawData$Model)
    
    rawData$Baccuracy <- as.numeric(rawData$Baccuracy)
    rawData$F1 <- as.numeric(rawData$F1)
    
    DataBacc <- rawData[, c(1, 8)]
    DataF1 <- rawData[, c(1, 7)]
    
    DataBacc <- DataBacc[order(DataBacc$Baccuracy, decreasing=TRUE), ] 
    DataF1 <- DataF1[order(DataF1$F1, decreasing=TRUE), ]
    
    return(list(DataBacc, DataF1))
  }
  
  
  out <- process_data(trainData)
  trainDataBacc <- out[[1]]
  trainDataF1 <- out[[2]]

  
  out <- process_data(testData)
  testDataBacc <- out[[1]]
  testDataF1 <- out[[2]]

  
  out <- plot_individual_results(trainDataBacc, testDataBacc, "Spread of balanced accuracy for all models on validation data", 
                   "Balanced accuracy on test data", "Balanced accuracy", 2, subplot_title_size, axis_tick_size)
  
  p1 <- out[[1]]
  p2 <- out[[2]]
  
  title = "Visualization of balanced accuracy for all models"
  x_axis_label = "Balanced accuracy"
  y_axis_label = "Model"
  
  
  grid_title <- textGrob(title, gp = gpar(fontsize = plot_title_size, fontface = 'bold'))
  grid_x_label <- textGrob(y_axis_label, hjust = 0.5, vjust=-2.5, gp = gpar(fontsize = axis_label_size, fontface = 'bold'))
  grid_y_label <- textGrob(x_axis_label, rot=90, vjust = 1.2, gp = gpar(fontsize = axis_label_size, fontface = 'bold'))
  
  
  combined_plot <- grid.arrange(grid_title, arrangeGrob(p1, p2, nrow=2), nrow=2, left=grid_y_label, 
                                bottom=grid_x_label, heights=c(0.5, 10))
  
  
  ggsave(file.path(out_dir, paste(typeData, "AllBacc.png", sep="_")), combined_plot, dpi=360, height=10,
         width=12, units="in")
  
  
  
  out <- plot_individual_results(trainDataF1, testDataF1, "Spread of F1 scores for all models on validation data", 
                                 "F1 score on test data", "F1 score", 2, subplot_title_size, axis_tick_size)
  
  p1 <- out[[1]]
  p2 <- out[[2]]
  
  title = "Visualization of F1 score for all models"
  x_axis_label = "F1 score"
  y_axis_label = "Model"
  
  grid_title <- textGrob(title, gp = gpar(fontsize = plot_title_size, fontface = 'bold'))
  grid_x_label <- textGrob(y_axis_label, hjust = 0.5, vjust=-2.5, gp = gpar(fontsize = axis_label_size, fontface = 'bold'))
  grid_y_label <- textGrob(x_axis_label, rot=90, vjust = 1.2, gp = gpar(fontsize = axis_label_size, fontface = 'bold'))
  
  combined_plot <- grid.arrange(grid_title, arrangeGrob(p1, p2, nrow=2), nrow=2, left=grid_y_label, 
                                bottom=grid_x_label, heights=c(0.5, 10))
  
  
  ggsave(file.path(out_dir, paste(typeData, "AllF1.png", sep="_")), combined_plot, dpi=360, height=10,
                width=12, units="in")
  

}

# trainData <- read.csv2(file = "/Users/schmuck/Library/CloudStorage/OneDrive-IndianaUniversity/PhD/TIMP_Classification/debrujin_blast/train_results.csv",
#                         stringsAsFactors = FALSE, sep=",")
# 
# testData <- read.csv2(file = "/Users/schmuck/Library/CloudStorage/OneDrive-IndianaUniversity/PhD/TIMP_Classification/debrujin_blast/test_results.csv",
#                        stringsAsFactors = FALSE, sep=",")
# 
# out_dir = "/Users/schmuck/Library/CloudStorage/OneDrive-IndianaUniversity/PhD/TIMP_Classification/random100_avg/"
# 
# drawAllPlots(trainData, testData, "debrujin_blast", out_dir)











