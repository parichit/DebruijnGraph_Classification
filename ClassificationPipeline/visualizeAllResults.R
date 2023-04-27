library(ggplot2)
library(magrittr)
library(dplyr)
library(grid)
library(gridExtra)

source("plotAllResults.R")

# Set plot label and size parameters
plot_title_size = 12
subplot_title_size = 9
axis_label_size = 9
axis_tick_size = 6.5
legend_size = 1
legend_title_color = "Black"
typeData = "real"


drawAllPlots <- function(trainData, testData, typeData, out_dir, out_file) {
  
  # trainFilePath = file.path(out_dir, "train_Zreal_results.csv")
  # testFilePath = file.path(out_dir, "test_Zreal_results.csv")
  # trainData <- read.csv2(file = trainFilePath, stringsAsFactors = FALSE, sep=",")
  # testData <- read.csv2(file = testFilePath, stringsAsFactors = FALSE, sep=",")
  
  # trainData <- read.csv2(file = "/Users/schmuck/Library/CloudStorage/OneDrive-IndianaUniversity/Box Sync/PhD/battery-machine-intelligence/ResultsNew/Volt_train_Zreal_results.csv",
  #                        stringsAsFactors = FALSE, sep=",")
  # 
  # testData <- read.csv2(file = "/Users/schmuck/Library/CloudStorage/OneDrive-IndianaUniversity/Box Sync/PhD/battery-machine-intelligence/ResultsNew/Volt_test_Zreal_results.csv",
  #                       stringsAsFactors = FALSE, sep=",")
  
  # Process raw data
  process_data <- function(rawData){
    
    rawData$Model <- factor(rawData$Model)
    rawData$Accuracy <- as.numeric(rawData$Accuracy)
    DataAcc <- rawData[, c(1, 2)]
    DataAcc <- DataAcc %>% arrange(Accuracy)
    return(DataAcc)
  }
  
  
  out <- process_data(trainData)
  trainDataAcc = out

  out <- process_data(testData)
  testDataAcc = out

  
  # source("plotAllResults.R")
  out <- plot_individual_results(trainDataAcc, testDataAcc, "Spread of Accuracy for all models on training data", 
                   "Prediction error on test data", "Accuracy", 1, subplot_title_size, axis_tick_size)
  
  p1 <- out[[1]]
  p2 <- out[[2]]
  
  
  title = "Visualization of Accuracy for all models"
  x_axis_label = "Accuracy"
  y_axis_label = "Model"
  
  grid_title <- textGrob(title, gp = gpar(fontsize = plot_title_size, fontface = 'bold'))
  grid_x_label <- textGrob(y_axis_label, hjust = 0.5, vjust=-2.5, gp = gpar(fontsize = axis_label_size, fontface = 'bold'))
  grid_y_label <- textGrob(x_axis_label, rot=90, vjust = 1.2, gp = gpar(fontsize = axis_label_size, fontface = 'bold'))
  
  combined_plot <- grid.arrange(grid_title, arrangeGrob(p1, p2, nrow=2), nrow=2, left=grid_y_label, bottom=grid_x_label, heights=c(0.5, 10))
  
  # combined_plot <- grid.arrange(grid_title, arrangeGrob(p1, p2, ncol=2), nrow=2, left=grid_y_label, bottom=grid_x_label, heights=c(0.5, 10))
  
  ggsave(file.path(out_dir, paste(typeData, "AllAcc.png", sep="")), combined_plot, dpi=360, height=10,
                width=9, units="in")
  
  # ggsave(file.path(out_dir, paste("Z", typeData, "TrainRMSE.png", sep="")), p1, dpi=360, height=6,
  #        width=8, units="in")
  # ggsave(file.path(out_dir, paste("Z", typeData, "TestRMSE.png", sep="")), p1, dpi=360, height=6,
  #        width=8, units="in")
  

}











