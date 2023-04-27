library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)

source("plotResults.R")

# Set plot label and size parameters
plot_title_size = 13
subplot_title_size = 10
axis_label_size = 11
axis_tick_size = 9
legend_size = 1
legend_title_color = "Black"


drawPlots <- function(trainData, testData, typeData, out_dir, out_file) {
  
  outFileName = ""
  outFilePath = file.path(out_dir, out_file)
  
  select_topN <- function(someData, num){
    
    temp = as.vector(unique(someData$Model)[1:num])
    someData <- someData[someData$Model %in% temp, ]
    return(someData)
  }
  
  # Process raw data
  process_data <- function(rawData, type_data){
    
    num = 20
    
    rawData$Model <- factor(rawData$Model)
    rawData$Accuracy <- as.numeric(rawData$Accuracy)
    DataAcc <- rawData[, c(1, 2)]
    
    DataAcc <- DataAcc %>% arrange(Accuracy)
    
    # Select top 20 models for visualization
    if (type_data == "train"){
      DataAcc <- select_topN(DataAcc, num)
    }
    else if (type_data == "test"){
      DataAcc <- DataAcc[!duplicated(DataAcc$Model), ]
      DataAcc <- DataAcc[1:num, ]
    }
    return(DataAcc)
  }
  
  out <- process_data(trainData, "train")
  trainDataAcc = out

  out <- process_data(testData, "test")
  testDataAcc = out
  
  
  # source("plotResults.R")
  out <- plot_on_grid(trainDataAcc, testDataAcc, "(A) Accuracy of models on the validation data", 
                      "Boxplots for top 20 models", "(D) Average acc on test data", 
                      "Accuracy values for top 20 models", "",
                      "Accuracy", 1, subplot_title_size, axis_label_size, axis_tick_size)
  
  p1 <- out[[1]]
  p2 <- out[[2]]
  
  # grid.arrange(p1, p2, nrow=1)
  
  title = paste("Prediction performance of the top 20 models on the ", typeData, sep="")
  grid_title <- textGrob(title, gp = gpar(fontsize = plot_title_size, fontface = 'bold'))
  
  FinalPlot <- grid.arrange(grid_title, arrangeGrob(p1, p2, nrow=1, ncol=2), nrow=2, heights=c(0.5, 10))
  
  # Save the plot on disk
  ggsave(file.path(outFilePath), FinalPlot, dpi=360, height=16,
         width=14, units="in")
  
}


# trainData <- read.csv2(file = "/Users/schmuck/Documents/Box Sync/PhD/ML_Battery_Data/Results/train_Zimag_results.csv",
#                        stringsAsFactors = FALSE, sep=",")
# 
# testData <- read.csv2(file = "/Users/schmuck/Documents/Box Sync/PhD/ML_Battery_Data/Results/test_Zimag_results.csv",
#                       stringsAsFactors = FALSE, sep=",")
# 
# # source("plotResults.R")
# drawPlots(trainData, testData, "imag", out_dir, "Zimag.png")





















       
