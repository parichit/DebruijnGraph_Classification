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

  
  # trainData <- read.csv2(file = "/Users/schmuck/Documents/Box Sync/PhD/ML_Battery_Data/Results/train_Zreal_results.csv",
  #                        stringsAsFactors = FALSE, sep=",")
  # 
  # testData <- read.csv2(file = "/Users/schmuck/Documents/Box Sync/PhD/ML_Battery_Data/Results/test_Zreal_results.csv",
  #                       stringsAsFactors = FALSE, sep=",")
  
  select_topN <- function(someData){
    
    temp = as.vector(unique(someData$Model)[1:20])
    someData <- someData[someData$Model %in% temp, ]
    return(someData)
  }
  
  # Process raw data
  process_data <- function(rawData, type_data){
    
    rawData$Model <- factor(rawData$Model)
    
    rawData$RMSE <- as.numeric(rawData$RMSE)
    rawData$Rsquared <- as.numeric(rawData$Rsquared)
    rawData$MAE <- as.numeric(rawData$MAE)
    
    DataRMSE <- rawData[, c(1, 4)]
    DataRSQ <- rawData[, c(2, 4)]
    DataMAE <- rawData[, c(3, 4)]
    
    DataRMSE <- DataRMSE %>% arrange(RMSE)
    DataRSQ <- DataRSQ %>% arrange(-Rsquared)
    DataMAE <- DataMAE %>% arrange(MAE)
    
    # Select top 20 models for visualization
    if (type_data == "train"){
      # topModelsRMSE = as.vector(unique(temp_RMSE$Model[1:1000]))
      # topModelsRSQ = as.vector(unique(temp_Rsquared$Model[1:1000]))
      # topModelsMAE = as.vector(unique(temp_MAE$Model[1:1000]))
      #
      # DataRMSE <- rawData[rawData$Model %in% topModelsRMSE, c(1, 4)]
      # DataRSQ <- rawData[rawData$Model %in% topModelsRSQ, c(1, 4)]
      # DataMAE <- rawData[rawData$Model %in% topModelsMAE, c(1, 4)]
      DataRMSE <- select_topN(DataRMSE)
      DataRSQ <- select_topN(DataRSQ)
      DataMAE <- select_topN(DataMAE)
    }
    else if (type_data == "test"){
      DataRMSE <- DataRMSE[!duplicated(DataRMSE$Model), ]
      DataRMSE <- DataRMSE[1:20, ]
      
      DataRSQ <- DataRSQ[!duplicated(DataRSQ$Model), ]
      DataRSQ <- DataRSQ[1:20, ]
      
      DataMAE <- DataMAE[!duplicated(DataMAE$Model), ]
      DataMAE <- DataMAE[1:20, ]
    }
    return(list(DataRMSE, DataRSQ, DataMAE))
  }
  
  
  out <- process_data(trainData, "train")
  trainDataRMSE = out[[1]]
  trainDataRSQ = out[[2]]
  trainDataMAE = out[[3]]
  
  
  out <- process_data(testData, "test")
  testDataRMSE = out[[1]]
  testDataRSQ = out[[2]]
  testDataMAE = out[[3]]
  
  
  # source("plotResults.R")
  out <- plot_on_grid(trainDataRMSE, testDataRMSE, "(A) RMSE of regression models on validation data", 
                      "Boxplots for top 20 models", "(D) Average error on test data", 
                      "RMSE values for top 20 models", "",
                      "RMSE (Root mean squared error)", 1, subplot_title_size, axis_label_size, axis_tick_size)
  
  p1 <- out[[1]]
  p2 <- out[[2]]
  
  # grid.arrange(p1, p2, nrow=1)
  
  out <- plot_on_grid(trainDataMAE, testDataMAE, "(B) MAE of regression models on validation data", 
                      "Boxplots for top 20 models", "(E) Average error on test data", 
                      "MAE values for top 20 models", "",
                      "MAE", 1, subplot_title_size, axis_label_size, axis_tick_size)
  p3 <- out[[1]]
  p4 <- out[[2]]
  
  
  out <- plot_on_grid(trainDataRSQ, testDataRSQ, "(C) RSquared of regression models on validation data", 
                      "Boxplots for top 20 models", "(F) Cor-relation between ground-truth and predictions measured via R-Squared", 
                      "RSquared values for top 20 models", "Rsquared",
                      "RSquared", 1, subplot_title_size, axis_label_size, axis_tick_size)
  
  p5 <- out[[1]]
  p6 <- out[[2]]
  
  # grid.arrange(p3, p4, nrow=1)
  
  title = paste("Prediction performance of the top 20 models on the ", typeData, " component of impedance spectra (Z_", typeData, ")", sep="")
  grid_title <- textGrob(title, gp = gpar(fontsize = plot_title_size, fontface = 'bold'))
  
  FinalPlot <- grid.arrange(grid_title, arrangeGrob(p1, p2, p3, p4, p5, p6, nrow=3, ncol=2), nrow=2, heights=c(0.5, 10))
  
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





















       
