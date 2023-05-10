library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)

source("plotResults.R")

# Set plot label and size parameters
plot_title_size = 14
subplot_title_size = 13
axis_label_size = 10
axis_tick_size = 11
legend_size = 3
legend_title_color = "Black"


drawPlots <- function(trainData1, testData1, trainData2, testData2, out_dir, out_file, num_top) {
  
  outFileName = ""
  outFilePath = file.path(out_dir, out_file)
  
  # trainData <- read.csv2(file = "/Users/schmuck/Library/CloudStorage/OneDrive-IndianaUniversity/PhD/TIMP_Classification/debrujin_runs/train_results.csv",
  #                        stringsAsFactors = FALSE, sep=",")
  # 
  # testData <- read.csv2(file = "/Users/schmuck/Library/CloudStorage/OneDrive-IndianaUniversity/PhD/TIMP_Classification/debrujin_runs/test_results.csv",
  #                       stringsAsFactors = FALSE, sep=",")
  # 
  # num_top = 20
  
  select_topN <- function(someData, num_top){
    
    temp = as.vector(unique(someData$Model)[1:num_top])
    someData <- someData[someData$Model %in% temp, ]
    return(someData)
  }
  
  # Process raw data
  process_data <- function(rawData, type_data, num_top){
    
    rawData$Model <- factor(rawData$Model)
    
    rawData$Baccuracy <- as.numeric(rawData$Baccuracy)
    rawData$F1 <- as.numeric(rawData$F1)
    
    DataBacc <- rawData[, c(1, 8)]
    DataF1 <- rawData[, c(1, 7)]
    
    DataBacc <- DataBacc[order(DataBacc$Baccuracy, decreasing=TRUE), ] 
    DataF1 <- DataF1[order(DataF1$F1, decreasing=TRUE), ]

    
    # Select top models for visualization
    if (type_data == "train"){
      DataBacc <- select_topN(DataBacc, num_top)
      DataF1 <- select_topN(DataF1, num_top)
    }
    else if (type_data == "test"){
      DataBacc <- DataBacc[!duplicated(DataBacc$Model), ]
      DataBacc <- DataBacc[1:num_top, ]
      
      DataF1 <- DataF1[!duplicated(DataF1$Model), ]
      DataF1 <- DataF1[1:num_top, ]
    }
    return(list(DataBacc, DataF1))
  }
  
  
  out <- process_data(trainData1, "train", num_top)
  debrujin_trainDataBacc <- out[[1]]
  debrujin_trainDataF1 <- out[[2]]

  out <- process_data(testData1, "test", num_top)
  debrujin_testDataBacc <- out[[1]]
  debrujin_testDataF1 <- out[[2]]
  
  
  # source("plotResults.R")
  out <- plot_on_grid(debrujin_trainDataBacc, debrujin_testDataBacc, "(A) Balanced accuracy on the validation data", 
                      paste("Boxplots for top", num_top, "models"), "(B) Balanced accuracy on test data", 
                      paste("Balanced accuracy for top", num_top, "models"), "",
                      "Accuracy", 2, subplot_title_size, axis_label_size, axis_tick_size)
  
  p1 <- out[[1]]
  p2 <- out[[2]]
  
  
  out <- plot_on_grid(debrujin_trainDataF1, debrujin_testDataF1, "(A) F1 score on the validation data", 
                      paste("Boxplots for top", num_top, "models"), "(B) F1 scores on test data", 
                      paste("F1 values for top", num_top, "models"), "",
                      "F1 score", 2, subplot_title_size, axis_label_size, axis_tick_size)
  
  p3 <- out[[1]]
  p4 <- out[[2]]
  
  title = paste("(A) Classification via Debrujin motifs", sep="")
  grid_title <- textGrob(title, gp = gpar(fontsize = plot_title_size, fontface = 'bold'))
  
  
  plot1 <- grid.arrange(grid_title, arrangeGrob(p1, p2, p3, p4, nrow=2, ncol=2), nrow=2, heights=c(0.5, 10))
  
  
  out <- process_data(trainData2, "train", num_top)
  rand_trainDataBacc <- out[[1]]
  rand_trainDataF1 <- out[[2]]
  
  out <- process_data(testData2, "test", num_top)
  rand_testDataBacc <- out[[1]]
  rand_testDataF1 <- out[[2]]
  
  
  # source("plotResults.R")
  out <- plot_on_grid(rand_trainDataBacc, rand_testDataBacc, "(A) Balanced accuracy on the validation data", 
                      paste("Boxplots for top", num_top, "models"), "(B) Balanced accuracy on test data", 
                      paste("Balanced accuracy for top", num_top, "models"), "",
                      "Accuracy", 2, subplot_title_size, axis_label_size, axis_tick_size)
  
  
  
  p5 <- out[[1]]
  p6 <- out[[2]]
  
  
  out <- plot_on_grid(rand_trainDataF1, rand_testDataF1, "(A) F1 score on the validation data", 
                      paste("Boxplots for top", num_top, "models"), "(B) F1 scores on test data", 
                      paste("F1 values for top", num_top, "models"), "",
                      "F1 score", 2, subplot_title_size, axis_label_size, axis_tick_size)
  
  p7 <- out[[1]]
  p8 <- out[[2]]
  
  
  title = paste("(B) Classification via randomly sampled motifs", sep="")
  grid_title <- textGrob(title, gp = gpar(fontsize = plot_title_size, fontface = 'bold'))
  
  plot2 <- grid.arrange(grid_title, arrangeGrob(p5, p6, p7, p8, nrow=2, ncol=2), nrow=2, heights=c(0.5, 10))
  
  
  FinalPlot <- grid.arrange(plot1, plot2, nrow=2)
  
  # Save the plot on disk
  ggsave(file.path(outFilePath), FinalPlot, dpi=360, height=18,
         width=14, units="in")
  
}


trainData1 <- read.csv2(file = "/Users/schmuck/Library/CloudStorage/OneDrive-IndianaUniversity/PhD/TIMP_Classification/debrujin_runs/train_results.csv",
                       stringsAsFactors = FALSE, sep=",")

testData1 <- read.csv2(file = "/Users/schmuck/Library/CloudStorage/OneDrive-IndianaUniversity/PhD/TIMP_Classification/debrujin_runs/test_results.csv",
                      stringsAsFactors = FALSE, sep=",")



trainData2 <- read.csv2(file = "/Users/schmuck/Library/CloudStorage/OneDrive-IndianaUniversity/PhD/TIMP_Classification/random100_avg/train_results.csv",
                        stringsAsFactors = FALSE, sep=",")

testData2 <- read.csv2(file = "/Users/schmuck/Library/CloudStorage/OneDrive-IndianaUniversity/PhD/TIMP_Classification/random100_avg/test_results.csv",
                       stringsAsFactors = FALSE, sep=",")


out_dir = getwd()
drawPlots(trainData1, testData1, trainData2, testData2, out_dir, "Timp_rand_100.png", 10)





















       
