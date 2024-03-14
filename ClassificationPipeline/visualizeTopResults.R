library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)


# font_import()
# loadfonts(device = "all")

source("plotResults.R")

# Set plot label and size parameters
plot_title_size = 14
subplot_title_size = 13
axis_label_size = 12
axis_tick_size = 11
legend_title_color = "Black"


drawPlots <- function(trainData1, testData1, trainData2, testData2, trainData3, testData3, out_dir, out_file, num_top) {
  
  num_top = 10
  outFileName = ""
  outFilePath = file.path(out_dir, out_file)
  
  select_topN <- function(someData, num_top){
    
    temp = as.vector(unique(someData$Model)[1:num_top])
    someData <- someData[someData$Model %in% temp, ]
    return(someData)
  }
  
  # Process raw data
  process_data <- function(rawData, type_data, num_top, miss_records){
    
    rawData$Model <- factor(rawData$Model)
    
    rawData$Baccuracy <- as.numeric(rawData$Baccuracy)
    rawData$F1 <- as.numeric(rawData$F1)

    DataBacc <- rawData[, c(1, 8)]
    DataF1 <- rawData[, c(1, 7)]

    DataBacc <- DataBacc[order(DataBacc$Baccuracy, decreasing=TRUE), ]
    DataF1 <- DataF1[order(DataF1$F1, decreasing=TRUE), ]
    
    # rawData$Baccuracy <- as.numeric(rawData$Accuracy)
    # rawData$F1 <- as.numeric(rawData$F1)
    # 
    # DataBacc <- rawData[, c(1, 2)]
    # DataF1 <- rawData[, c(1, 7)]
    # 
    # DataBacc <- DataBacc[order(DataBacc$Accuracy, decreasing=TRUE), ] 
    # DataF1 <- DataF1[order(DataF1$F1, decreasing=TRUE), ]
    
    if (miss_records == TRUE){
      DataBacc <- DataBacc[-which((DataBacc$Model) %in% c("nb", "cforest", "rda", "dwdRadial", "svmRadial", "svmRadialCost")),]
      DataF1 <- DataF1[-which((DataF1$Model) %in% c("nb", "cforest", "rda", "dwdRadial", "svmRadial", "svmRadialCost", "bagEarthGCV")),]
    }

    
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
  
  
  ########## Smith Waterman
  
  out <- process_data(trainData1, "train", num_top, FALSE)
  debrujin_trainDataBacc <- out[[1]]
  debrujin_trainDataF1 <- out[[2]]

  out <- process_data(testData1, "test", num_top, FALSE)
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
                      paste("Boxplots for top", num_top, "models"), "(B) F1 score on test data", 
                      paste("F1 values for top", num_top, "models"), "",
                      "F1 score", 2, subplot_title_size, axis_label_size, axis_tick_size)
  
  p3 <- out[[1]]
  p4 <- out[[2]]
  
  title = paste("(A) Smith Waterman aligned Debruijn motifs", sep="")
  grid_title <- textGrob(title, gp = gpar(fontsize = plot_title_size, fontface = 'bold', family="Futura"))
  
  
  plot1 <- grid.arrange(grid_title, arrangeGrob(p1, p2, ncol=2), heights=c(0.5, 10))
  plot2 <- grid.arrange(grid_title, arrangeGrob(p3, p4, ncol=2), heights=c(0.5, 10))
  

  
  
  ########## BLAST

  
  out <- process_data(trainData2, "train", num_top, FALSE)
  blast_trainDataBacc <- out[[1]]
  blast_trainDataF1 <- out[[2]]
  
  out <- process_data(testData2, "test", num_top, FALSE)
  blast_testDataBacc <- out[[1]]
  blast_testDataF1 <- out[[2]]
  
  
  out <- plot_on_grid(blast_trainDataBacc, blast_testDataBacc, "(A) Balanced accuracy on the validation data", 
                      paste("Boxplots for top", num_top, "models"), "(B) Balanced accuracy on test data", 
                      paste("Balanced accuracy for top", num_top, "models"), "",
                      "Accuracy", 2, subplot_title_size, axis_label_size, axis_tick_size)
  
  p5 <- out[[1]]
  p6 <- out[[2]]
  
  
  out <- plot_on_grid(blast_trainDataF1, blast_testDataF1, "(A) F1 score on the validation data", 
                      paste("Boxplots for top", num_top, "models"), "(B) F1 score on test data", 
                      paste("F1 values for top", num_top, "models"), "",
                      "F1 score", 2, subplot_title_size, axis_label_size, axis_tick_size)
  
  p7 <- out[[1]]
  p8 <- out[[2]]
  
  title = paste("(A) BLAST aligned Debruijn motifs", sep="")
  grid_title <- textGrob(title, gp = gpar(fontsize = plot_title_size, fontface = 'bold', family="Futura"))
  
  
  plot3 <- grid.arrange(grid_title, arrangeGrob(p5, p6, ncol=2), heights=c(0.5, 10))
  plot4 <- grid.arrange(grid_title, arrangeGrob(p7, p8, ncol=2), heights=c(0.5, 10))
  
  
  
  
  ########## Random
  
  # trainData3 <- na.omit(trainData3) 
  
  out <- process_data(trainData3, "train", num_top, TRUE)
  rand_trainDataBacc <- out[[1]]
  rand_trainDataF1 <- out[[2]]
  
  
  # DataF1 <- trainData3[, c(1, 7)]
  # DataBacc <- DataBacc[order(DataBacc$Baccuracy, decreasing=TRUE), ] 
  # DataF1 <- DataF1[order(DataF1$F1, decreasing=TRUE), ]
  
  
  out <- process_data(testData3, "test", num_top, FALSE)
  rand_testDataBacc <- out[[1]]
  rand_testDataF1 <- out[[2]]
  # rand_testDataBacc <- round(rand_testDataBacc$Baccuracy, 2)
  # rand_testDataF1 <- round(rand_testDataF1$F1, 2)
  
  
  out <- plot_on_grid(rand_trainDataBacc, rand_testDataBacc, "(A) Balanced accuracy on the validation data", 
                      paste("Boxplots for top", num_top, "models"), "(B) Balanced accuracy on test data", 
                      paste("Balanced accuracy for top", num_top, "models"), "",
                      "Accuracy", 2, subplot_title_size, axis_label_size, axis_tick_size)
  
  p9 <- out[[1]]
  p10 <- out[[2]]
  
  
  out <- plot_on_grid(rand_trainDataF1, rand_testDataF1, "(A) F1 score on the validation data", 
                      paste("Boxplots for top", num_top, "models"), "(B) F1 score on test data", 
                      paste("F1 values for top", num_top, "models"), "",
                      "F1 score", 2, subplot_title_size, axis_label_size, axis_tick_size)
  
  p11 <- out[[1]]
  p12 <- out[[2]]
  
  
  title = paste("(B) Randomly sampled motifs (average of 100 trials)", sep="")
  grid_title <- textGrob(title, gp = gpar(fontsize = plot_title_size, fontface = 'bold', family="Futura"))
  
  
  plot5 <- grid.arrange(grid_title, arrangeGrob(p9, p10, ncol=2), heights=c(0.5, 10))
  plot6 <- grid.arrange(grid_title, arrangeGrob(p11, p12, ncol=2), heights=c(0.5, 10))
  
  
  
  FinalPlot1 <- grid.arrange(plot1, plot3, plot5, nrow=3)
  FinalPlot2 <- grid.arrange(plot2, plot4, plot6, nrow=3)
  
  
  # Save the plot on disk
  ggsave(file.path(outFilePath), FinalPlot1, dpi=650, height=18,
         width=14, units="in")
}


# trainData1 <- read.csv2(file = "/Users/schmuck/Library/CloudStorage/OneDrive-IndianaUniversity/PhD/TIMP_Classification/debrujin_runs/train_sw_results.csv",
#                        stringsAsFactors = FALSE, sep=",")
# 
# testData1 <- read.csv2(file = "/Users/schmuck/Library/CloudStorage/OneDrive-IndianaUniversity/PhD/TIMP_Classification/debrujin_runs/test_sw_results.csv",
#                       stringsAsFactors = FALSE, sep=",")
# 
# 
# trainData2 <- read.csv2(file = "/Users/schmuck/Library/CloudStorage/OneDrive-IndianaUniversity/PhD/TIMP_Classification/debrujin_blast/train_db_results.csv",
#                         stringsAsFactors = FALSE, sep=",")
# 
# testData2 <- read.csv2(file = "/Users/schmuck/Library/CloudStorage/OneDrive-IndianaUniversity/PhD/TIMP_Classification/debrujin_blast/test_db_results.csv",
#                        stringsAsFactors = FALSE, sep=",")
# 
# trainData3 <- read.csv2(file = "/Users/schmuck/Library/CloudStorage/OneDrive-IndianaUniversity/PhD/TIMP_Classification/random100_avg/train_random100_results.csv",
#                         stringsAsFactors = FALSE, sep=",")
# 
# testData3 <- read.csv2(file = "/Users/schmuck/Library/CloudStorage/OneDrive-IndianaUniversity/PhD/TIMP_Classification/random100_avg/test_random100_results.csv",
#                        stringsAsFactors = FALSE, sep=",")
# 
# out_dir = "/Users/schmuck/Library/CloudStorage/OneDrive-IndianaUniversity/PhD/TIMP_Classification/"
# out_file = "TimpBA.png"
# 
# out_dir = getwd()
# drawPlots(trainData1, testData1, trainData2, testData2, trainData3, testData3, out_dir, "TimpF1.png", 10)





















       
