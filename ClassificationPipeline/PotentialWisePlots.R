library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)
library(ggrepel)

plot_title_size = 14
subplot_title_size = 6
axis_label_size = 14
axis_tick_size = 12
legend_size = 1
title_color = "Black"


data <- read.csv2(file = "/Users/schmuck/Library/CloudStorage/OneDrive-IndianaUniversity/PhD/ML_Battery_Data/RegressionPipeline/Regenerated_data.csv",
                       stringsAsFactors = FALSE, sep=",")

colnames(data) <- c("Soc", "Volt", "Freq", "Zreal", "Zimag", "Zreal_pred", "Zimag_pred")
data <- as.data.frame(apply(data, 2, as.numeric))
data[, 1] = as.factor(data[, 1])
data[, 2] = as.factor(data[, 2])
data$Zimag = -data$Zimag
data$Zimag_pred = -data$Zimag_pred
data = data[order(data$Freq, decreasing = TRUE), ]


potentail_plots <- function(){
  
  potentials = unique(data$Volt)
  out = list()
  
  for (i in 1:length(potentials)){
      
    temp_data = data[data$Volt == potentials[i], ]
    
    temp_data1 <- temp_data[, c(4, 5)]
    temp_data2 <- temp_data[, c(6, 7)]
    colnames(temp_data2) <- c("Zreal", "Zimag")
    temp_data1 <- rbind(temp_data1, temp_data2)
    Annotation=c(rep("Data", nrow(temp_data)), rep("Regenerated", nrow(temp_data)))
    temp_data1 <- cbind(temp_data1, Annotation)
    
    labels = rep("", nrow(temp_data1))
    start = which(temp_data$Freq>100000)
    mid = which(temp_data$Freq>=0.9 & temp_data$Freq<=1)
    end = nrow(temp_data)
    
    # print(paste(potentials[i], " ", mid))
    
    labels[start] = "100 kHz"
    labels[mid] = "1 Hz"
    labels[end] = "10 mHz"
    
    temp_data1["labels"] = labels
    
    # p1 <- ggplot(data=temp_data, aes(x=Zreal, y=Zimag)) + geom_point(colour ="black", fill="pink", shape=21, size=4) + 
    #   geom_path(data=temp_data, aes(x=Zreal_pred, y=Zimag_pred), color="skyblue", linewidth=0.8) + geom_hline(yintercept = 0, 
    #                                                                                                linetype="dashed", size=0.5)
    # 
    # p1 <- p1 + labs(x=expression(bold(paste(Z[r], " / ", Omega, " cm"^2, sep=" "))),
    #                 y = expression(bold(paste(-Z[j], " / ", Omega, " cm"^2, sep=" "))),
    #                 title = paste(potentials[i], "V")) +
    #   theme(plot.title = element_text(size=plot_title_size, colour = title_color, hjust=0.5),
    #         plot.subtitle = element_text(margin = margin(5, 0, 0, 10)),
    #         axis.title.x = element_text(face="bold.italic",
    #                                     size = axis_label_size),
    #         axis.title.y = element_text(face="bold.italic",
    #                                     size = axis_label_size),
    #         axis.text.x = element_text(face="bold", size = axis_tick_size),
    #         axis.text.y = element_text(face="bold", size = axis_tick_size),
    #         panel.background = element_rect(fill = "white"),
    #         panel.border = element_rect(colour = "#663300", fill=NA, linewidth=0.8))
    
      if (i == 2){
        pos = "right"
      } else{
        pos = "none"
      }
      
    p1 <- ggplot(data=temp_data1[temp_data1$Annotation=="Data", ], aes(x=Zreal, y=Zimag, colour=Annotation)) +
      
      geom_point(shape=1, size=3) +
      
      geom_text_repel(aes(label=labels), colour="black", box.padding = 0.5, max.overlaps = Inf, size=4, fontface="bold") +
      
      geom_path(data=temp_data1[temp_data1$Annotation=="Regenerated", ], size=0.8) +
      
      geom_hline(yintercept = 0, linetype="dashed", size=0.5) +
      
      geom_point(data=temp_data1[temp_data1$labels!="", ], aes(x=Zreal, y=Zimag, fill="pink"), size=4, show.legend = FALSE) +
      
      scale_colour_manual(values = c("pink", "skyblue"),
                          guide = guide_legend(override.aes = list(
                            linetype = c(rep("blank", 1), "solid"),
                            shape = c(rep(21, 1), NA),
                            size=c(rep(5, 1), 2))))

      p1 <- p1 + labs(x=expression(bold(paste(Z[r], " / ", Omega, " cm"^2, sep=" "))),
                      y = expression(bold(paste(-Z[j], " / ", Omega, " cm"^2, sep=" "))),
                      title = paste(potentials[i], "V")) +
        theme(plot.title = element_text(size=plot_title_size, colour = title_color, hjust=0.5),
              plot.subtitle = element_text(margin = margin(5, 0, 0, 10)),
              axis.title.x = element_text(face="bold.italic",
                                          size = axis_label_size),
              axis.title.y = element_text(face="bold.italic",
                                          size = axis_label_size),
              axis.text.x = element_text(face="bold", size = axis_tick_size),
              axis.text.y = element_text(face="bold", size = axis_tick_size),
              panel.background = element_rect(fill = "white"),
              panel.border = element_rect(colour = "#663300", fill=NA, linewidth=0.8),
              
              legend.background = element_rect(color="#663300"),
              # Change legend key size and key width
              legend.key.size = unit(1.5, "cm"),
              legend.key.width = unit(3,"cm"),
              legend.title = element_text(face="bold", size=10, hjust=0.5),
              legend.text = element_text(face="bold", size=9, color = "black"),
              legend.position = pos)
    
    out[[i]] <- p1
    
  }
  return(out)
}

title = "Visualizing the regenrated impedance for each cell potential"
grid_title <- textGrob(title, gp = gpar(fontsize = 16, fontface = 'bold'))


get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

comb_plots = potentail_plots()
myLegend <- get_legend(comb_plots[[2]])
comb_plots[[2]] <- comb_plots[[2]] + theme(legend.position = "none")
comb_plots[[length(comb_plots)+1]] <- myLegend

FinalPlot = grid.arrange(grid_title, arrangeGrob(grobs=comb_plots, nrow=4, ncol=3), 
                         nrow=2, heights=c(0.5, 10))

ggsave("/Users/schmuck/Library/CloudStorage/OneDrive-IndianaUniversity/PhD/ML_Battery_Data/RegressionPipeline/PotentialWisePlot.png", 
       FinalPlot, dpi=360, height=16,
       width=14, units="in")


# temp_data = data[data$Volt == 3.45, ]
# temp_data1 <- temp_data[, c(4, 5)]
# temp_data2 <- temp_data[, c(6, 7)]
# colnames(temp_data2) <- c("Zreal", "Zimag")
# temp_data1 <- rbind(temp_data1, temp_data2)
# Annotation=c(rep("Data", nrow(temp_data)), rep("Regenerated", nrow(temp_data)))
# temp_data1 <- cbind(temp_data1, Annotation)
# 
# 
# labels = rep("", nrow(temp_data1))
# start = which(temp_data$Freq>100000)
# mid = which(temp_data$Freq>=0.999 & temp_data$Freq<1)
# end = nrow(temp_data)
# 
# labels[start] = "100 kHz"
# labels[mid] = "1 Hz"
# labels[end] = "10 mHz"
# 
# temp_data1["labels"] = labels
# 
# p1 <- ggplot(data=temp_data1[temp_data1$Annotation=="Data", ], aes(x=Zreal, y=Zimag, colour=Annotation)) +
# 
#   geom_point(shape=1, size=3) +
# 
#   geom_text_repel(aes(label=labels), colour="black", fontface="bold.italic", box.padding = 0.5, max.overlaps = Inf) +
# 
#   geom_path(data=temp_data1[temp_data1$Annotation=="Regenerated", ], size=0.8) +
# 
#   geom_hline(yintercept = 0, linetype="dashed", size=0.5) +
# 
#   geom_point(data=temp_data1[temp_data1$labels!="", ], aes(x=Zreal, y=Zimag, fill="pink"), size=4, show.legend = FALSE) +
# 
#   scale_colour_manual(values = c("pink", "skyblue"),
#                       guide = guide_legend(override.aes = list(
#                       linetype = c(rep("blank", 1), "solid"),
#                       shape = c(rep(21, 1), NA),
#                       size=c(rep(5, 1), 2))))
# 
# p1 <- p1 + labs(x=expression(bold(paste(Z[r], " / ", Omega, " cm"^2, sep=" "))),
#                 y = expression(bold(paste(-Z[j], " / ", Omega, " cm"^2, sep=" "))),
#                 title = paste("3.14", "V")) +
#   theme(plot.title = element_text(size=plot_title_size, colour = title_color, hjust=0.5),
#         plot.subtitle = element_text(margin = margin(5, 0, 0, 10)),
#         axis.title.x = element_text(face="bold.italic",
#                                     size = axis_label_size),
#         axis.title.y = element_text(face="bold.italic",
#                                     size = axis_label_size),
#         axis.text.x = element_text(face="bold", size = axis_tick_size),
#         axis.text.y = element_text(face="bold", size = axis_tick_size),
#         panel.background = element_rect(fill = "white"),
#         panel.border = element_rect(colour = "#663300", fill=NA, linewidth=0.8),
# 
#         legend.background = element_rect(color="black"),
#         # Change legend key size and key width
#         legend.key.size = unit(1.5, "cm"),
#         legend.key.width = unit(3,"cm"),
#         legend.title = element_text(face="bold", size=10, hjust=0.5),
#         legend.text = element_text(face="bold", size=9, color = "black"),
#         legend.position = "right")
# p1



