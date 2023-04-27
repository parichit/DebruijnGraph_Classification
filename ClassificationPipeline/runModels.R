# load packages
require(caret)
require(magrittr)
require(tidyverse)
require(caretEnsemble)
require(ggthemes)
require(ggplot2)
require(xtable)
require(tictoc)
require(R.utils)
require(dplyr)


runModels <- function(selected_Models, train_data, test_data, time_limit, number, repeats,
                      output_path, train_out_file, test_out_file, stat_file){
  
  train_out = ""
  test_out = ""
  failCounter = 0;
  
  allresultsDF = data.frame()
  
  output_path = out_dir
  selected_Models = availableModels
  
  set.seed(47195)
  control <- trainControl(method = "repeatedcv", 
                          number = number, 
                          repeats = repeats, 
                          savePredictions = "final",
                          index = createResample(train_data[, 1], number*repeats), 
                          allowParallel = FALSE)
  

    train_out = file.path(output_path, train_out_file)
    test_out = file.path(output_path, test_out_file)
    res_stats = file.path(output_path, stat_file)
  
  
  selected_Models = selected_Models[(length(selected_Models)-2):length(selected_Models)]
  # selected_Models = selected_Models[100:169]
  
  # result <- train(Zreal~., data=train_data, method="rbfDDA",
  #       trControl = control, preProcess= c("center","scale"))
  
  
  for(i in 1:length(selected_Models)){
    
    print(paste("Running", selected_Models[i]))
    
        result <- tryCatch(
        {
           withTimeout(
            {
              train(target~., data=train_data, method=selected_Models[i],
                    trControl = control, preProcess= c("center","scale"))
            }, timeout = time_limit)
        },
        
        TimeoutException = function(ex) {
          result = "timeout"
          print(paste("Timeout for ", selected_Models[i]))
        },
        
        error = function(err){
          result =  "error"
        }
      )

    if (length(result) > 1){
      
      resultsDF <- as.data.frame(result$resample)
      resultsDF["Model"] = rep(selected_Models[i], nrow(resultsDF))
      resultsDF <- resultsDF[, c(1,4)]
      
      allresultsDF <- rbind(allresultsDF, resultsDF)
      
      # Save the results on training data in file
      if (!file.exists(train_out)){
        write.table(resultsDF, file=train_out, row.names = FALSE, sep = "," , quote = FALSE)
      } else{
        write.table(resultsDF, file=train_out, row.names = FALSE, sep = "," , append = TRUE, quote = FALSE, 
                    col.names = FALSE)
      }
      
      # Predict the results on test data
      testPredictions <- predict(result, test_data)
      
      # Calculate the RMSE between predictions and actual test data
      resultsTestDF = data.frame(t(data.matrix(postResample(unlist(testPredictions),  test_data$target))))

      resultsTestDF["Model"] = selected_Models[i]
      resultsTestDF = resultsTestDF[, c(1, 3)]
      
      if (!file.exists(test_out)){
        write.table(resultsTestDF, file=test_out, row.names = FALSE, sep= ",", quote = FALSE)
      } else{
        write.table(resultsTestDF, file=test_out, row.names = FALSE, append = TRUE, sep = ",", quote = FALSE, 
                    col.names = FALSE)
      }
      
    } else if (length(result) == 1){
      failCounter = failCounter + 1
    }
  
        
  }
  
  if (length(failCounter) == length(selected_Models)){
    print("None of the models completed successfully.")
  } else if(length(failCounter) < length(selected_Models)){
    
    
    # Save summary statistics of k-fold cross-validation 
    acc_stat <- as.data.frame(allresultsDF %>% dplyr::select(Accuracy, Model) %>% group_by(Model) %>%
                                summarize(min = min(Accuracy), max = max(Accuracy), median = median(Accuracy),
                                          mean = mean(Accuracy), sd = sd(Accuracy)) %>%
                                arrange(-mean) %>% mutate_if(is.numeric, function(x) {round(x, 3)}))


    # mae_stat <- mae_stat[order(mae_stat$max, mae_stat$min, mae_stat$sd),]
    acc_stat <- acc_stat[order(acc_stat$max, acc_stat$min, acc_stat$sd),]
    # rsquared_stat <- rsquared_stat[order(-rsquared_stat$min, rsquared_stat$sd, 
    #                                      -rsquared_stat$max, -rsquared_stat$mean, 
    #                                      -rsquared_stat$median),]
    
    # result_statistics <- cbind(rmse_stat, mae_stat, rsquared_stat)
    write.table(acc_stat, file=res_stats, row.names = FALSE, sep= ",", quote = FALSE)
  }
  
}

