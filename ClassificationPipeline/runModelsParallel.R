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
require(foreach)


runModels <- function(selected_Models, train_data, test_data, time_limit, number, repeats,
                      type_pred, output_path){
  
  train_out = ""
  test_out = ""
  failCounter = 0;
  
  allresultsDF = data.frame()
  
  # output_path = out_dir
  # type_pred = "real"
  # selected_Models = availableModels
  
  set.seed(47195)
  control <- trainControl(method = "repeatedcv", 
                          number = number, 
                          repeats = repeats, 
                          savePredictions = "final",
                          index = createResample(train_data$Soc, number*repeats),
                          allowParallel = TRUE)
  
  if (type_pred == "real"){
    train_out = file.path(output_path, "train_Zreal_results.csv")
    test_out = file.path(output_path, "test_Zreal_results.csv")
    res_stats = file.path(output_path, "ZrealStats.csv")
  } else if (type_pred == "imag"){
    train_out = file.path(output_path, "train_Zimag_results.csv")
    test_out = file.path(output_path, "test_Zimag_results.csv")
    res_stats = file.path(output_path, "ZimagStats.csv")
  }
  
  # selected_Models = selected_Models[(length(selected_Models)-3):length(selected_Models)]
  # selected_Models = selected_Models[2: length(selected_Models)]
  
  # result <- train(Zreal~., data=train_data, method="rbfDDA",
  #       trControl = control, preProcess= c("center","scale"))
  
  # result <- withTimeout(
  #     train(Zreal~., data=train_data, method="ANFIS",
  #           trControl = control, preProcess= c("center","scale")), timeout = 2)
  
  
  library(doParallel)
  registerDoParallel(cores = detectCores() - 1)
  
  
  for(i in 1:length(selected_Models))
  {
    
    print(paste("Running", selected_Models[i]))
    
    if (type_pred == "real"){
      
      result <- tryCatch(
        {
          withTimeout(
            {
              train(Zreal~., data=train_data, method=selected_Models[i],
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
      
    } else if (type_pred == "imag"){
      
      result <- tryCatch(
        {
          withTimeout(
            {
              train(Zimag~., data=train_data, method=selected_Models[i],
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
      
    }
    
    
    if (length(result) > 1){
      
      resultsDF <- as.data.frame(result$resample)
      resultsDF <- resultsDF[, -ncol(resultsDF)]
      resultsDF["Model"] = rep(selected_Models[i], nrow(resultsDF))
      
      allresultsDF <- rbind(allresultsDF, resultsDF)
      
      # Save the results on training data in file
      if (!file.exists(train_out)){
        write.table(resultsDF, file=train_out, row.names = FALSE, sep = "," , quote = FALSE)
      }
      else{
        write.table(resultsDF, file=train_out, row.names = FALSE, sep = "," , append = TRUE, quote = FALSE, 
                    col.names = FALSE)
      }
      
      # Predict the results on test data
      testPredictions <- predict(result, test_data)
      
      # Calculate the RMSE between predictions and actual test data
      if (type_pred == "real"){
        resultsTestDF = data.frame(t(data.matrix(postResample(as.numeric(unlist(testPredictions)),  test_data$Zreal))))
      } else if (type_pred == "imag"){
        resultsTestDF = data.frame(t(data.matrix(postResample(as.numeric(unlist(testPredictions)),  test_data$Zimag))))
      }
      
      resultsTestDF["Model"] = selected_Models[i]
      
      if (!file.exists(test_out)){
        write.table(resultsTestDF, file=test_out, row.names = FALSE, sep= ",", quote = FALSE)
      }
      else{
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
    mae_stat <- as.data.frame(allresultsDF %>% dplyr::select(MAE, Model) %>% group_by(Model) %>%
                                summarize(min = min(MAE), max = max(MAE), median = median(MAE), 
                                          mean = mean(MAE), sd= sd(MAE)) %>%
                                arrange(-mean) %>% mutate_if(is.numeric, function(x) {round(x, 3)}))
    
    rmse_stat <- as.data.frame(allresultsDF %>% dplyr::select(RMSE, Model) %>% group_by(Model) %>%
                                 summarize(min = min(RMSE), max = max(RMSE), median = median(RMSE), 
                                           mean = mean(RMSE), sd= sd(RMSE)) %>% arrange(-mean) %>%
                                 mutate_if(is.numeric, function(x) {round(x, 3)}))
    
    rsquared_stat <- as.data.frame(allresultsDF %>% dplyr::select(Rsquared, Model) %>% group_by(Model) %>%
                                     summarize(min = min(Rsquared), max = max(Rsquared), median = median(Rsquared), 
                                               mean = mean(Rsquared), sd= sd(Rsquared)) %>% arrange(-mean) %>%
                                     mutate_if(is.numeric, function(x) {round(x, 3)}))
    
    
    mae_stat <- mae_stat[order(mae_stat$max, mae_stat$min, mae_stat$sd),]
    rmse_stat <- rmse_stat[order(rmse_stat$max, rmse_stat$min, rmse_stat$sd),]
    rsquared_stat <- rsquared_stat[order(-rsquared_stat$min, rsquared_stat$sd, 
                                         -rsquared_stat$max, -rsquared_stat$mean, 
                                         -rsquared_stat$median),]
    
    result_statistics <- cbind(rmse_stat, mae_stat, rsquared_stat)
    
    write.table(result_statistics, file=res_stats, row.names = FALSE, sep= ",", quote = FALSE)
  }
  
}

