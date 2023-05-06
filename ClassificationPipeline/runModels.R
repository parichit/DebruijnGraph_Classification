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
                      output_path, train_out_file, test_out_file, stat_file, num_mdls){
  
  train_out = ""
  test_out = ""
  failCounter = 0;
  
  allresultsDF = data.frame()
  
  output_path = out_dir

  set.seed(100)
  control <- trainControl(method = "repeatedcv", 
                          number = number, 
                          repeats = repeats, 
                          savePredictions = "final",
                          index = createResample(train_data$target, number*repeats),
                          classProbs = TRUE)
  
  
    train_out = file.path(output_path, train_out_file)
    test_out = file.path(output_path, test_out_file)
    res_stats = file.path(output_path, stat_file)
    
    # set.seed(9560)
    # train_data <- ROSE(target~., data = train_data)$data
    # 
    # train_data <- as.data.frame(train_data)
    # 
    # target <- train_data$target
    # target <- make.names(target)
    # train_data$target <- target
    
    
    if (num_mdls > 0){
      selected_Models = availableModels[(length(availableModels)-num_mdls):length(availableModels)]
    } else if (num_mdls == 0){
      selected_Models = availableModels
    }
    
  
    for(i in 1:length(selected_Models)){
      
      print(paste("Running", selected_Models[i]))
      
          result <- tryCatch(
          {
             withTimeout(
              {
                caret::train(target~., data=train_data, method=selected_Models[i],
                      trControl = control, preProcess= c("center","scale"), metric="RUC")
              }, timeout = time_limit)
          },
          
          TimeoutException = function(ex) {
            result = "timeout"
            print(paste("Timeout for ", selected_Models[i]))
          },
          
          error = function(err){
            print(err)
            result =  "error"
          }
        )
    #}

    if (length(result) > 1){
      
      # some_mdl = "mlpWeightDecayML"
      
      resultsDF <- as.data.frame(result$resample)
      resultsDF <- resultsDF[, 1]
      resultsDF = cbind("Model"=rep(selected_Models[i], length(resultsDF)), "Accuracy"=resultsDF)
      
      intermediate_train_stats <- caret::confusionMatrix(result$pred$pred, result$pred$obs, mode="everything")
      
      # Get more statistics (particularly the balanced accuracy)
      resultsDF = cbind(resultsDF, "Sensitivity" = intermediate_train_stats$byClass[[1]], 
                            "Specificity"=intermediate_train_stats$byClass[[2]], 
                            "Precision"= intermediate_train_stats$byClass[[5]],  
                            "Recall"=intermediate_train_stats$byClass[[6]], 
                            "F1"=intermediate_train_stats$byClass[[7]], 
                            "Baccuracy"=intermediate_train_stats$byClass[[11]])
      
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
      
      # Calculate the accuracy between predictions and actual test data
      resultsTestDF = data.frame(t(data.matrix(postResample(unlist(testPredictions), test_data$target))))
      
      resultsTestDF = resultsTestDF[, 1]
      
      intermediate_stats <- caret::confusionMatrix(testPredictions, test_data$target, mode="everything")

      resultsTestDF = cbind("Model"=selected_Models[i], "Accuracy"=resultsTestDF)
      
      # Get more statistics (particularly the balanced accuracy)
      resultsTestDF = cbind(resultsTestDF, "Sensitivity" = intermediate_stats$byClass[[1]], 
                            "Specificity"=intermediate_stats$byClass[[2]], 
                            "Precision"= intermediate_stats$byClass[[5]],  
                            "Recall"=intermediate_stats$byClass[[6]], 
                            "F1"=intermediate_stats$byClass[[7]], 
                            "Baccuracy"=intermediate_stats$byClass[[11]])
      
      
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
    acc_stat <- as.data.frame(allresultsDF %>% dplyr::select(Baccuracy, Model) %>% group_by(Model) %>%
                                summarize(min = min(Baccuracy), max = max(Baccuracy), median = median(Baccuracy),
                                          mean = mean(Baccuracy), sd = sd(Baccuracy)) %>%
                                arrange(-mean) %>% mutate_if(is.numeric, function(x) {round(x, 3)}))
    
    
    f1_stat <- as.data.frame(allresultsDF %>% dplyr::select(F1, Model) %>% group_by(Model) %>%
                                summarize(min = min(F1), max = max(F1), median = median(F1),
                                          mean = mean(F1), sd = sd(F1)) %>%
                                arrange(-mean) %>% mutate_if(is.numeric, function(x) {round(x, 3)}))

    acc_stat <- acc_stat[order(acc_stat$max, acc_stat$min, acc_stat$sd),]
    f1_stat <- f1_stat[order(f1_stat$max, f1_stat$min, f1_stat$sd),]
    
    result_statistics <- cbind(acc_stat, f1_stat)
    write.table(result_statistics, file=res_stats, row.names = FALSE, sep= ",", quote = FALSE)
  }
  
}

