
number = 5
repeats = 2

set.seed(47195)
control <- trainControl(method = "repeatedcv", 
                        number = number, 
                        repeats = repeats, 
                        savePredictions = "final",
                        index = createResample(train_data[, 1], number*repeats), 
                        allowParallel = FALSE)

selected_Models <- availableModels

selected_Models <- selected_Models[(length(selected_Models)-1):length(selected_Models)]

res = list()

for(i in 1:length(selected_Models)){
      res[[i]] <- train(target~., data=train_data, method=selected_Models[i],
        trControl = control, preProcess= c("center","scale"))
}
