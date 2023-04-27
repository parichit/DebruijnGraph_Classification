suppressMessages(library("caretEnsemble"))
suppressMessages(library("rattle"))
library(ggplot2)
library(tidyr)
library(reshape2)

real_comp_best_models = c("ranger", "kknn", "xgbLinear")
imag_comp_best_models = c("Rborist", "RRF")

run_ensemble <- function(pred_type, train_data, number, repeats){

set.seed(47195)
control <- trainControl(method = "repeatedcv", 
                        number = number, 
                        repeats = repeats, 
                        savePredictions = "final",
                        index = createResample(train_data[, 1], number*repeats))

if (pred_type == "real"){
  model_list <- caretList(Zreal~., data=train_data, trControl=control, methodList=real_comp_best_models)
} else if (pred_type == "imag"){
  model_list <- caretList(Zimag~., data=train_data, trControl=control, methodList=imag_comp_best_models)
}

return(model_list)

}

number <- 5
repeats <- 5

total_data = rbind(train_data, test_data)
total_data = scale(total_data)

soc_s = attr(total_data, "scaled:scale")[1]
soc_c = attr(total_data, "scaled:center")[1]

volt_s = attr(total_data, "scaled:scale")[2]
volt_c = attr(total_data, "scaled:center")[2]

freq_s = attr(total_data, "scaled:scale")[3]
freq_c = attr(total_data, "scaled:center")[3]

zreal_s = attr(total_data, "scaled:scale")[4]
zreal_c = attr(total_data, "scaled:center")[4]

zimag_s = attr(total_data, "scaled:scale")[5]
zimag_c = attr(total_data, "scaled:center")[5]

some_test_data <- total_data[((dim(total_data)[1]-dim(test_data)[1]+1):dim(total_data)[1]), ]
some_train_data <- total_data[-((dim(total_data)[1]-dim(test_data)[1]+1):dim(total_data)[1]), ]

real_data <- some_train_data[, -5]
real_test_data <- some_test_data[, -5]

imag_data <- some_train_data[, -4]
imag_test_data <- some_test_data[, -4]

real_data <- total_data[, -5]
imag_data = total_data[, -4]


real_model_list <- run_ensemble("real", real_data, number, repeats)
# real_pred <- as.data.frame(predict(real_model_list, newdata=head(real_test_data)))
ensemble_real_data <- caretEnsemble(real_model_list, metric="RMSE")
summary(ensemble_real_data)


imag_model_list <- run_ensemble("imag", imag_data, number, repeats)
# imag_pred <- as.data.frame(predict(imag_model_list, newdata=head(imag_test_data)))
ensemble_imag_data <- caretEnsemble(imag_model_list, metric="RMSE")
summary(ensemble_imag_data)


### Inverse scale
# real_data = rbind(real_data, real_test_data)
# real_train_preds = predict(ensemble_real_data, newdata = real_data[, c(1,2,3)])
# real_data[, 1] = (real_data[, 1] * soc_s) + soc_c
# real_data[, 2] = (real_data[, 2] * volt_s) + volt_c
# real_data[, 3] = (real_data[, 3] * freq_s) + freq_c
# real_data[, 4] = (real_data[, 4] * zreal_s) + zreal_c
# real_train_preds <- (real_train_preds * zreal_s) + zreal_c
# real_data = as.data.frame(cbind(real_data, "Zreal_pred"=real_train_preds))

# real_test_preds = predict(ensemble_real_data, newdata = real_test_data[, c(1,2,3)])
# real_test_data[, 1] = real_test_data[, 1] * r1_s +  r1_c
# real_test_data[, 2] = (real_test_data[, 2] * r2_s) +  r2_c
# real_test_data[, 3] = (real_test_data[, 3] * r3_s) +  r3_c
# real_test_data[, 4] = (real_test_data[, 4] * r4_s) +  r4_c
# real_test_preds <- (real_test_preds * r4_s) + r4_c
# real_test_data = as.data.frame(cbind(real_test_data, "Zreal_pred"=real_test_preds))
# 
# real_data <- rbind(real_data, real_test_data)

# imag_data = rbind(imag_data, imag_test_data)
# imag_train_preds = predict(ensemble_imag_data, newdata = imag_data[, c(1,2,3)])
# imag_data[, 1] = (imag_data[, 1] * soc_s) + soc_c
# imag_data[, 2] = (imag_data[, 2] * volt_s) + volt_c
# imag_data[, 3] = (imag_data[, 3] * freq_s) + freq_c
# imag_data[, 4] = (imag_data[, 4] * zimag_s) + zimag_c
# imag_train_preds <- (imag_train_preds * zimag_s) + zimag_c
# imag_data = as.data.frame(cbind(imag_data, "Zimag_pred"=imag_train_preds))


# imag_test_preds = predict(ensemble_imag_data, newdata = imag_test_data[, c(1,2,3)])
# imag_test_data[, 1] = (imag_test_data[, 1] * i1_s) +  i1_c
# imag_test_data[, 2] = (imag_test_data[, 2] * i2_s) +  i2_c
# imag_test_data[, 3] = (imag_test_data[, 3] * i3_s) +  i3_c
# imag_test_data[, 4] = (imag_test_data[, 4] * i4_s) +  i4_c
# imag_test_preds <- (imag_test_preds * i4_s) + i4_c
# imag_test_data = as.data.frame(cbind(imag_test_data, "Zimag_pred"=imag_test_preds))
# 
# imag_data <- rbind(imag_data, imag_test_data)


real_preds = predict(ensemble_real_data, newdata = real_data[, c(1,2,3)])
imag_preds = predict(ensemble_imag_data, newdata = imag_data[, c(1,2,3)])

real_data = as.data.frame(cbind(real_data, "Zreal_pred"=real_preds))
imag_data = as.data.frame(cbind(imag_data, "Zimag_pred"=imag_preds))


orig_volt = 3.45
some_volt = orig_volt
# some_volt = (orig_volt - volt_c) / volt_s 

regenerated_data <- as.data.frame(cbind(real_data[, c(1, 2, 3, 4)], "Zimag"=imag_data$Zimag, 
                                        "Zreal_pred"=real_data$Zreal_pred, "Zimag_pred"=imag_data$Zimag_pred))

temp = regenerated_data[regenerated_data$Volt == some_volt, ]

temp1 <- temp[, c(4, 5)]
temp2 = temp[, c(6, 7)]
colnames(temp2) <- c("Zreal", "Zimag")

ggplot(data=temp1, aes(x=Zreal, y=Zimag)) + geom_point() + geom_point(data=temp2, colour="blue")

write.table(regenerated_data, file="Regenerated_whole_data_fit.csv", row.names = FALSE, sep=",")


new_d = read.csv2(file=file.path(base_path,"RegressionPipeline","Regenerated_data.csv"), sep=",")
new_d$Soc <- as.numeric(new_d$Soc)
new_d$Volt <- as.numeric(new_d$Volt)
new_d$Freq <- as.numeric(new_d$Freq)
new_d$Z_r_orig <- as.numeric(new_d$Z_r_orig)
new_d$Z_i_orig <- as.numeric(new_d$Z_i_orig)
new_d$Z_r_pred <- as.numeric(new_d$Z_r_pred)
new_d$Z_i_pred <- as.numeric(new_d$Z_i_pred)

 
temp = new_d[new_d$Volt == orig_volt, ]

temp1 <- temp[, c(4, 5)]
temp2 = temp[, c(6, 7)]
colnames(temp2) <- c("Z_r_orig", "Z_i_orig")

ggplot(data=temp1, aes(x=Z_r_orig, y=Z_i_orig)) + geom_point() + geom_point(data=temp2, colour="blue")




