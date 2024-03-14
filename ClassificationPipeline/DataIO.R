require("readxl")
require("stringr")
require("caret")
require("ROSE")

load_data <- function(base_data_path, upsample){

  
# Read the raw data
read_data <- function(base_data_pathm, upsample){
  
  Inputdata <- read.csv2(base_data_path, sep=",", stringsAsFactors = FALSE)
  
  target <- Inputdata$target
  Inputdata <- Inputdata[, -1]
  
  Inputdata <- apply(Inputdata, 2, as.numeric)
  Inputdata <- as.data.frame(Inputdata)
  
  Inputdata = cbind("target"=as.factor(target), Inputdata)
  
  if(upsample == "TRUE"){
      set.seed(9560)
      Inputdata <- ROSE(target~., data = Inputdata)$data
  } else{
    print("NO UPSAMPLING")
  }
  
  Inputdata <- as.data.frame(Inputdata)
  
  target <- Inputdata$target
  target <- make.names(target)
  Inputdata$target <- target
  
  return(Inputdata)
}



# Inputdata <- read_data(file.path(base_path, "data", "308_full.csv"))

# for(i in list_files){
#     temp <- read.table(file=i, header = TRUE, stringsAsFactors = FALSE)
#     soc_values <- rep(tools::file_path_sans_ext(basename(i), nrow(temp)))
#     temp = cbind("soc"=as.numeric(soc_values), temp)
#     Inputdata <- rbind(Inputdata, temp)
# }

Inputdata <- read_data(base_data_path, upsample)


# Create training and test data
set.seed(555)
train_indices <- createDataPartition(y = as.factor(Inputdata$target), p = 0.75, list = FALSE)
training_data <- as.data.frame(Inputdata[train_indices, ])
test_data <- as.data.frame(Inputdata[-train_indices, ])


#################
# Save the train and test data
#################
# write.table(training_data, file=file.path(dirname(base_data_path), "debrujin_train_data.csv"), sep=",", row.names = FALSE)
# write.table(test_data, file=file.path(dirname(base_data_path), "debrujin_test_data.csv"), sep=",", row.names = FALSE)


print("Data read-in successfully")
print(paste("Rows:", nrow(Inputdata), " Cols:", ncol(Inputdata)))
out = list("train_data" = training_data, "test_data" = test_data)


return(out)

}


