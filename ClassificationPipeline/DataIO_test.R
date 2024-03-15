require("readxl")
require("stringr")
require("caret")
require("ROSE")

load_data <- function(base_data_path, upsample){

  
# Read the raw data
# read_data <- function(base_data_path){
  
#   Inputdata <- read.csv2(base_data_path, sep=",", stringsAsFactors = FALSE)
  
#   target <- Inputdata$target
#   Inputdata <- Inputdata[, -1]
  
#   Inputdata <- apply(Inputdata, 2, as.numeric)
#   Inputdata <- as.data.frame(Inputdata)
  
#   Inputdata = cbind("target"=as.factor(target), Inputdata)
  
#   set.seed(9560)
#   Inputdata <- ROSE(target~., data = Inputdata)$data

#   Inputdata <- as.data.frame(Inputdata)
#   target <- Inputdata$target
#   target <- make.names(target)
#   Inputdata$target <- as.factor(target)
  
#   return(Inputdata)
# }

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

train_data <- read_data(base_data_path, upsample)


# Create training and test data
# set.seed(555)
test_data <- read.csv2(file=file.path(dirname(base_data_path), "debrujin_test_data.csv"), sep=",", stringsAsFactors = FALSE)

target <- test_data$target
test_data <- test_data[, -1]

test_data <- apply(test_data, 2, as.numeric)
test_data <- as.data.frame(test_data)
test_data <- cbind("target"=as.factor(target), test_data)
test_data$target <- as.factor(make.names(target))

colnames(test_data) <- colnames(train_data)


print("Data read-in successfully")
print(paste("Rows:", nrow(train_data), " Cols:", ncol(train_data)))
out = list("train_data" = train_data, "test_data" = test_data)


return(out)
}


