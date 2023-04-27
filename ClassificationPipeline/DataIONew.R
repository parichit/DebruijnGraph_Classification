require("caret")
require("readxl")
require("stringr")

load_data_new <- function(base_data_path, keep_cols_indices, type_data){
  
  # Read the raw data
  # Format of input data
  # A excel (XLSX) file with multiple sheets
  # In this file, a sheet corresponds to a specific Voltage/Temperature

  read_data <- function(base_data_path, keep_cols_indices, type_data){
    
    sheet_names = excel_sheets(base_data_path)
    Inputdata = data.frame()
    
    for (sh in sheet_names){
      temp <- as.data.frame(read_excel(base_data_path, sheet =sh))
      temp = temp[, keep_cols_indices]
      
      if (type_data == "Temperature"){
        items = strsplit(sh, " ")
        temperature = str_sub(items[[1]][1])
        temp = cbind("Temperature" = rep(temperature, nrow(temp)), temp)
      } else{
        items = strsplit(sh, "V")
        volt = str_sub(items[[1]][1])
        temp = cbind("Volt" = rep(volt, nrow(temp)), temp)
      }
      Inputdata <- rbind(Inputdata, temp)
    }
    
    Inputdata <- as.data.frame(sapply(Inputdata, as.numeric))
    
    return(Inputdata)
  }
  
  # filepath = "/Users/schmuck/Library/CloudStorage/OneDrive-IndianaUniversity/Box Sync/PhD/battery-machine-intelligence/data/EIS_4200to3200mV.xlsx"
  # keep_cols_indices = c(1,2,3)
  # type_data = "Volt"
  # 
  # filepath = "/Users/schmuck/Library/CloudStorage/OneDrive-IndianaUniversity/Box Sync/PhD/battery-machine-intelligence/data/EIS_10to50C_4V.xlsx"
  # keep_cols_indices = c(3,4,5)
  # type_data = "Temperature"
  
  Inputdata = data.frame()
  
  
  # for (sh in sheet_names){
  #   temp <- as.data.frame(read_excel(filepath, sheet =sh))
  #   temp = temp[, keep_cols_indices]
  #   items = strsplit(sh, "V")
  #   volt = str_sub(items[[1]][1])
  #   print(volt)
  #   temp = cbind("Volt" = rep(volt, nrow(temp)), temp)
  #   Inputdata <- rbind(Inputdata, temp)
  #   print(head(Inputdata))
  # }
  
  Inputdata <- read_data(base_data_path, keep_cols_indices, type_data)
  
  # Create training and test data
  set.seed(7105)
  if (type_data == "Temperature"){
    train_indices <- createDataPartition(y = as.factor(Inputdata$Temperature), p = 0.85, list = FALSE)
  }else{
    train_indices <- createDataPartition(y = as.factor(Inputdata$Volt), p = 0.85, list = FALSE)
  }
  
  training_data <- Inputdata[train_indices, ]
  test_data <- Inputdata[-train_indices, ]
  
  print("Data read-in successfully")
  print(paste("Rows:", nrow(Inputdata), " Cols:", ncol(Inputdata)))
  
  out = list("train_data" = training_data, "test_data" = test_data)
  
  return(out)
}