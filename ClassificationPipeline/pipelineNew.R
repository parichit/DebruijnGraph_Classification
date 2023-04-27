path = getwd()
setwd(path)
base_script_path = path

base_path = dirname(base_script_path)
out_dir = file.path(base_path, "ResultsNew")


args = commandArgs(trailingOnly=TRUE)

if (length(args) < 1){
  print("Please mention which prediction you want to make?")
  print("Choices: real, imaginary")
  stop(exiting)
}

type_pred = args[1]
already_running = args[2]

# type_pred = "realTemp"
# type_pred = "imagTemp"
# already_running = "no"


if ( (dir.exists(out_dir)) && (already_running == "no") ){
  time_stamp = format(Sys.time(), "%m_%d_%H-%m-%S")
  new_name = paste(out_dir, "_old_", time_stamp)
  file.rename(out_dir, new_name)
  dir.create(out_dir)
} else if ( !(dir.exists(out_dir)) && (already_running == "no") ){
  dir.create(out_dir)
}



print(paste("Started execution on:", Sys.time()))


print("###################################")
print("1 Install packages")
print("###################################")


source("InstallMissingPackages.R")
source("visualizeTopResults.R")
source("visualizeAllResults.R")


availableModels = install_missing_packages(file.path(base_script_path, "modelList.txt"))


print("###################################")
print("2 READ/COMBINE DATA")
print("###################################")


source("DataIONew.R")

out <- load_data_new(file.path(base_path, "data", "EIS_10to50C_4V.xlsx"), c(3,4,5), "Temperature")
temp_train_data <- out[[1]]
temp_test_data <- out[[2]]

out <- load_data_new(file.path(base_path, "data", "EIS_4200to3200mV.xlsx"), c(1,2,3), "Volt")
volt_train_data <- out[[1]]
volt_test_data <- out[[2]]


source("runModels.R")

# Set parameters
time_limit = 1000
number <- 5
repeats <- 5

if (type_pred ==  "realTemp"){
  
  print("###################################")
  print("3 RUN MODELS FOR PREDICTING REAL COMPONENT OF IMPEDANCE")
  print("###################################")
  
  train_out_file = "Temperature_train_Zreal_results.csv"
  test_out_file = "Temperature_test_Zreal_results.csv"
  stat_file = "Temperature_Zreal_stat.csv"
  plot_file = "Temperature_Zreal.png"
  
  runModels(availableModels, temp_train_data[, c(1,2,3)], temp_test_data[, c(1,2,3)], time_limit, 
            number, repeats, "real", out_dir, train_out_file, test_out_file, stat_file)
  
  trainFilePath = file.path(out_dir, train_out_file)
  testFilePath = file.path(out_dir, test_out_file)
  
  if (file.exists(trainFilePath) && (file.exists(testFilePath))){
    trainData <- read.csv2(file = trainFilePath, stringsAsFactors = FALSE, sep=",")
    testData <- read.csv2(file = testFilePath, stringsAsFactors = FALSE, sep=",")
    
    # plot top 30 models
    drawPlots(trainData, testData, "real", out_dir, plot_file)
    
    # plot all models
    drawAllPlots(trainData, testData, "_real_Temperature_", out_dir, plot_file)
    
  } else{
    print("Results could not be located! Please check if the run was completed.")
  }
} else if(type_pred == "realVolt"){
  
  print("###################################")
  print("3 RUN MODELS FOR PREDICTING REAL COMPONENT OF IMPEDANCE")
  print("###################################")
  
  train_out_file = "Volt_train_Zreal_results.csv"
  test_out_file = "Volt_test_Zreal_results.csv"
  stat_file = "Volt_Zreal_stat.csv"
  plot_file = "Volt_Zreal.png"
  all_plot_file = "All_Volt_Zreal.png"
  
  runModels(availableModels, volt_train_data[, c(1,2,3)], volt_test_data[, c(1,2,3)], time_limit, 
            number, repeats, "real", out_dir, train_out_file, test_out_file, stat_file)
  
  trainFilePath = file.path(out_dir, train_out_file)
  testFilePath = file.path(out_dir, test_out_file)
  
  if (file.exists(trainFilePath) && (file.exists(testFilePath))){
    trainData <- read.csv2(file = trainFilePath, stringsAsFactors = FALSE, sep=",")
    testData <- read.csv2(file = testFilePath, stringsAsFactors = FALSE, sep=",")
    
    # plot top 30 models
    drawPlots(trainData, testData, "real", out_dir, plot_file)
    
    # plot all models
    drawAllPlots(trainData, testData, "_real_Volt_", out_dir, plot_file)
    
  } else{
    print("Results could not be located! Please check if the run was completed.")
  }
  
} else if (type_pred ==  "imagVolt"){
  
  print("###################################")
  print("3 RUN MODELS FOR PREDICTING IMAGINARY COMPONENT OF IMPEDANCE")
  print("###################################")
  
  train_out_file = "Volt_train_Zimag_results.csv"
  test_out_file = "Volt_test_Zimag_results.csv"
  stat_file = "Volt_Zimag_stat.csv"
  plot_file = "Volt_Zimag.png"
  all_plot_file = "All_Volt_Zreal.png"
  
  runModels(availableModels, volt_train_data[, c(1,2,4)], volt_test_data[, c(1,2,4)], time_limit, 
            number, repeats, "imag", out_dir, train_out_file, test_out_file, stat_file)
  
  trainFilePath = file.path(out_dir, train_out_file)
  testFilePath = file.path(out_dir, test_out_file)
  
  if (file.exists(trainFilePath) && (file.exists(testFilePath))){
    trainData <- read.csv2(file = trainFilePath, stringsAsFactors = FALSE, sep=",")
    testData <- read.csv2(file = testFilePath, stringsAsFactors = FALSE, sep=",")
    
    # plot top 30 models
    drawPlots(trainData, testData, "imag", out_dir, plot_file)
    
    # plot all models
    drawAllPlots(trainData, testData, "_imag_Volt_", out_dir, plot_file)
    
  }else{
    print("Results could not be located! Please check if the run was completed.")
  }
} else if (type_pred ==  "imagTemp"){
  
  print("###################################")
  print("3 RUN MODELS FOR PREDICTING IMAGINARY COMPONENT OF IMPEDANCE")
  print("###################################")
  
  train_out_file = "Temperature_train_Zimag_results.csv"
  test_out_file = "Temperature_test_Zimag_results.csv"
  stat_file = "Temperature_Zimag_stat.csv"
  plot_file = "Temperature_Zimag.png"
  all_plot_file = "All_Temperature_Zreal.png"
  
  runModels(availableModels, temp_train_data[, c(1,2,4)], temp_test_data[, c(1,2,4)], time_limit, 
            number, repeats, "imag", out_dir, train_out_file, test_out_file, stat_file)
  
  trainFilePath = file.path(out_dir, train_out_file)
  testFilePath = file.path(out_dir, test_out_file)
  
  if (file.exists(trainFilePath) && (file.exists(testFilePath))){
    trainData <- read.csv2(file = trainFilePath, stringsAsFactors = FALSE, sep=",")
    testData <- read.csv2(file = testFilePath, stringsAsFactors = FALSE, sep=",")
    
    # plot top 30 models
    drawPlots(trainData, testData, "imag", out_dir, plot_file)
    
    # plot all models
    drawAllPlots(trainData, testData, "_imag_Temperature_", out_dir, plot_file)
    
  }else{
    print("Results could not be located! Please check if the run was completed.")
  }
}


print(paste("Completed execution on:", Sys.time()))

