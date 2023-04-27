path = getwd()
setwd(file.path(path, "RegressionPipeline"))

out_dir = file.path(path, "Results")
base_path = path 


args = commandArgs(trailingOnly=TRUE)

if (length(args) < 1){
  print("Please mention which prediction you want to make?")
  print("Choices: real, imaginary")
  stop(exiting)
}

type_pred = args[1]
already_running = args[2]

# type_pred = "real"
# already_running = "no"

if ( (dir.exists(out_dir)) && (already_running == "no") ){
  time_stamp = format(Sys.time(), "%m_%d_%H-%m-%S")
  new_name = paste(out_dir, "_old_", time_stamp, sep="")
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


availableModels = install_missing_packages(file.path("modelList.txt"))


print("###################################")
print("2 READ/COMBINE DATA")
print("###################################")


source("DataIO.R")

out <- load_data(file.path(base_path, "data", "data.xlsx"))
train_data <- out[[1]]
test_data <- out[[2]]


source("runModels.R")

# Set parameters
time_limit = 1000
number <- 5
repeats <- 5

if (type_pred ==  "real"){
  
  print("###################################")
  print("3 RUN MODELS FOR PREDICTING REAL COMPONENT OF IMPEDANCE")
  print("###################################")
  
  train_out_file = "train_Zreal_results.csv"
  test_out_file = "test_Zreal_results.csv"
  stat_file = "Zreal_stat.csv"
  plot_file = "Zreal.png"
  
  trainFilePath = file.path(out_dir, train_out_file)
  testFilePath = file.path(out_dir, test_out_file)
  
  runModels(availableModels, train_data, test_data, time_limit, 
            number, repeats, "real", out_dir, train_out_file, test_out_file, stat_file)
  
  trainFilePath = file.path(out_dir, train_out_file)
  testFilePath = file.path(out_dir, test_out_file)
  
  if (file.exists(trainFilePath) && (file.exists(testFilePath))){
    trainData <- read.csv2(file = trainFilePath, stringsAsFactors = FALSE, sep=",")
    testData <- read.csv2(file = testFilePath, stringsAsFactors = FALSE, sep=",")
    
    # plot top 30 models
    drawPlots(trainData, testData, "real", out_dir, plot_file)
    
    # plot all models
    drawAllPlots(trainData, testData, "real", out_dir, plot_file)
    
  } else{
    print("Results could not be located! Please check if the run was completed.")
  }
}

# type_pred = "imaginary"

if (type_pred ==  "imaginary"){
  
  print("###################################")
  print("3 RUN MODELS FOR PREDICTING IMAGINARY COMPONENT OF IMPEDANCE")
  print("###################################")
  
  train_out_file = "train_Zimag_results.csv"
  test_out_file = "test_Zimag_results.csv"
  stat_file = "Zimag_stat.csv"
  plot_file = "Zimag.png"
  
  trainFilePath = file.path(out_dir, train_out_file)
  testFilePath = file.path(out_dir, test_out_file)
  
  runModels(availableModels, train_data, test_data, time_limit, 
            number, repeats, "imag", out_dir, train_out_file, test_out_file, stat_file)
  
  trainFilePath = file.path(out_dir, train_out_file)
  testFilePath = file.path(out_dir, test_out_file)
  
  if (file.exists(trainFilePath) && (file.exists(testFilePath))){
    trainData <- read.csv2(file = trainFilePath, stringsAsFactors = FALSE, sep=",")
    testData <- read.csv2(file = testFilePath, stringsAsFactors = FALSE, sep=",")
    
    # plot top 30 models
    drawPlots(trainData, testData, "imag", out_dir, plot_file)
    
    # plot all models
    drawAllPlots(trainData, testData, "imag", out_dir, plot_file)
    
  }else{
    print("Results could not be located! Please check if the run was completed.")
  }
}


print(paste("Completed execution on:", Sys.time()))

