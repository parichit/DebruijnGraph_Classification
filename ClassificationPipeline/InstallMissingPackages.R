# Modified by Parichit Sharma, please contact hakurban@gmail.com or parishar@iu.edu for any questions
#@IUB, Computer Science Department


install_missing_packages <- function(modelFilePath){
  
  alreadyInstalled = rownames(installed.packages())
  
  base_libs = list()
  missing_libs = c()
  lib_model_list = list()
  regressionModels = c()
  
  successPackages = c()
  failedPackage = c()
  successModels = c()
  failedModels = c()

  options(repos = c(CRAN = "https://cloud.r-project.org/"))
  
  supp_libs = c("doParallel", "tidyverse", "caretEnsemble", "ggplot2", "ggthemes", 
                "xtable", "tictoc", "foreach", "R.utils", 
                "readxl", "stringr", "dplyr", "magrittr", "gridExtra", "yardstick", "ROSE")
  
  # Install additional supporting libs
  for (i in 1:length(supp_libs)){
    if (!(supp_libs[i] %in% alreadyInstalled)){
      install.packages(supp_libs[i], dependencies = c("Imports", "Depends", "Suggests"))
    }
  }
  
  
  modelFile <- file(modelFilePath, open="r")
  contents <- readLines(modelFile)
  close(modelFile)
  
  
  for (line in contents){
    cols = unlist(strsplit(line, ","))
    for (i in 2:length(cols)){
      base_libs[[cols[1]]] = append(base_libs[[cols[1]]], cols[i]) 
      regressionModels = c(regressionModels, trimws(noquote(cols[i])))
    }
  }
  
  
  namesBaseLibs <- unique(names(base_libs))
  regressionModels = unique(regressionModels)
  
  
  # Find libraries that are missing on this system
  for (i in namesBaseLibs){
    # Record missing packages
    if (!(i %in% alreadyInstalled)){
      missing_libs = c(missing_libs, i)
    }
    else{
      successPackages = c(successPackages, i)
      successModels = c(successModels, trimws(noquote(unlist(base_libs[[i]]))))
    }
  }
  
  print(paste("Total classification models: ", length(regressionModels), "Number of required libraries:", length(namesBaseLibs), 
              "Number of missing libraries: ", length(missing_libs)))
  
  
  # Install the missing libraries and
  # count the number of covered packages
  
  if (length(missing_libs) > 1){
    
    print("##########################")
    print("Attempting to install missing packages")
    print("##########################")
    
    
    # install.packages(missing_libs, dependencies = TRUE, Ncpus = 8)
    
    for (i in missing_libs){
      if (!(i %in% alreadyInstalled)){
        install.packages(i, dependencies = c("Imports", "Depends", "Suggests"))
      }
    }
    
    # Check how many packages got installed
    # for the current version of R
    for (i in missing_libs){
      if (!(i %in% alreadyInstalled)){
        failedPackage = c(failedPackage, i)
        failedModels = c(failedModels, unlist(base_libs[[i]]))
      }
      else{
        successPackages = c(successPackages, i)
        successModels = c(successModels, trimws(noquote(unlist(base_libs[[i]]))))
      }
    }
    
    successModels = unique(successModels)
    failedModels = unique(failedModels)
    
    # Determine the number of models covered by installed packages
    print(paste("Installed", length(successPackages), "packages successfullly"))
    print(paste("Installed packages account for", length(successModels), "models"))
    
    print(paste("Failed to install", length(failedPackage), "packages"))
    print(paste("Failed packages account for", length(failedModels), "models"))
    
  }
  
  else{
    print("No missing libraries found")
    print(paste(length(successPackages),  "required libraries already installed"))
  }
  
  print(paste("Total Models:", length(unique(successModels))))
  return(unique(successModels))
}


# install_missing_packages
# install_missing_packages("/Users/schmuck/Library/CloudStorage/OneDrive-IndianaUniversity/PhD/TIMP_Classification/ClassificationPipeline/classModelList.txt")
