library("caret")

allModels <- modelLookup()
allModels <- allModels[allModels$forClass == TRUE, ]

unique_models <- unique(allModels$model)


# Find the associated package
packages = list()

for(mdl in unique_models){
  
  temp <- getModelInfo(mdl)[[1]][[2]]
  
  if (length(temp)>1) {
    temp = temp[1]  
  }
  
  if (!is.null(temp)){
    
    if (is.null(packages[[temp]])){
      packages[[temp]] = mdl
    }
    else{
      packages[[temp]] = c(packages[[temp]], mdl)
    }
    
  }
  
}

print(length(packages))

remove_pkgs = c("fastAdaboost", "adaptDA",  "CHAID", "deepboost", 
                "elmNN", "extraTrees", "gpls", "logicFS", "FCNN4R", 
                "mxnet", "nodeHarvest", "obliqueRF", "Classification", "rrlda")

for (k in remove_pkgs){
  packages[[k]] <- NULL
}


print(length(packages))

# Write the packages to a file
for (i in 1:length(packages)){

  out = c(names(packages[i]))

  if (length(packages[[i]]) > 1){
      for (i in packages[[i]]){
        temp = paste(temp, i, sep=",")
      }
  }else{
    temp = packages[[i]]
  }

  out = paste(out, temp, sep=",")

  cat(out, "\n", file = "classModelList.txt", append=TRUE)
}



