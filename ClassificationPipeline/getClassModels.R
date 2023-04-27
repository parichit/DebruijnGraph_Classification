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
  
  # print(temp)
  
  if (!is.null(temp)){
    
    if (is.null(packages[[temp]])){
      packages[[temp]] = mdl
    }
    else{
      packages[[temp]] = c(packages[[temp]], mdl)
    }
    
  }
  
}

# Write the packages to a file
for (i in 1:length(packages)){
  out = paste(names(packages[i]), packages[[i]], sep=",")
  print(out)
  cat(out, file="outfile.txt", sep="\n", append=TRUE)
}
