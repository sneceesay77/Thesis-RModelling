library(ggplot2)
library(dplyr)
library(gridExtra)
library(caret)
library(grid)
library(ggpubr)
library(e1071)
library(randomForest)
library(caTools)
library(jsonlite)
library(mongolite)



#* @post /predict
predict.default.rate <- function(DataSizeGB ,NumEx,ExCore,ExMem,LevelPar,App) {
  data <- list(
    DataSizeGB=as.numeric(DataSizeGB)
    ,NumEx=as.numeric(NumEx)
    , ExCore=as.numeric(ExCore)
    , ExMem=as.numeric(ExMem)
    ,LevelPar=as.numeric(LevelPar)
  )
  if(App=="SVM"){
    print("Predicting SVM")
    load("RFGridModelForSVM.RData")
    load("SVMGridModelForSVM.RData")
    rfpred <- predict(rf.tune.grid, data)
    svmpred <- predict(svm.tune.grid, data)
    return(list("rf"=rfpred, "svm"=svmpred))
  }else if(App=="BAYES"){
    print("Predicting BAYES")
    load("RFGridModelForBAYES.RData")
    load("SVMGridModelForBAYES.RData")
    rfpred <- predict(rf.tune.grid.bayes, data)
    svmpred <- predict(svm.tune.grid.bayes, data)
    return(list("rf"=rfpred, "svm"=svmpred))
  }else if(App=="LINEAR"){
    print("Predicting LINEAR REGRESSION")
    load("RFGridModelForLINEAR.RData")
    load("SVMGridModelForLINEAR.RData")
    rfpred <- predict(rf.tune.grid.linear, data)
    svmpred <- predict(svm.tune.grid.linear, data)
    return(list("rf"=rfpred, "svm"=svmpred))
  }else if(App=="KMEANS"){
    print("Predicting KMEANS")
    load("RFGridModelForKMEANS.RData")
    load("SVMGridModelForKMEANS.RData")
    rfpred <- predict(rf.tune.grid.kmeans, data)
    svmpred <- predict(svm.tune.grid.kmeans, data)
    return(list("rf"=rfpred, "svm"=svmpred))
  }else if(App=="RF"){
    print("Predicting RF")
    load("RFGridModelForRF.RData")
    load("SVMGridModelForRF.RData")
    rfpred <- predict(rf.tune.grid.rf, data)
    svmpred <- predict(svm.tune.grid.rf, data)
    return(list("rf"=rfpred, "svm"=svmpred))
  }else if(App=="LR"){
    print("Predicting LR")
    load("RFGridModelForLR.RData")
    load("SVMGridModelForLR.RData")
    rfpred <- predict(rf.tune.grid.lr, data)
    svmpred <- predict(svm.tune.grid.lr, data)
    return(list("rf"=rfpred, "svm"=svmpred))
  }
}


#* @post /best
bestConfig <- function(App, DataSizeGB){
  if(App=="SVM"){
    f <- read.table(file = "SVM.txt", header = TRUE, fill=TRUE)
    f <- filter(f, f$DataSize==DataSizeGB)
    min_val <- min(f$Predictions)
    min_ob <- filter(f, f$Prediction==min_val) %>% head(1)
    max_val <- max(f$Predictions)
    max_ob <- filter(f, f$Prediction==max_val) %>% head(1)
    return(list("Best Config"=min_ob, "Worst Config"=max_ob))
  }else if(App=="RF"){
    f <- read.table(file = "RF.txt", header = TRUE, fill=TRUE)
    f <- filter(f, f$DataSize==DataSizeGB)
    min_val <- min(f$Predictions)
    min_ob <- filter(f, f$Prediction==min_val) %>% head(1)
    max_val <- max(f$Predictions)
    max_ob <- filter(f, f$Prediction==max_val) %>% head(1)
    return(list("Best Config"=min_ob, "Worst Config"=max_ob))
  }else if(App=="LR"){
    f <- read.table(file = "LR.txt", header = TRUE, fill=TRUE)
    f <- filter(f, f$DataSize==DataSizeGB)
    min_val <- min(f$Predictions)
    min_ob <- filter(f, f$Prediction==min_val) %>% head(1)
    max_val <- max(f$Predictions)
    max_ob <- filter(f, f$Prediction==max_val) %>% head(1)
    return(list("Best Config"=min_ob, "Worst Config"=max_ob)) 
  }else if(App=="KMEANS"){
    f <- read.table(file = "KMEANS.txt", header = TRUE, fill=TRUE)
    f <- filter(f, f$DataSize==DataSizeGB)
    min_val <- min(f$Predictions)
    min_ob <- filter(f, f$Prediction==min_val) %>% head(1)
    max_val <- max(f$Predictions)
    max_ob <- filter(f, f$Prediction==max_val) %>% head(1)
    return(list("Best Config"=min_ob, "Worst Config"=max_ob))
  }else if(App=="BAYES"){
    f <- read.table(file = "BAYES.txt", header = TRUE, fill=TRUE)
    f <- filter(f, f$DataSize==DataSizeGB)
    min_val <- min(f$Predictions)
    min_ob <- filter(f, f$Prediction==min_val) %>% head(1)
    max_val <- max(f$Predictions)
    max_ob <- filter(f, f$Prediction==max_val) %>% head(1)
    return(list("Best Config"=min_ob, "Worst Config"=max_ob))
  }else if(App=="LINEAR"){
   f <- read.table(file = "LINEAR-REG.txt", header = TRUE, fill=TRUE)
    f <- filter(f, f$DataSize==DataSizeGB)
    min_val <- min(f$Predictions)
    min_ob <- filter(f, f$Prediction==min_val) %>% head(1)
    max_val <- max(f$Predictions)
    max_ob <- filter(f, f$Prediction==max_val) %>% head(1)
    return(list("Best Config"=min_ob, "Worst Config"=max_ob))
  }
}

#* @post /bestec2
generateBestEC2Instance <- function(NumEx, ExMem, ExCore, DataSize){
  totalNodes <- ceiling(NumEx/5)
  exPerNode <- 5
  memPerNode <- exPerNode * ExMem
  corePerNode <- exPerNode * ExCore
  
  totalNodes1 <- ceiling(NumEx/3)
  exPerNode1 <- 3
  memPerNode1 <- exPerNode1 * ExMem
  corePerNode1 <- exPerNode1 * ExCore
  
  # connect to mongodb
  m <- mongo(db = "dfwc", collection  = "aws_pricing")
  # query based on numex, exmem, excore
  result= m$find(paste0('{"Operating System":"Linux", "PricePerUnit": {"$ne":0}, "Unit":{"$eq": "Hrs"}, "vCPU":{"$gte":', corePerNode,'}, "Memory":{"$gte":', memPerNode,'}, "Pre Installed S/W" : "NA"}'), 
                 fields = '{"_id":0, "PricePerUnit" : 1,"Currency" : 1, "Instance Type" : 1,"Instance Family" : 1,"vCPU" : 1,"Memory" : 1,"Storage" : 1,"Operating System" : 1}',
                 limit = 1,
                 sort = '{"PricePerUnit": 1}'
                 
  )
  
  result1= m$find(paste0('{"Operating System":"Linux", "PricePerUnit": {"$ne":0}, "Unit":{"$eq": "Hrs"}, "vCPU":{"$gte":', corePerNode1,'}, "Memory":{"$gte":', memPerNode1,'}, "Pre Installed S/W" : "NA"}'), 
                 fields = '{"_id":0, "PricePerUnit" : 1,"Currency" : 1, "Instance Type" : 1,"Instance Family" : 1,"vCPU" : 1,"Memory" : 1,"Storage" : 1,"Operating System" : 1}',
                 limit = 1,
                 sort = '{"PricePerUnit": 1}'
  )
  
  return(list("BestEC2_1"=result, "BestEC2_2"=result1, "TotalNodes"=totalNodes))
}

generateBestEC2Instance(17, 19, 5, 12)
