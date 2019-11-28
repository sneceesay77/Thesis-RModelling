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
  }else if(App=="LINEAR-REG"){
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
