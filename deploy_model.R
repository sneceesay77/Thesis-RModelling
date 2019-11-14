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
load("RFGridModelForSVM.RData")
load("SVMGridModelForSVM.RData")

#* @post /predict
predict.default.rate <- function(
  DataSizeGB
  ,NumEx
  , ExCore
  , ExMem
  ,LevelPar
) {
  data <- list(
    DataSizeGB=as.numeric(DataSizeGB)
    ,NumEx=as.numeric(NumEx)
    , ExCore=as.numeric(ExCore)
    , ExMem=as.numeric(ExMem)
    ,LevelPar=as.numeric(LevelPar)
  )
  rfpred <- predict(rf.tune.grid, data)
  svmpred <- predict(svm.tune.grid, data)
  return(list("rf"=rfpred, "svm"=svmpred))
}
