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
    DataSizeGB=DataSizeGB
    ,NumEx=NumEx
    , ExCore=ExCore
    , ExMem=ExMem
    ,LevelPar=LevelPar
  )
  rfpred <- predict(rf.tune.grid, data)
  svmpred <- predict(svm.tune.grid, data)
  return(list("rf"=rfpred, "svm"=svmpred))
}