#BigData Application Performance Pridiction and Cluster Recommendation
#Date :05.05.19
library(ggplot2)
library(dplyr)
library(gridExtra)
library(caret)
library(grid)
library(ggpubr)
library(e1071)
library(randomForest)
library(caTools)
#PARALLEL PROCESSING LIBRARY
library(doMC)
registerDoMC(cores = 8)

# dev.off()
options(scipen=999)
setwd("/home/sc306/Dropbox/SA/ClusterBenchMarking/hadoop/ClusterBenchmarking/dfwc/")

generateModelPlotOnTest <- function(datafile, title, xlabel, ylabel){
  allDataOriginal <- read.table(file = datafile, header = TRUE)#contains both 128
  
  
  allDataOriginal$DataSizeMB <- round((allDataOriginal$Input_data_size/1048576))
  allDataOriginal$DataSizeGB <- allDataOriginal$Input_data_size/1073741824
  
  
  #allDataOriginal <- select(allDataOriginal, Duration.s., NumEx, ExCore, ExMem, DataSizeMB, DataSizeGB)
  
  
  #Do some data preprocessing
  
  allDataOriginal$ExMem = as.integer(gsub("g", "", allDataOriginal$ExMem))
  
  set.seed(123)
  #Two third 20 for training and 10 for testing
  split = sample.split(allDataOriginal$Duration.s., SplitRatio = 0.8)
  training_set = subset(allDataOriginal, split == TRUE)
  test_set = subset(allDataOriginal, split == FALSE)
  
  #set.seed(445454)
  data <- (sample_n(test_set, 30))
  
  control <- trainControl(method="repeatedcv", number=10, repeats=3, search = "random")
  model <- train(Duration.s.~ DataSizeGB + NumEx  + ExCore + ExMem, data=training_set[c(-6)], method="svmRadial", trControl=control)
  
  plot <- ggplot(data) +
    geom_line(aes(seq(1:nrow(data)), Duration.s., color='red'))+
    geom_line(aes(seq(1:nrow(data)), predict(model, data), color='green'))+
    ggtitle(title)+
    xlab(xlabel)+
    ylab(ylabel)+
    scale_color_identity(name = "Model fit",
                         breaks = c("red", "green"),
                         labels = c("Actual", "Predicted"),
                         guide = "legend")+
    theme(plot.title = element_text(size = 12, face = "bold"), axis.text.y=element_text(size=11, face = "bold"),
          axis.title=element_text(size=12,face="bold"), axis.text.x = element_text(size = 11, face = "bold", angle = 0, hjust = 1), legend.text = element_text(size=11, face = "bold"), legend.title = element_blank())
  return (plot)
}




svm <- generateModelPlotOnTest("svm.report", "Support Vector", "Observation", "Time(s)")
bayes <- generateModelPlotOnTest("bayes.report", "Naive Bayes", "Observation", "Time(s)")

lr <- generateModelPlotOnTest("lr.report", "Logistic Regression", "Observation", "Time(s)")
linear <- generateModelPlotOnTest("linear.report", "Linear Regression", "Observation", "Time(s)")

kmeans <- generateModelPlotOnTest("kmeans.report", "K-Means", "Observation", "Time(s)")
rf <- generateModelPlotOnTest("rf.report", "Random Forest", "Observation", "Time(s)")



ggarrange(svm, bayes, nrow=1, ncol = 2, common.legend = TRUE, legend = "bottom")
ggarrange(lr, linear, nrow=1, ncol = 2, common.legend = TRUE, legend = "bottom")
ggarrange(kmeans, rf, nrow=1, ncol = 2, common.legend = TRUE, legend = "bottom")




