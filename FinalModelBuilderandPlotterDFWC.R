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
  if(datafile == "linear.report"){
    allDataOriginal <- filter(allDataOriginal, Duration.s. <= 100)
  }else if(datafile == "lr.report"){
    allDataOriginal <- filter(allDataOriginal, Duration.s. <= 500)
  }else if(datafile == "rf.report"){
    allDataOriginal <- filter(allDataOriginal, Duration.s. <= 200)
  }else if(datafile == "kmeans.report"){
    allDataOriginal <- filter(allDataOriginal, Duration.s. <= 200)
  }
 
  
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
  
  
  data$predict <- predict(model,data)
  # plot <- ggplot(data) +
  #   geom_line(aes(seq(1:nrow(data)), Duration.s., color='red'))+
  #   geom_line(aes(seq(1:nrow(data)), predict(model, data), color='green'))+
  plot <- ggplot(data, aes(Duration.s., predict)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    ggtitle(title)+
    xlab(xlabel)+
    ylab(ylabel)+
    scale_color_identity(name = "Model fit",
                         breaks = c("red", "green"),
                         labels = c("Actual", "Predicted"),
                         guide = "legend")+
    theme(plot.title = element_text(size = 12, face = "bold"), axis.text.y=element_text(size=11, face = "bold"),
          axis.title=element_text(size=12,face="bold"), axis.text.x = element_text(size = 11, face = "bold", angle = 0, hjust = 1), legend.text = element_text(size=11, face = "bold"), legend.title = element_blank())
  return (list("plot"=plot, "data"=data))
}




svm <- generateModelPlotOnTest("svm.report", "Support Vector", "Predicted", "Actual")
bayes <- generateModelPlotOnTest("bayes.report", "Naive Bayes", "Predicted", "Actual")

lr <- generateModelPlotOnTest("lr.report", "Logistic Regression", "Predicted", "Actual")
linear <- generateModelPlotOnTest("linear.report", "Linear Regression", "Predicted", "Actual")

kmeans <- generateModelPlotOnTest("kmeans.report", "K-Means", "Predicted", "Actual")
rf <- generateModelPlotOnTest("rf.report", "Random Forest", "Predicted", "Actual")



ggarrange(svm$plot, bayes$plot, nrow=1, ncol = 2, common.legend = TRUE, legend = "bottom")
ggarrange(lr$plot, linear$plot, nrow=1, ncol = 2, common.legend = TRUE, legend = "bottom")
ggarrange(kmeans$plot, rf$plot, nrow=1, ncol = 2, common.legend = TRUE, legend = "bottom")


