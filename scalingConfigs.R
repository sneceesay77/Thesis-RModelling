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
setwd("/home/sc306/Dropbox/SA/ClusterBenchMarking/hadoop/ClusterBenchmarking/dfwc/paper_review/")

plotScaling <- function(datafile, title, xlabel, ylabel, xvalue){
  data <- read.table(file = datafile, header = TRUE)#contains both 128
  
  
  
 
  plot <- ggplot(data, aes(data[[xvalue]], Predictions)) +
    geom_point() +
    ggtitle(title)+
    xlab(xlabel)+
    ylab(ylabel)+
    theme(plot.title = element_text(size = 12, face = "bold"), axis.text.y=element_text(size=11, face = "bold"),
          axis.title=element_text(size=12,face="bold"), axis.text.x = element_text(size = 11, face = "bold", angle = 0, hjust = 1), legend.text = element_text(size=11, face = "bold"), legend.title = element_blank())
  return (list("plot"=plot, "data"=data))
}




svm <- plotScaling("scaleDsData/SVM.txt", "Support Vector", "DataSize(GB)", "Time(s)", "DataSize")
bayes <- plotScaling("scaleDsData/BAYES.txt", "Naive Bayes", "DataSize(GB)", "Time(s)", "DataSize")

lr <- plotScaling("scaleDsData/LR.txt", "Logistic Regression", "DataSize(GB)", "Time(s)", "DataSize")
linear <- plotScaling("scaleDsData/LINEAR.txt", "Linear Regression", "DataSize(GB)", "Time(s)", "DataSize")

kmeans <- plotScaling("scaleDsData/KMEANS.txt", "K-Means", "DataSize(GB)", "Time(s)", "DataSize")
rf <- plotScaling("scaleDsData/RF.txt", "Random Forest", "DataSize(GB)", "Time(s)", "DataSize")

ggarrange(svm$plot, bayes$plot, lr$plot, linear$plot, kmeans$plot, rf$plot, nrow=2, ncol = 3, common.legend = TRUE, legend = "bottom")

svm <- plotScaling("scaleNumEx/SVM.txt", "Support Vector", "Executors #", "Time(s)", "Numex")
bayes <- plotScaling("scaleNumEx/BAYES.txt", "Naive Bayes", "Executors #", "Time(s)", "Numex")

lr <- plotScaling("scaleNumEx/LR.txt", "Logistic Regression", "Executors #", "Time(s)", "Numex")
linear <- plotScaling("scaleNumEx/LINEAR.txt", "Linear Regression", "Executors #", "Time(s)", "Numex")

kmeans <- plotScaling("scaleNumEx/KMEANS.txt", "K-Means", "Executors #", "Time(s)", "Numex")
rf <- plotScaling("scaleNumEx/RF.txt", "Random Forest", "Executors #", "Time(s)", "Numex")

ggarrange(svm$plot, bayes$plot, lr$plot, linear$plot, kmeans$plot, rf$plot, nrow=2, ncol = 3, common.legend = TRUE, legend = "bottom")


svm <- plotScaling("scaleMem/SVM.txt", "Support Vector", "Executor Memory", "Time(s)", "ExMem")
bayes <- plotScaling("scaleMem/BAYES.txt", "Naive Bayes", "Executor Memory", "Time(s)", "ExMem")

lr <- plotScaling("scaleMem/LR.txt", "Logistic Regression", "Executor Memory", "Time(s)", "ExMem")
linear <- plotScaling("scaleMem/LINEAR.txt", "Linear Regression", "Executor Memory", "Time(s)", "ExMem")

kmeans <- plotScaling("scaleMem/KMEANS.txt", "K-Means", "Executor Memory", "Time(s)", "ExMem")
rf <- plotScaling("scaleMem/RF.txt", "Random Forest", "Executor Memory", "Time(s)", "ExMem")

ggarrange(svm$plot, bayes$plot, lr$plot, linear$plot, kmeans$plot, rf$plot, nrow=2, ncol = 3, common.legend = TRUE, legend = "bottom")









ggarrange(lr$plot, linear$plot, nrow=1, ncol = 2, common.legend = TRUE, legend = "bottom")
ggarrange(kmeans$plot, rf$plot, nrow=1, ncol = 2, common.legend = TRUE, legend = "bottom")


