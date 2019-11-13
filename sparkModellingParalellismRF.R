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
allDataOriginal <- read.table(file = "paralellism.report", header = TRUE)#contains both 128
head(allDataOriginal)

names(allDataOriginal)
#add observation and features and scal it to tenth
for(i in seq(1:nrow(allDataOriginal))){
  if(allDataOriginal$Input_data_size[i] == 8032735291){
    allDataOriginal$observations[i] = 50000000/1000
    allDataOriginal$features[i] = 20
  }else if(allDataOriginal$Input_data_size[i] == 12049140406){
    allDataOriginal$observations[i] = 60000000/1000
    allDataOriginal$features[i] = 20
  }
}

head(allDataOriginal)

allDataOriginal$DataSizeMB <- round((allDataOriginal$Input_data_size/1048576))
allDataOriginal$DataSizeGB <- allDataOriginal$Input_data_size/1073741824


allDataOriginal <- select(allDataOriginal, Duration.s., NumEx, ExCore, ExMem, LevelPar, DataSizeMB, DataSizeGB, LevelPar, CompAlgo, sh.com, spill.com, broa.comp)
allDataOriginal <- filter(allDataOriginal, Duration.s. <= 200)

#Do some data preprocessing

allDataOriginal$ExMem = as.integer(gsub("g", "", allDataOriginal$ExMem))



summary(allDataOriginal)

head(allDataOriginal)
str(allDataOriginal)
#allDataOriginal = filter(allDataOriginal, allDataOriginal$Duration.s. <= 3000)

summary(allDataOriginal)

#allDataOriginal$NumEx = factor(allDataOriginal$NumEx)
#allDataOriginal$ExCore = factor(allDataOriginal$ExCore)
#allDataOriginal$ExMem = factor(allDataOriginal$ExMem)
#allDataOriginal$DriverMem = factor(allDataOriginal$DriverMem)


generatePlot <- function(originalData, filterbyCol, filterByVal, title, color){
  
  #xval = seq(min(filteredData$DataSizeMB), nrow(filteredData), by = (max(filteredData$DataSizeMB)/nrow(filteredData)))
  #p<-ggplot(filteredData, aes(x=filteredData$DataSizeMB, y=filteredData$Duration.s.)) + geom_point()+geom_smooth(method='auto')+labs(x="Data Size (MB)",y="Time(s)")+ggtitle(title)
  p <- '';
  if(missing(color)){
    filteredData = originalData %>%  filter(originalData[[filterbyCol]] == filterByVal) %>% arrange(!!sym(color))
    p<-ggplot(filteredData, aes(x=filteredData$DataSizeMB, y=filteredData$Duration.s.)) + geom_point()+labs(x="Data Size (MB)",y="Time(s)")+ggtitle(title)
  }else{
    f = originalData %>%  filter(originalData[[filterbyCol]] == filterByVal) %>% arrange(!!sym(color)) 
    p<-ggplot(f, aes(x=seq(1:nrow(f)), y=f$Duration.s.,  colour=factor(f[[color]]))) + geom_point()+labs(x="Observation",y="Time(s)", color=color)+ggtitle(title)
    # scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
    # scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) +
    # scale_color_hue(labels = c("1 MB", "313 MB"))
  }
  return(p)
}



p01 <- generatePlot(allDataOriginal, "DataSizeMB", 1, "1MB", "LevelPar")
p02 <- generatePlot(allDataOriginal, "DataSizeMB", 15, "15MB", "LevelPar")
p03 <- generatePlot(allDataOriginal, "DataSizeMB", 76, "76MB", "LevelPar")
p04 <- generatePlot(allDataOriginal, "DataSizeMB", 1908, "2GB", "LevelPar")
p05 <- generatePlot(allDataOriginal, "DataSizeMB", 4006, "4GB", "LevelPar")
p06 <- generatePlot(allDataOriginal, "DataSizeMB", 6867, "6GB", "LevelPar")


#annotate_figure(ggarrange(p01,p02,p03,p05,p06,p07, ncol=2, nrow=3, common.legend = TRUE, legend = "bottom"), top="SVM Algorithm: Scatter Plot of scaling Executors")
annotate_figure(ggarrange(p03,p04,p05,p06, ncol=2, nrow=2, common.legend = TRUE, legend = "bottom"), top="Random Forest Paralellism")

p01 <- generatePlot(allDataOriginal, "LevelPar", 2, "1MB", "LevelPar")
p02 <- generatePlot(allDataOriginal, "LevelPar", 15, "15MB", "LevelPar")
p03 <- generatePlot(allDataOriginal, "LevelPar", 76, "76MB", "LevelPar")
p04 <- generatePlot(allDataOriginal, "LevelPar", 1908, "4GB", "LevelPar")
p05 <- generatePlot(allDataOriginal, "DataSizeMB", 4006, "4GB", "LevelPar")
p06 <- generatePlot(allDataOriginal, "DataSizeMB", 6867, "6GB", "LevelPar")


#annotate_figure(ggarrange(p01,p02,p03,p05,p06,p07, ncol=2, nrow=3, common.legend = TRUE, legend = "bottom"), top="SVM Algorithm: Scatter Plot of scaling Executors")
annotate_figure(ggarrange(p01,p02,P=p03,p04,p05,p06, ncol=2, nrow=3, common.legend = TRUE, legend = "bottom"), top="KMeans Compression Algorithm")

p1 <- generatePlot(allDataOriginal, "DataSizeMB", 7661, "7GB", "sh.com")
p2 <- generatePlot(allDataOriginal, "DataSizeMB", 11491, "11GB", "sh.com") 


annotate_figure(ggarrange(p1,p2, ncol=1, nrow=2, common.legend = TRUE, legend = "bottom"), top="Shuffle Compress")

p3 <- generatePlot(allDataOriginal, "DataSizeMB", 7661, "7GB", "spill.com")
p4 <- generatePlot(allDataOriginal, "DataSizeMB", 11491, "11GB", "spill.com") 
annotate_figure(ggarrange(p3,p4, ncol=1, nrow=2, common.legend = TRUE, legend = "bottom"), top="Spill Compress")

p5 <- generatePlot(allDataOriginal, "DataSizeMB", 7661, "7GB", "broa.comp")
p6 <- generatePlot(allDataOriginal, "DataSizeMB", 11491, "11GB", "broa.comp") 
annotate_figure(ggarrange(p5,p6, ncol=1, nrow=2, common.legend = TRUE, legend = "bottom"), top="Spill Compress")


