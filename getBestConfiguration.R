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
f <- read.table(file = "SVMORI.txt", header = TRUE, fill = TRUE)

#BigData Application Performance Pridiction and Cluster Recommendation
#Date :05.05.19
library(ggplot2)
library(caret)


generatePlotBestConfig <- function(datafile, title, color, ds){
  
  f <- read.table(file = datafile, header = TRUE, fill = TRUE)
  f <- na.omit(f)
  f <- filter(f, f$DataSize==ds)
  
  min_val <- min(f$Predictions)
  min_ob <- filter(f, f$Prediction==min_val) %>% head(1)
  
  max_val <- max(f$Predictions)
  max_ob <- filter(f, f$Prediction==max_val) %>% head(1)
  
    p<-ggplot(f, aes(x=seq(1:nrow(f)),y=f$Predictions, colour=factor(f[[color]]))) + geom_point()+labs(x="Observation",y="Time(s)", color=color)+ggtitle(title)+
      geom_text(aes(70, min_val, label=min_ob$Numex))+
      theme(plot.title = element_text(size = 12, face = "bold"), axis.text.y=element_text(size=11, face = "bold"),
            axis.title=element_text(size=12,face="bold"), axis.text.x = element_text(size = 11, face = "bold", angle = 0, hjust = 1))
  return(list("p"=p, "minob"=min_ob, "maxob"=max_ob))
}

p <- generatePlotBestConfig("SVMORI.txt", "Title", "DataSize", 39)

p$minob
p$maxob
p$p
