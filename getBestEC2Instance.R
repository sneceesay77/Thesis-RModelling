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

#BigData Application Performance Pridiction and Cluster Recommendation
#Date :05.05.19
library(ggplot2)
library(caret)
library(mongolite)


generatePlotBestConfig <- function(datafile, title, color, ds){
  
  f <- read.table(file = datafile, header = TRUE, fill = TRUE)
  f <- na.omit(f)
  f <- filter(f, f$DataSize==ds)
  
  min_val <- min(f$Predictions)
  min_ob <- filter(f, f$Prediction==min_val) %>% head(1)
  
  max_val <- max(f$Predictions)
  max_ob <- filter(f, f$Prediction==max_val) %>% head(1)
  
  return(list("minob"=min_ob, "maxob"=max_ob))
}

p <- generatePlotBestConfig("SVMORI.txt", "Title", "DataSize", 100)

p$minob

generateBestEC2Instance <- function(NumEx, ExMem, ExCore){
  # connect to mongodb
  m <- mongo(db = "dfwc", collection  = "aws_pricing")
  # query based on numex, exmem, excore
  result= m$find(paste0('{"Operating System":"Linux", "PricePerUnit": {"$ne":0}, "Unit":{"$eq": "Hrs"}, "vCPU":{"$gte":', ExCore,'}, "Memory":{"$gte":', ExMem,'}, "Pre Installed S/W" : "NA"}'), 
                 fields = '{"PricePerUnit" : 1,"Currency" : 1, "Instance Type" : 1,"Instance Family" : 1,"vCPU" : 1,"Memory" : 1,"Storage" : 1,"Operating System" : 1}',
                 limit = 10,
                 sort = '{"PricePerUnit": 1}'
                 
                 )
  return(list("result"=result))
}

bestec2 <- generateBestEC2Instance(p$minob$Numex, p$minob$ExMem, p$minob$ExCore)

bestec2$result


