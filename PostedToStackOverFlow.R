#BigData Application Performance Pridiction and Cluster Recommendation
#Date :05.05.19
library(ggplot2)
library(caret)


generateModelPlotOnTest <- function(datafile, title, xlabel, ylabel){
  allDataOriginal <- read.table(file = datafile, header = TRUE)#contains both 128
  
  
  allDataOriginal$DataSizeMB <- round((allDataOriginal$Input_data_size/1048576))
  allDataOriginal$DataSizeGB <- allDataOriginal$Input_data_size/1073741824
 
  allDataOriginal$ExMem = as.integer(gsub("g", "", allDataOriginal$ExMem))
  allDataOriginal <- filter(allDataOriginal, Duration.s. <= 200)
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
  plot <- ggplot(data, aes(Duration.s., predict)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    ggtitle(title)+
    xlab(xlabel)+
    ylab(ylabel)
  return (list("plot"=plot, "data"=data))
}


kmeans <- generateModelPlotOnTest("https://raw.githubusercontent.com/sneceesay77/Thesis-RModelling/master/kmeans.txt", "K-Means", "Actutual(s)", "Predicted(s)")

kmeans$data
kmeans$plot


