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
allDataOriginal <- read.table(file = "svm.report", header = TRUE)#contains both 128
head(allDataOriginal)
  
names(allDataOriginal)
#add observation and features and scal it to tenth
for(i in seq(1:nrow(allDataOriginal))){
  if(allDataOriginal$Input_data_size[i] == 200302808){
    allDataOriginal$observations[i] = 5000/1000
    allDataOriginal$features[i] = 10000/1000
  }else if(allDataOriginal$Input_data_size[i] == 288361800){
    allDataOriginal$observations[i] = 6000/1000
    allDataOriginal$features[i] = 15000/1000
  }else if(allDataOriginal$Input_data_size[i] == 512482200){
    allDataOriginal$observations[i] = 30000/1000
    allDataOriginal$features[i] = 30000/1000
  }else if(allDataOriginal$Input_data_size[i] == 648543608){
    allDataOriginal$observations[i] = 40000/1000
    allDataOriginal$features[i] = 40000/1000
  }else if(allDataOriginal$Input_data_size[i] == 800602600){
    allDataOriginal$observations[i] = 50000/1000
    allDataOriginal$features[i] = 50000/1000
  }else if(allDataOriginal$Input_data_size[i] == 3201204600){
    allDataOriginal$observations[i] = 60000/1000
    allDataOriginal$features[i] = 60000/1000
  }else if(allDataOriginal$Input_data_size[i] == 3201204600){
    allDataOriginal$observations[i] = 60000/1000
    allDataOriginal$features[i] = 60000/1000
  }else if(allDataOriginal$Input_data_size[i] == 12802408600){
    allDataOriginal$observations[i] = 120000/1000
    allDataOriginal$features[i] = 120000/1000
  }
}

head(allDataOriginal)

allDataOriginal$DataSizeMB <- round((allDataOriginal$Input_data_size/1048576))
allDataOriginal$DataSizeGB <- allDataOriginal$Input_data_size/1073741824


allDataOriginal <- select(allDataOriginal, Duration.s., NumEx, ExCore, ExMem, LevelPar, DataSizeMB, DataSizeGB, observations, features)
allDataOriginal <- filter(allDataOriginal, Duration.s. <= 700)

#Do some data preprocessing

allDataOriginal$ExMem = as.integer(gsub("g", "", allDataOriginal$ExMem))



summary(allDataOriginal)

head(allDataOriginal)
str(allDataOriginal)
#allDataOriginal = filter(allDataOriginal, allDataOriginal$Duration.s. <= 3000)

filter(allDataOriginal, NumEx==4 & ExMem==2 & ExCore==2, LevelPar==8)

summary(allDataOriginal)

#allDataOriginal$NumEx = factor(allDataOriginal$NumEx)
#allDataOriginal$ExCore = factor(allDataOriginal$ExCore)
#allDataOriginal$ExMem = factor(allDataOriginal$ExMem)
#allDataOriginal$DriverMem = factor(allDataOriginal$DriverMem)


geom_boxplot(allDataOriginal)


generatePlot <- function(originalData, filterbyCol, filterByVal, title, color){
  
  #xval = seq(min(filteredData$DataSizeMB), nrow(filteredData), by = (max(filteredData$DataSizeMB)/nrow(filteredData)))
  #p<-ggplot(filteredData, aes(x=filteredData$DataSizeMB, y=filteredData$Duration.s.)) + geom_point()+geom_smooth(method='auto')+labs(x="Data Size (MB)",y="Time(s)")+ggtitle(title)
  p <- '';
  if(missing(color)){
    filteredData = originalData %>%  filter(originalData[[filterbyCol]] == filterByVal) %>% arrange(!!sym(color))
    p<-ggplot(filteredData, aes(x=filteredData$DataSizeMB, y=filteredData$Duration.s.)) + geom_point()+labs(x="Data Size (MB)",y="Time(s)")+ggtitle(title)+
      theme(plot.title = element_text(size = 12, face = "bold"), axis.text.y=element_text(size=11, face = "bold"),
            axis.title=element_text(size=12,face="bold"), axis.text.x = element_text(size = 11, face = "bold", angle = 0, hjust = 1))
  }else{
    f = originalData %>%  filter(originalData[[filterbyCol]] == filterByVal) %>% arrange(!!sym(color)) 
    p<-ggplot(f, aes(x=seq(1:nrow(f)), y=f$Duration.s.,  colour=factor(f[[color]]))) + geom_point()+labs(x="Observation",y="Time(s)", color=color)+ggtitle(title)+
      theme(plot.title = element_text(size = 12, face = "bold"), axis.text.y=element_text(size=11, face = "bold"),
            axis.title=element_text(size=12,face="bold"), axis.text.x = element_text(size = 11, face = "bold", angle = 0, hjust = 1))
  }
  return(p)
}

generateDistributionPlot <- function(originalData, filterbyCol, filterByVal, title, color){
  f <- originalData %>%  filter(originalData[[filterbyCol]] == filterByVal) %>% arrange(!!sym(color))
  p<-ggplot(f, aes(x=f$Duration.s.,  colour=factor(f[[color]]))) + geom_histogram(binwidth = 50, color="black", fill="white") + labs(x="Time(s)", y="Frequency", color=color)+ggtitle(title)+
    #geom_vline(aes(xintercept=mean(f$Duration.s., na.rm=T)), color="red", linetype="dashed", size=1)+
    geom_vline(aes(xintercept=median(f$Duration.s., na.rm=T)), color="blue", linetype="dashed", size=1)+
    theme(plot.title = element_text(size = 12, face = "bold"), axis.text.y=element_text(size=11, face = "bold"),
          axis.title=element_text(size=12,face="bold"), axis.text.x = element_text(size = 11, face = "bold", angle = 0, hjust = 1))
  return(p)
}

generateBoxPlot <- function(originalData, filterbyCol, col2,val2,title){
  a = originalData %>%  filter(originalData$Duration.s. <= 200)
  f = a %>%  filter(a[[filterbyCol]] %in% c(4,8,16,24,32) & a[[col2]] %in% c(val2))
  p <- ggplot(f, aes(x=as.factor(f$NumEx), y=f$Duration.s.))+ggtitle(title)+
    geom_boxplot() +
    labs(x="Executors", y="Time(s)")+
    theme(plot.title = element_text(size = 12, face = "bold"), axis.text.y=element_text(size=11, face = "bold"),
          axis.title=element_text(size=12,face="bold"), axis.text.x = element_text(size = 11, face = "bold", angle = 0, hjust = 1))
  return(p)
}



box01 <- generateBoxPlot(allDataOriginal, "NumEx", "DataSizeMB", 191, "191 MB")
box02 <- generateBoxPlot(allDataOriginal, "NumEx", "DataSizeMB", 618,"3 GB")
box03 <- generateBoxPlot(allDataOriginal, "NumEx", "DataSizeMB", 12209, "12 GB")

#Distribution plot
ne1 <- generateDistributionPlot(allDataOriginal, "DataSizeMB", 191, "191MB", "NumEx")
ne2 <- generateDistributionPlot(allDataOriginal, "DataSizeMB", 275, "275MB", "NumEx")  
ne3 <- generateDistributionPlot(allDataOriginal, "DataSizeMB", 489, "489MB", "NumEx")
ne4 <- generateDistributionPlot(allDataOriginal, "DataSizeMB", 618, "618MB", "NumEx")
ne5 <- generateDistributionPlot(allDataOriginal, "DataSizeMB", 764, "764MB", "NumEx")
ne6 <- generateDistributionPlot(allDataOriginal, "DataSizeMB", 3053, "3GB", "NumEx")
ne7 <- generateDistributionPlot(allDataOriginal, "DataSizeMB", 12209, "12GB", "NumEx")
annotate_figure(ggarrange(ne1,ne3,ne5,ne7, ncol=2, nrow=2, common.legend = TRUE, legend = "bottom"), top="SVM Algorithm: Scatter Plot of scaling Executors")

ne8<-generateDistributionPlot(allDataOriginal, "NumEx", 4, "4 Executors", "DataSizeMB")
ne9<-generateDistributionPlot(allDataOriginal, "NumEx", 8, "8 Executors", "DataSizeMB")
ne10<-generateDistributionPlot(allDataOriginal, "NumEx", 16, "16 Executors", "DataSizeMB")
ne11<-generateDistributionPlot(allDataOriginal, "NumEx", 24, "24 Executors", "DataSizeMB")
ne12<-generateDistributionPlot(allDataOriginal, "NumEx", 32, "32 Executors", "DataSizeMB")
annotate_figure(ggarrange(ne8,ne9,ne10,ne12,box01,box03, ncol=2, nrow=3, common.legend = TRUE, legend = "bottom"))

annotate_figure(ggarrange(box01,box03, ncol=2, nrow=1, common.legend = TRUE, legend = "bottom"))

p01 <- generatePlot(allDataOriginal, "DataSizeMB", 191, "191MB", "NumEx")
p02 <- generatePlot(allDataOriginal, "DataSizeMB", 275, "275MB", "NumEx")  
p03 <- generatePlot(allDataOriginal, "DataSizeMB", 489, "489MB", "NumEx")
p04 <- generatePlot(allDataOriginal, "DataSizeMB", 618, "618MB", "NumEx")
p05 <- generatePlot(allDataOriginal, "DataSizeMB", 764, "764MB", "NumEx")
p06 <- generatePlot(allDataOriginal, "DataSizeMB", 3053, "3GB", "NumEx")
p07 <- generatePlot(allDataOriginal, "DataSizeMB", 12209, "12GB", "NumEx")

#annotate_figure(ggarrange(p01,p02,p03,p05,p06,p07, ncol=2, nrow=3, common.legend = TRUE, legend = "bottom"), top="SVM Algorithm: Scatter Plot of scaling Executors")
annotate_figure(ggarrange(p01,p04,p06,p07, ncol=2, nrow=2, common.legend = TRUE, legend = "bottom"), top="SVM Algorithm: Scatter Plot of scaling Executors")

p1<-generatePlot(allDataOriginal, "NumEx", 4, "4 Executors", "DataSizeMB")
p2<-generatePlot(allDataOriginal, "NumEx", 8, "8 Executors", "DataSizeMB")
p3<-generatePlot(allDataOriginal, "NumEx", 16, "16 Executors", "DataSizeMB")
p4<-generatePlot(allDataOriginal, "NumEx", 24, "24 Executors", "DataSizeMB")
p5<-generatePlot(allDataOriginal, "NumEx", 32, "32 Executors", "DataSizeMB")

annotate_figure(ggarrange(p1,p2,p3,p4,p5, ncol=2, nrow=3, common.legend = TRUE, legend = "bottom"), top="Scaling Dataset for each Executor")

exc1<-generateDistributionPlot(allDataOriginal, "ExCore", 2, "2 Cores", "DataSizeMB")
exc2<-generateDistributionPlot(allDataOriginal, "ExCore", 4, "4 Cores", "DataSizeMB")
exc3<-generateDistributionPlot(allDataOriginal, "ExCore", 6, "6 Cores", "DataSizeMB")
exc4<-generateDistributionPlot(allDataOriginal, "ExCore", 8, "8 Cores", "DataSizeMB")
annotate_figure(ggarrange(exc1,exc2,exc3,exc4, ncol=2, nrow=2, common.legend = TRUE, legend = "bottom"))

p6<-generatePlot(allDataOriginal, "ExCore", 2, "2 Cores", "DataSizeMB")
p7<-generatePlot(allDataOriginal, "ExCore", 4, "4 Cores", "DataSizeMB")
p8<-generatePlot(allDataOriginal, "ExCore", 6, "6 Cores", "DataSizeMB")
p9<-generatePlot(allDataOriginal, "ExCore", 8, "8 Cores", "DataSizeMB")


annotate_figure(ggarrange(p6,p7,p8,p9, ncol=2, nrow=2, common.legend = TRUE, legend = "bottom"), top="Scaling Executor Cores")

ph <- generatePlot(allDataOriginal, "DataSizeMB", 191, "191MB", "ExCore")
pi <- generatePlot(allDataOriginal, "DataSizeMB", 275, "275MB", "ExCore")
pj <- generatePlot(allDataOriginal, "DataSizeMB", 489, "489MB", "ExCore")
pk <- generatePlot(allDataOriginal, "DataSizeMB", 618, "618MB", "ExCore")
pl <- generatePlot(allDataOriginal, "DataSizeMB", 764, "764MB", "ExCore")
pm <- generatePlot(allDataOriginal, "DataSizeMB", 3053, "3GB", "ExCore")
pn <- generatePlot(allDataOriginal, "DataSizeMB", 12209, "12GB", "ExCore")

annotate_figure(ggarrange(ph,pi,pj,pk,pl,pm,pn, ncol=2, nrow=4, common.legend = TRUE, legend = "bottom"), top="Scaling Executors Cores for each Dataset")



em1<-generateDistributionPlot(allDataOriginal, "ExMem", 2, "2GB", "DataSizeMB")
em2<-generateDistributionPlot(allDataOriginal, "ExMem", 4, "4GB", "DataSizeMB")
em3<-generateDistributionPlot(allDataOriginal, "ExMem", 6, "6GB", "DataSizeMB")
em4<-generateDistributionPlot(allDataOriginal, "ExMem", 8, "8GB", "DataSizeMB")

annotate_figure(ggarrange(em1, em2, em3, em4, ncol=2, nrow=2, common.legend = TRUE, legend = "bottom"))

p10<-generatePlot(allDataOriginal, "ExMem", 2, "2GB", "DataSizeMB")
p11<-generatePlot(allDataOriginal, "ExMem", 4, "4GB", "DataSizeMB")
p12<-generatePlot(allDataOriginal, "ExMem", 6, "6GB", "DataSizeMB")
p13<-generatePlot(allDataOriginal, "ExMem", 8, "8GB", "DataSizeMB")

annotate_figure(ggarrange(p10, p11, p12, p13, ncol=2, nrow=2, common.legend = TRUE, legend = "bottom"), top="Scaling Dataset for each Memory Set")

pa <- generatePlot(allDataOriginal, "DataSizeMB", 191, "191MB", "ExMem")
pb <- generatePlot(allDataOriginal, "DataSizeMB", 275, "275MB", "ExMem")
pc <- generatePlot(allDataOriginal, "DataSizeMB", 489, "489MB", "ExMem")
pd <- generatePlot(allDataOriginal, "DataSizeMB", 618, "618MB", "ExMem")
pe <- generatePlot(allDataOriginal, "DataSizeMB", 764, "764MB", "ExMem")
pf <- generatePlot(allDataOriginal, "DataSizeMB", 3053, "3GB", "ExMem")
pg <- generatePlot(allDataOriginal, "DataSizeMB", 12209, "12GB", "ExMem")

annotate_figure(ggarrange(pa,pb,pc,pd,pe,pf,pg, ncol=2, nrow=4, common.legend = TRUE, legend = "bottom"), top="Scaling Executors Memory for each Dataset")

p14<-generatePlot(allDataOriginal, "LevelPar", 8, "8", "DataSizeMB")
p15<-generatePlot(allDataOriginal, "LevelPar", 16, "16", "DataSizeMB")
p16<-generatePlot(allDataOriginal, "LevelPar", 32, "32", "DataSizeMB")
p17<-generatePlot(allDataOriginal, "LevelPar", 64, "64", "DataSizeMB")

annotate_figure(ggarrange(p14, p15, p16, p17, ncol=2, nrow=2, common.legend = TRUE, legend = "bottom"), top="Scaling Dataset for each Level of Paralellism Set")

po <- generatePlot(allDataOriginal, "DataSizeMB", 191, "191MB", "LevelPar")
pp <- generatePlot(allDataOriginal, "DataSizeMB", 275, "275MB", "LevelPar")
pq <- generatePlot(allDataOriginal, "DataSizeMB", 489, "489MB", "LevelPar")
pr <- generatePlot(allDataOriginal, "DataSizeMB", 618, "618MB", "LevelPar")
ps <- generatePlot(allDataOriginal, "DataSizeMB", 764, "764MB", "LevelPar")
pt <- generatePlot(allDataOriginal, "DataSizeMB", 3053, "3GB", "LevelPar")
pu <- generatePlot(allDataOriginal, "DataSizeMB", 12209, "12GB", "LevelPar")

annotate_figure(ggarrange(po,pp,pq,pr,ps,pt,pu, ncol=2, nrow=4, common.legend = TRUE, legend = "bottom"), top="Scaling Level of Paralellism Memory for each Dataset")




##Perform some job level basic modelling

set.seed(123)
#Two third 20 for training and 10 for testing
split = sample.split(allDataOriginal$Duration.s., SplitRatio = 0.8)
training_set = subset(allDataOriginal, split == TRUE)
test_set = subset(allDataOriginal, split == FALSE)

#The next four lines i.e. preprocessing has not improve performance in my case or may be I am doing it wrong but I doubt it.
#trainingPreProcValues = preProcess(training_set, method = list(center = names(training_set)[2:7], scale=names(training_set)[2:7]))
#testPreProcValues = preProcess(test_set, method = list(center = names(test_set)[2:7], scale=names(test_set)[2:7]))

#training_set = predict(trainingPreProcValues, training_set)
#test_set = predict(testPreProcValues, test_set)

#Do feature scaling, comment this if you are using caret
#training_set[,2:7] = scale(training_set[,2:7])
#test_set[,2:7] = scale(test_set[,2:7])

#scaleobject = scale(training_set[,c(2:5,7)])

#Now using caret package run the regression for the algorithms below
#Run Linear, SVM, RandomForest and Decision Trees without any tuning. 
#Run these algorithms tuning the parameters. 
#Evaluate their RMSEs and RSquares. 
#Select the best ML algorithms for each 

head(training_set)
#Training without any tuning. 
control <- trainControl(method="repeatedcv", number=10, repeats=3)
rf <- train(Duration.s.~ DataSizeGB + NumEx  + ExCore + ExMem, data=training_set[c(-6)], method="rf", trControl=control)
svm <- train(Duration.s.~ DataSizeGB + NumEx  + ExCore + ExMem, data=training_set[c(-6)], method="svmRadial", trControl=control)
lm <- train(Duration.s.~ DataSizeGB + NumEx  + ExCore + ExMem, data=training_set[c(-6)], method="lm", trControl=control)
rpart <- train(Duration.s.~ DataSizeGB + NumEx  + ExCore + ExMem, data=training_set[c(-6)], method="rpart2", trControl=control)
#bayes <- train(Duration.s.~ DataSizeGB + NumEx  + ExCore + ExMem, data=training_set[c(-6)], method="bridge", trControl=control)

#Train with tuning random search: sorce: https://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/
control <- trainControl(method="repeatedcv", number=10, repeats=3, search = "random")
mtry <- sqrt(ncol(training_set))#not needed random search will automaticall do this
rf.tune.random <- train(Duration.s.~ DataSizeGB + NumEx  + ExCore + ExMem, data=training_set[c(-6)], method="rf", trControl=control)
svm.svm.tune.random <- train(Duration.s.~ DataSizeGB + NumEx  + ExCore + ExMem, data=training_set[c(-6)], method="svmRadial", trControl=control)
lm.tune.random <- train(Duration.s.~ DataSizeGB + NumEx  + ExCore + ExMem, data=training_set[c(-6)], method="lm", trControl=control)
rpart.tune.random <- train(Duration.s.~ DataSizeGB + NumEx  + ExCore + ExMem, data=training_set[c(-6)], method="rpart2", trControl=control)


#Training models using grid search source: https://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/
control <- trainControl(method="repeatedcv", number=10, repeats=3, search = "grid")
grid_radial <- expand.grid(sigma = c(0, 0.0001, 0.001, 0.01, 0.02, 0.025), C = c(1, 100, 500, 1000, 1500, 2000, 3000, 4000, 5000 ))
mtry <- sqrt(ncol(training_set))#not needed grid search will automaticall do this
rf.tune.grid <- train(Duration.s.~ DataSizeGB + NumEx  + ExCore + ExMem, data=training_set[c(-6)], method="rf", trControl=control)
svm.tune.grid <- train(Duration.s.~ DataSizeGB + NumEx  + ExCore + ExMem, data=training_set[c(-6)], method="svmRadial", tuneGrid=grid_radial, trControl=control)
lm.tune.grid <- train(Duration.s.~ DataSizeGB + NumEx  + ExCore + ExMem, data=training_set[c(-6)], method="lm", trControl=control)
rpart.tune.grid <- train(Duration.s.~ DataSizeGB + NumEx  + ExCore + ExMem, data=training_set[c(-6)], method="rpart2", trControl=control)

allModels <- resamples(list(RF.NORMAL=rf, SVM.NORMAL=svm, LM.NORMAL=lm, RPART.NORMAL=rpart,
                            RF.RANDOM=rf.tune.random, SVM.RANDOM=svm.tune.random, LM.RANDOM=lm.tune.random, RPART.RANDOM=rpart.tune.random, 
                            RF.GRID=rf.tune.grid, SVM.GRID=svm.tune.grid, LM.GRID=lm.tune.grid, RPART.GRID=rpart.tune.grid))

bwplot(allModels, metric = "Rsquared")
bwplot(allModels, metric = "RMSE")
bwplot(allModels, metric = "MAE")



# all <- allModels$values
# 
# summary(allModels)
# bwplot(allModels)
# dotplot(allModels)
# 
# compare_models(rf.tune, svm.tune)
# 
# print(svm)
# print(rpart.tune)
# 
# plot(svm)
# plot(rf.tune.grid)
# print(rf.tune.grid)

generateModelPlotOnTest <- function(data, model, title, xlabel, ylabel){
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
          axis.title=element_text(size=12,face="bold"), axis.text.x = element_text(size = 11, face = "bold", angle = 0, hjust = 1))
  return (plot)
}

set.seed(12333)
testsamplesvm <- (sample_n(test_set, 100))

tp1bys <- generateModelPlotOnTest(testsamplesvm, svm, "SVM", "Index", "Time(s)")
finalsvmplot <- generateModelPlotOnTest(testsamplesvm, svm.tune.random, "Support Vector Machine", "Observation", "Time(s)")
tp3bys <- generateModelPlotOnTest(testsamplesvm, svm.tune.grid, "SVM Grid", "Index", "Time(s)")

tp1 <- generateModelPlotOnTest(test_set, svm, "SVM Without Tunning", "Index", "Time(s)")
tp2 <- generateModelPlotOnTest(test_set, svm.tune.random, "SVM Tunned with Random Method", "Index", "Time(s)")
tp3 <- generateModelPlotOnTest(test_set, svm.tune.grid, "SVM Tunned with Grid Method", "Index", "Time(s)")
tp4 <- generateModelPlotOnTest(test_set, rf, "RF Without Tuning", "Index", "Time(s)")
tp5 <- generateModelPlotOnTest(test_set, rf.tune.random, "RF Tunned with Random Method", "Index", "Time(s)")
tp6 <- generateModelPlotOnTest(test_set, rf.tune.grid, "RF Tunned with Grid Method", "Index", "Time(s)")
tp7 <- generateModelPlotOnTest(test_set, rpart, "DT Without Tuning", "Index", "Time(s)")
tp8 <- generateModelPlotOnTest(test_set, rpart.tune.random, "DT Tunned with Normal Method", "Index", "Time(s)")
tp9 <- generateModelPlotOnTest(test_set, rpart.tune.grid, "DT Tunned with Grid Method", "Index", "Time(s)")
#tp10 <- generateModelPlotOnTest(test_set, lm, "LM", "Index", "Time(s)")
#tp11 <- generateModelPlotOnTest(test_set, lm.tune.random, "LM Tunned with Normal Method", "Index", "Time(s)")
#tp12 <- generateModelPlotOnTest(test_set, lm.tune.random, "LM Tunned with Grid Method", "Index", "Time(s)")

ggarrange(tp1,tp2,tp3,tp4,tp5,tp6,tp7,tp8,tp9, nrow=3, ncol = 3, common.legend = TRUE, legend = "bottom")

#65.917     4      2     2       64        191  0.1865465

predict(svm, data.frame("DataSizeGB" = 0.1865465, "NumEx" = 4, "ExCore" = 2, "ExMem" = 2, "LevelPar" = 64))
predict(svm.tune.random, data.frame("DataSizeGB" = 0.1865465, "NumEx" = 4, "ExCore" = 2, "ExMem" = 2, "LevelPar" = 64))
predict(svm.tune.grid, data.frame("DataSizeGB" = 0.1865465, "NumEx" = 4, "ExCore" = 2, "ExMem" = 2, "LevelPar" = 64))

predict(rf, data.frame("DataSizeGB" = 0.1865465, "NumEx" = 4, "ExCore" = 2, "ExMem" = 2, "LevelPar" = 64))
predict(rf.tune.random, data.frame("DataSizeGB" = 0.1865465, "NumEx" = 4, "ExCore" = 2, "ExMem" = 2, "LevelPar" = 64))
predict(rf.tune.grid, data.frame("DataSizeGB" = 0.1865465, "NumEx" = 4, "ExCore" = 2, "ExMem" = 2, "LevelPar" = 64))

predict(rpart, data.frame("DataSizeGB" = 0.1865465, "NumEx" = 4, "ExCore" = 2, "ExMem" = 2, "LevelPar" = 64))
predict(rpart.tune.random, data.frame("DataSizeGB" = 0.1865465, "NumEx" = 4, "ExCore" = 2, "ExMem" = 2, "LevelPar" = 64))
predict(rpart.tune.grid, data.frame("DataSizeGB" = 0.1865465, "NumEx" = 4, "ExCore" = 2, "ExMem" = 2, "LevelPar" = 64))

#1923    2907.241     4      2     2       64      12209 11.9231721

predict(svm, data.frame("DataSizeGB" = 20.9231721, "NumEx" = 4, "ExCore" = 2, "ExMem" = 2, "LevelPar" = 64))
predict(svm.tune.random, data.frame("DataSizeGB" = 20.9231721, "NumEx" = 4, "ExCore" = 2, "ExMem" = 2, "LevelPar" = 64))
predict(svm.tune.grid, data.frame("DataSizeGB" = 20.9231721, "NumEx" = 4, "ExCore" = 2, "ExMem" = 2, "LevelPar" = 64))

predict(rf, data.frame("DataSizeGB" = 1.9231721, "NumEx" = 4, "ExCore" = 2, "ExMem" = 2, "LevelPar" = 64))
predict(rf.tune.random, data.frame("DataSizeGB" = 1.9231721, "NumEx" = 4, "ExCore" = 2, "ExMem" = 2, "LevelPar" = 64))
predict(rf.tune.grid, data.frame("DataSizeGB" = 1.9231721, "NumEx" = 4, "ExCore" = 2, "ExMem" = 2, "LevelPar" = 64))

# predict(rpart, data.frame("DataSizeGB" = 110.9231721, "NumEx" = 4, "ExCore" = 2, "ExMem" = 2, "LevelPar" = 64))
# predict(rpart.tune.random, data.frame("DataSizeGB" = 110.9231721, "NumEx" = 4, "ExCore" = 2, "ExMem" = 2, "LevelPar" = 64))
# predict(rpart.tune.grid, data.frame("DataSizeGB" = 110.9231721, "NumEx" = 4, "ExCore" = 2, "ExMem" = 2, "LevelPar" = 64))

save(rf.tune.grid, file = "RFGridModelForSVM.RData")
save(svm.tune.grid, file = "SVMGridModelForSVM.RData")
