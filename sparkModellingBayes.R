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
allDataOriginal <- read.table(file = "bayes.report", header = TRUE)#contains both 128
head(allDataOriginal)

names(allDataOriginal)
#add observation and features and scal it to tenthsvm.report

head(allDataOriginal)

allDataOriginal$DataSizeMB <- round((allDataOriginal$Input_data_size/1048576))
allDataOriginal$DataSizeGB <- allDataOriginal$Input_data_size/1073741824


allDataOriginal <- select(allDataOriginal, Duration.s., NumEx, ExCore, ExMem, LevelPar, DataSizeMB, DataSizeGB)

#allDataOriginal <- filter(allDataOriginal, Duration.s. <= 700)

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
mtry <- sqrt(ncol(training_set))
tunegrid <- expand.grid(.mtry=mtry)
rf <- train(Duration.s.~ DataSizeGB + NumEx  + ExCore + ExMem, data=training_set[c(-6)], method="rf", trControl=control, tuneGrid=tunegrid)
svm <- train(Duration.s.~ DataSizeGB + NumEx  + ExCore + ExMem, data=training_set[c(-6)], method="svmRadial", trControl=control)
lm <- train(Duration.s.~ DataSizeGB + NumEx  + ExCore + ExMem, data=training_set[c(-6)], method="lm", trControl=control)
rpart <- train(Duration.s.~ DataSizeGB + NumEx  + ExCore + ExMem, data=training_set[c(-6)], method="rpart2", trControl=control)
#bayes <- train(Duration.s.~ DataSizeGB + NumEx  + ExCore + ExMem, data=training_set[c(-6)], method="bridge", trControl=control)

#Train with tuning random search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search = "random")
rf.tune.random <- train(Duration.s.~ DataSizeGB + NumEx  + ExCore + ExMem, data=training_set[c(-6)], method="rf", trControl=control)
bayes.svm.tune.random <- train(Duration.s.~ DataSizeGB + NumEx  + ExCore + ExMem, data=training_set[c(-6)], method="svmRadial", trControl=control)
lm.tune.random <- train(Duration.s.~ DataSizeGB + NumEx  + ExCore + ExMem, data=training_set[c(-6)], method="lm", trControl=control)
rpart.tune.random <- train(Duration.s.~ DataSizeGB + NumEx  + ExCore + ExMem, data=training_set[c(-6)], method="rpart2", trControl=control)


#Training models using grid search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search = "grid")
grid_radial <- expand.grid(sigma = c(0, 0.0001, 0.001, 0.01, 0.02, 0.025), C = c(1, 100, 500, 1000, 1500, 2000, 3000, 4000, 5000 ))
rf.tune.grid <- train(Duration.s.~ DataSizeGB + NumEx  + ExCore + ExMem, data=training_set[c(-6)], method="rf", trControl=control)
svm.tune.grid <- train(Duration.s.~ DataSizeGB + NumEx  + ExCore + ExMem, data=training_set[c(-6)], method="svmRadial", tuneGrid=grid_radial, trControl=control)
lm.tune.grid <- train(Duration.s.~ DataSizeGB + NumEx  + ExCore + ExMem, data=training_set[c(-6)], method="lm", trControl=control)
rpart.tune.grid <- train(Duration.s.~ DataSizeGB + NumEx  + ExCore + ExMem, data=training_set[c(-6)], method="rpart2", trControl=control)

allModels <- resamples(list(RF.NORMAL=rf, SVM.NORMAL=svm, LM.NORMAL=lm, RPART.NORMAL=rpart,
                            RF.RANDOM=rf.tune.random, SVM.RANDOM=svm.tune.random, LM.RANDOM=lm.tune.random, RPART.RANDOM=rpart.tune.random, 
                            RF.GRID=rf.tune.grid, SVM.GRID=svm.tune.grid, LM.GRID=lm.tune.grid, RPART.GRID=rpart.tune.grid))

print(rf)

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

# set.seed(12345)
# head(sample_n(test_set, 30))
# testsample <- sample_n(test_set, 30)
# testdur <- testsample$Duration.s.
# testdata <- testsample$DataSizeMB
# predictsample <- predict(svm, testsample)
# df <- data.frame(testdur, predictsample, testdata)
# df

set.seed(12333)
testsamplebys <- (sample_n(test_set, 100))

tp1bys <- generateModelPlotOnTest(testsamplebys, svm, "SVM", "Index", "Time(s)")
tp2bys <- generateModelPlotOnTest(testsamplebys, bayes.svm.tune.random, "Naive Bayes", "Observation", "Time(s)")
tp3bys <- generateModelPlotOnTest(testsamplebys, svm.tune.grid, "SVM Grid", "Index", "Time(s)")

tp1bys
tp2bys
tp3bys

ggarrange(tp2kmeans, tp2lr, tp2svm, tp2lir, tp2rf, tp2bys, nrow=3, ncol = 2, common.legend = TRUE, legend = "bottom")

ggarrange(tp2lr, tp2lir, nrow=1, ncol = 2, common.legend = TRUE, legend = "bottom")

ggarrange(tp2kmeans, tp2rf,  nrow=1, ncol = 2, common.legend = TRUE, legend = "bottom")

ggarrange(tp2svm, tp2bys, nrow=1, ncol = 2, common.legend = TRUE, legend = "bottom")


tp4 <- generateModelPlotOnTest(sample_n(test_set, 60), rf, "RF", "Index", "Time(s)")
tp5 <- generateModelPlotOnTest(sample_n(test_set, 60), rf.tune.random, "RF Random", "Obserevation", "Time(s)")
tp6 <- generateModelPlotOnTest(sample_n(test_set, 60), rf.tune.grid, "RF Grid", "Index", "Time(s)")

tp4
tp5
tp6



tp4 <- generateModelPlotOnTest(test_set, rf, "RF", "Index", "Time(s)")
tp5 <- generateModelPlotOnTest(test_set, rf.tune.random, "RF Random", "Index", "Time(s)")
tp6 <- generateModelPlotOnTest(test_set, rf.tune.grid, "RF Grid", "Index", "Time(s)")


tp7 <- generateModelPlotOnTest(test_set, rpart, "DT", "Index", "Time(s)")
tp8 <- generateModelPlotOnTest(test_set, rpart.tune.random, "DT Random", "Index", "Time(s)")
tp9 <- generateModelPlotOnTest(test_set, rpart.tune.grid, "DT Grid", "Index", "Time(s)")
tp10 <- generateModelPlotOnTest(test_set, lm, "LM", "Index", "Time(s)")

#Linear Models are not tunable.
#tp11 <- generateModelPlotOnTest(test_set, lm.tune.random, "LM Tunned with Normal Method", "Index", "Time(s)")
#tp12 <- generateModelPlotOnTest(test_set, lm.tune.random, "LM Tunned with Grid Method", "Index", "Time(s)")

ggarrange(tp1,tp2,tp3,tp4,tp5,tp6,tp7,tp8,tp9, nrow=3, ncol = 3, common.legend = TRUE, legend = "bottom")

tp1
tp2
tp3

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
