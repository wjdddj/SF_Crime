setwd('~/my_Git_repos/SF_Crime/')
library(data.table)
library(ggplot2)
library(scales)
library(plyr)
library(doParallel)
library(RTextTools)


#########################################################################################################################
## step 1. data clean up training set
sf_train <- fread('~/Documents/Kaggle/SF_crime/train.csv', sep = ',')
sapply(sf_train, class)
str(sf_train)

sf_train$Category  <- factor(sf_train$Category, 
                             levels = names(sort(table(sf_train$Category), decreasing = TRUE)))
sf_train$PdDistrict  <- factor(sf_train$PdDistrict, 
                               levels = names(sort(table(sf_train$PdDistrict), decreasing = TRUE)))
sf_train$Resolution <- factor(sf_train$Resolution, 
                              levels = names(sort(table(sf_train$Resolution), decreasing = TRUE)))
sf_train$DayOfWeek <- factor(sf_train$DayOfWeek,
                             levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
sf_train$Day <- as.Date(sapply(sf_train$Dates, function(x)strsplit(x, ' ')[[1]][1]))
sf_train$Year <- factor(format(sf_train$Day, '%Y'))
sf_train$Month <- factor(format(sf_train$Day, '%m'))
sf_train$Date <- factor(format(sf_train$Day, '%d'))
sf_train$Time <- sapply(sf_train$Dates, function(x)strsplit(x, ' ')[[1]][2])
sf_train$Hour <- sapply(sf_train$Time, function(x)strsplit(x, ':')[[1]][1])

sf_train[, Hour:=factor(Hour)]
sf_train[, Address:=factor(Address)]
sf_train$Day <- NULL
sf_train$Time <- NULL
sf_train$Dates <- NULL
sf_train$Descript <- NULL
sf_train$Resolution <- NULL
str(sf_train)

save(sf_train, file = 'Models/sf_train.Rda')

## step2 model

## step3 test set
str(sf_test)

sf_test <- fread('~/Documents/Kaggle/SF_crime/test.csv', sep = ',')
sf_test$PdDistrict  <- factor(sf_test$PdDistrict, 
                              levels = names(sort(table(sf_test$PdDistrict), decreasing = TRUE)))
sf_test$DayOfWeek <- factor(sf_test$DayOfWeek,
                            levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
sf_test$Day <- as.Date(sapply(sf_test$Dates, function(x)strsplit(x, ' ')[[1]][1]))
sf_test$Year <- factor(format(sf_test$Day, '%Y'))
sf_test$Month <- factor(format(sf_test$Day, '%m'))
sf_test$Date <- factor(format(sf_test$Day, '%d'))
sf_test$Time <- sapply(sf_test$Dates, function(x)strsplit(x, ' ')[[1]][2])
sf_test$Hour <- sapply(sf_test$Time, function(x)strsplit(x, ':')[[1]][1])

sf_test[, Time:=as.factor(Time)]
sf_test[, Hour:=as.factor(Hour)]
sf_test[, Address:=as.ordered(Address)]
sf_test$Day <- NULL
sf_test$Time <- NULL
sf_test$Dates <- NULL
str(sf_test)

save(sf_test, file = 'Models/sf_test.Rda')


library(doParallel)
library(randomForest)
library(data.table)
library(plyr)

load('~/my_Git_repos/SF_Crime/RFmodel/sf_train.Rda')
sf_train1 <- droplevels(sf_train[1:100000])

## random forest
train_rf1 <- randomForest(sf_train1[, -(1), with = F], sf_train1$Category, ntree = 100, mtry = 5, replace = F, do.trace = T)
train_rf2 <- randomForest(sf_train1[, -1, with = F], sf_train1$Category, ntree = 100, mtry = 5, replace = F, do.trace = T)
train_rf <- combine(train_rf1, train_rf2)
votes <- train_rf$votes/rowSums(train_rf$votes)

mcll(train_rf1$votes, sf_train1$Category)
mcll(train_rf2$votes, sf_train1$Category)
mcll(votes, sf_train1$Category)

