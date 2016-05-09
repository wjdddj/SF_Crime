setwd('~/my_Git_repos/SF_Crime/')
library(data.table)
library(ggplot2)
library(scales)
library(plyr)
library(doParallel)
library(RTextTools)
source('Models/mcll.R')

load('~/my_Git_repos/SF_Crime/Models/sf_train.Rda')
set.seed(1)
sf_train1 <- droplevels(sf_train[sample(nrow(sf_train), 100000, replace = F)])

## neural network
library(nnet)
str(sf_train1)
sf_train1$Address <- NULL
sf_train1$X <- NULL
sf_train1$Y <- NULL
sf_train1$Date <- NULL
train_nn1 <- nnet(Category~., data = sf_train1[,1:3,with=F], size = 10, maxit = 100)
mcll(train_nn1$fitted.values, sf_train1$Category)


library(neuralnet)
+ Year + Month + Hour
sf_train1_fornet <- model.matrix(~ Category + DayOfWeek + PdDistrict, data = sf_train1)
train_net1 <- neuralnet(Category ~ DayOfWeek + PdDistrict, sf_train1_fornet, 
                        hidden = c(10), rep = 1, linear.output = FALSE)



str(sf_train)
sf_train$Address <- NULL
sf_train$X <- NULL
sf_train$Y <- NULL

train_nn <- nnet(Category~., data = sf_train, size = 10, MaxNWts = 20, maxit = 500)















