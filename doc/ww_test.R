##change this path
setwd('D:/wwyws/Documents/Columbia/STAT5243 Applied Data Science/fall2019-project4-sec1-grp9/doc')




library(dplyr)
library(tidyr)
library(ggplot2)
library(foreach)
library(doParallel)
library(zoo)

source("../lib/Matrix_Factorization.R")
source("../lib/cross_validation.R")

##load data
data <- read.csv("../data/ml-latest-small/ratings.csv")
set.seed(0)
maxIter <- 10


##train test split
test_idx <- sample(1:nrow(data), round(nrow(data)/5, 0))
train_idx <- setdiff(1:nrow(data), test_idx)
data_train <- data[train_idx,]
data_test <- data[test_idx,]



##ALS example
registerDoParallel(cores = detectCores())
result <- als(f = 10,  lambda = 0.1,max.iter = 10,data=data, train=data_train, test=data_test)
