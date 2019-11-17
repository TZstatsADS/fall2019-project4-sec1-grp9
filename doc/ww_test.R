setwd('D:/wwyws/Documents/Columbia/STAT5243 Applied Data Science/fall2019-project4-sec1-grp9/doc')


### Step 1 Load Data and Train-test Split

library(dplyr)
library(tidyr)
library(ggplot2)
library(foreach)
library(doParallel)
library(zoo)
library(Matrix)
library(lubridate)

source("../lib/Matrix_Factorization.R")
source("../lib/cross_validation.R")

data <- read.csv("../data/ml-latest-small/ratings.csv")

set.seed(123)


dataSplit <- train_test_split(data,0.8) #this function ensures that all users and movies are in the training set. 
data_train <- dataSplit$train
data_test <- dataSplit$test


registerDoParallel(cores = detectCores())# use parallelization for faster procesing

out <- als.t(f = 10,  lambda = 0.01,max.iter = 3,data = data, train = data_train, test = data_test)
