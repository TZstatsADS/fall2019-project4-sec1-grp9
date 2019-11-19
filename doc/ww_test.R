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

# out <- als.t(f = 10,  lambda = 0.01,max.iter = 10,data = data, train = data_train, test = data_test)



f_list <- c(10,20)
l_list <- c(0.01,0.05,0.1)
f_l <- expand.grid(f_list, l_list)



# resultList <- list()
# run_time <- system.time(for(i in 1:nrow(f_l)){
#   par <- paste("f = ", f_l[i,1], ", lambda = ", f_l[i,2])
#   cat(par, "\n")
# 
#   current_result  <- cv.als.t.function(data, K = 5, f = f_l[i,1], lambda = f_l[i,2], max.iter = 10)
#   resultList[[paste0('f = ',f_l[i,1],' , lambda = ',f_l[i,2])]] <- current_result
# 
# 
# })

resultList <- list()
run_time <- system.time(for(i in 1:nrow(f_l)){
  par <- paste("f = ", f_l[i,1], ", lambda = ", f_l[i,2])
  cat(par, "\n")
  
  current_result  <- als.t(f = f_l[i,1],  lambda = f_l[i,2],max.iter = 10,data = data, train = data_train, test = data_test)
  resultList[[paste0('f = ',f_l[i,1],' , lambda = ',f_l[i,2])]] <- current_result
  
  
})

save(resultList, file = "../output/CVResult.RData")


