#Define a function to calculate RMSE
RMSE <- function(rating, est_rating){
  sqr_err <- function(obs){
    sqr_error <- (obs[3] - est_rating[as.character(obs[2]), as.character(obs[1])])^2
    return(sqr_error)
  }
  return(sqrt(mean(apply(rating, 1, sqr_err))))  
}

#Stochastic Gradient Descent
# a function returns a list containing factorized matrices p and q, training and testing RMSEs.
gradesc <- function(f = 10, 
                    lambda = 0.3,lrate = 0.01, max.iter, stopping.deriv = 0.01,
                    data, train, test){
  set.seed(0)
  #random assign value to matrix p and q
  p <- matrix(runif(f*U, -1, 1), ncol = U) 
  colnames(p) <- as.character(1:U)
  q <- matrix(runif(f*I, -1, 1), ncol = I)
  colnames(q) <- levels(as.factor(data$movieId))
  
  train_RMSE <- c()
  test_RMSE <- c()
  
  for(l in 1:max.iter){
    sample_idx <- sample(1:nrow(train), nrow(train))
    #loop through each training case and perform update
    for (s in sample_idx){
      
      u <- as.character(train[s,1])
      
      i <- as.character(train[s,2])
      
      r_ui <- train[s,3]
      
      e_ui <- r_ui - t(q[,i]) %*% p[,u]
      
      grad_q <- e_ui %*% p[,u] - lambda * q[,i]
      
      if (all(abs(grad_q) > stopping.deriv, na.rm = T)){
        q[,i] <- q[,i] + lrate * grad_q
      }
      grad_p <- e_ui %*% q[,i] - lambda * p[,u]
      
      if (all(abs(grad_p) > stopping.deriv, na.rm = T)){
        p[,u] <- p[,u] + lrate * grad_p
      }
    }
    #print the values of training and testing RMSE
    if (l %% 10 == 0){
      cat("epoch:", l, "\t")
      est_rating <- t(q) %*% p
      rownames(est_rating) <- levels(as.factor(data$movieId))
      
      train_RMSE_cur <- RMSE(train, est_rating)
      cat("training RMSE:", train_RMSE_cur, "\t")
      train_RMSE <- c(train_RMSE, train_RMSE_cur)
      
      test_RMSE_cur <- RMSE(test, est_rating)
      cat("test RMSE:",test_RMSE_cur, "\n")
      test_RMSE <- c(test_RMSE, test_RMSE_cur)
    } 
  }
  
  return(list(p = p, q = q, train_RMSE = train_RMSE, test_RMSE = test_RMSE))
}


###########################Wenyue Edit###########################
##function to ensure that when splitting to train and test case, the training data contains all users and movies
##not stable at the moment. do not use
train_test_split <- function(data,train_ratio = 0.8){
  rownames(data) <- paste(data$userId,data$movieId,sep = '_')
  test_idx <- sample(1:nrow(data), round(nrow(data)/(1/(1-train_ratio)), 0))
  train_idx <- setdiff(1:nrow(data), test_idx)
  data_train <- data[train_idx,]
  data_test <- data[test_idx,]
  
  allMovies <- unique(data$movieId)
  allUsers <- unique(data$userId)
  
  dupMovies <- data_train %>% group_by(movieId) %>% summarise(Count = n()) %>% filter(Count > 1)
  dupUsers <- data_train %>% group_by(userId) %>% summarise(Count = n()) %>% filter(Count > 1)
  dupMovieUsers <- data_train %>% filter(movieId %in% dupMovies$movieId, userId %in% dupUsers$userId) ##these entries are safe to remove
  rownames(dupMovieUsers) <- paste(dupMovieUsers$userId,dupMovieUsers$movieId,sep = '_')
  
  #if there are movies in testing set but not in training set, we need to add them to training
  if (!all(allMovies %in% data_train$movieId)){
    print('Movies missing. Modifying:')
    addMovies <- allMovies[which(!allMovies %in% data_train$movieId)]
    
    #data to add to train
    add_data <- as.data.frame(data_test %>% filter(movieId %in% addMovies) %>% arrange(movieId) %>% group_by(movieId) %>% group_modify(~{
      .x[sample(1:nrow(.x),1),]
    }))
    rownames(add_data) <- paste(add_data$userId,add_data$movieId,sep = '_')
    
    #data to remove from train
    remove_data <- dupMovieUsers[sample(1:nrow(dupMovieUsers),nrow(add_data)),]
    
    #modify train data
    data_train <- data_train[-which(rownames(data_train) %in% rownames(remove_data)),]
    data_train <- rbind(data_train,add_data)
     
    #modify test data
    data_test <- data_test[-which(rownames(data_test) %in% rownames(add_data)),]
    data_test <- rbind(data_test, remove_data)
     
  }
  
  dupMovies <- data_train %>% group_by(movieId) %>% summarise(Count = n()) %>% filter(Count > 1)
  dupUsers <- data_train %>% group_by(userId) %>% summarise(Count = n()) %>% filter(Count > 1)
  dupMovieUsers <- data_train %>% filter(movieId %in% dupMovies$movieId, userId %in% dupUsers$userId) ##these entries are safe to remove
  rownames(dupMovieUsers) <- paste(dupMovieUsers$userId,dupMovieUsers$movieId,sep = '_')
  
  #if there are users in testing set but not in training set, we need to add them to training
  if (!all(allUsers %in% data_train$userId)){
    print('User missing. Modifying:')
    addUsers <- allUsers[which(!allUsers %in% data_train$userId)]
    
    #data to add to train
    add_data <- as.data.frame(data_test %>% filter(userId %in% addUsers) %>% arrange(userId) %>% group_by(userId) %>% group_modify(~{
      .x[sample(1:nrow(.x),1),]
    }))
    rownames(add_data) <- paste(add_data$userId,add_data$movieId,sep = '_')
    
    #data to remove from train
    remove_data <- dupMovieUsers[sample(1:nrow(dupMovieUsers),nrow(add_data)),]
    
    #modify train data
    data_train <- data_train[-which(rownames(data_train) %in% rownames(remove_data)),]
    data_train <- rbind(data_train,add_data)
    
    #modify test data
    data_test <- data_test[-which(rownames(data_test) %in% rownames(add_data)),]
    data_test <- rbind(data_test, remove_data)
    
  }
  
  return(list(train = data_train,test = data_test))  
  
  
}


als <- function(f = 10,  lambda = 0.3,max.iter = 10,data, train, test){
  U <- length(unique(data$userId))
  I <- length(unique(data$movieId))
  
  #p = user matrix; q = movie matrix
  #assign random small value to matrix p and q
  p <- matrix(runif(f*U, -1, 1), ncol = U) 
  colnames(p) <- as.character(1:U)
  q <- matrix(runif(f*I, -1, 1), ncol = I)
  colnames(q) <- levels(as.factor(data$movieId))
  #assign first row of q to be the average rating for that movie, as per P4
  meanMovieRating <- train %>% group_by(movieId) %>% summarise(Rating = mean(rating)) %>% arrange(movieId)
  q[1,as.character(meanMovieRating$movieId)] <- meanMovieRating$Rating
  
  #before 
  est_rating <- t(q) %*% p
  rownames(est_rating) <- levels(as.factor(data$movieId))
  # print(paste0("RMSE at start = ", round(RMSE(train, est_rating),4)))
  # 
  RMSEOut <- data.frame(matrix(NA,max.iter+1,5))
  colnames(RMSEOut) <- c("Iteration",'Train.P.Update','Train.Q.Update','Test.P.Update','Test.Q.Update')
  RMSEOut$Iteration <- c(0:max.iter)
  RMSEOut$Train.Q.Update[1] <- RMSE(train, est_rating)
  RMSEOut$Test.Q.Update[1] <- RMSE(test, est_rating)
  cat("RMSE at start, training:", RMSEOut$Train.Q.Update[1], "\t testing:",RMSEOut$Test.Q.Update[1], "\n")
  
  
  for (l in 1:max.iter){
    cat("Iteration =",l,':\n')
    #fix q and update p
    result <- foreach (pi = 1:ncol(p),.combine = cbind,.packages = c('dplyr','tidyr')) %dopar% {
      i <- colnames(p)[pi] #the user we're updating
      
      if (i %in% train$userId){ #only proceed if the user is in the training set
        Ii <- as.character(train %>% filter(userId == i) %>% arrange(movieId) %>% pull(movieId)) #list of movies that this user has rated  
        MIi <- q[,Ii,drop = F]
        RVec <- train %>% filter(userId == i) %>% arrange(movieId) %>% pull(rating) #all ratings by this user
        
        Ai <- MIi %*% t(MIi) + lambda * length(Ii) * diag(f)
        Vi <- MIi %*% RVec
        solve(Ai) %*% Vi
      }else{
        p[,i]
      }
    }
    colnames(result) <- colnames(p)
    p <- result
    
    est_rating <- t(q) %*% p
    rownames(est_rating) <- levels(as.factor(data$movieId))
    RMSEOut$Train.P.Update[l+1] <- RMSE(train, est_rating)
    RMSEOut$Test.P.Update[l+1] <- RMSE(test, est_rating)
    cat("RMSE after p update, training:", RMSEOut$Train.P.Update[l+1],"\t testing:",RMSEOut$Test.P.Update[l+1],'\n')

    
    #fix p and update q
    result <- foreach (qi = 1:ncol(q),.combine = cbind,.packages = c('dplyr','tidyr')) %dopar% {
      j<- colnames(q)[qi]
      
      if (j %in% train$movieId){ #only proceed if the movie is in the training set
        Ij <- train %>% filter(movieId == j) %>% arrange (userId) %>% pull(userId) #list of users that this movie was rated by
        UIj <- p[,Ij,drop = F]
        RVec <- train %>% filter(movieId == j) %>% arrange(userId) %>% pull(rating) #all ratings of this movie
        
        Aj <- UIj %*% t(UIj) + lambda * length(Ij) * diag(f)
        Vj <- UIj %*% RVec
        solve(Aj) %*% Vj
      }else{
        # print(paste0('Movie ',j, ' Not In Training Set'))
        q[,j]
      }
    }
    colnames(result) <- colnames(q)
    q <- result
    
    est_rating <- t(q) %*% p
    rownames(est_rating) <- levels(as.factor(data$movieId))
    RMSEOut$Train.Q.Update[l+1] <- RMSE(train, est_rating)
    RMSEOut$Test.Q.Update[l+1] <- RMSE(test, est_rating)
    cat("RMSE after q update, training:", RMSEOut$Train.Q.Update[l+1],"\t testing:",RMSEOut$Test.Q.Update[l+1],'\n')
  }
  
  
  return(list(p = p, q = q, RMSE = RMSEOut))

}


#predict ratings. Result is in a vector of same length as the number of rows of rating data frame
predictRating <- function(rating,mu,p,q,bi,bibin,dateIntervals){
  out <- c()
  if (!'Date' %in% colnames(rating)){
    rating$Date <- as.Date(as.POSIXlt(rating$timestamp,origin = "1970-01-01",tz = 'UTC'))
  }
  
  if (!'Interval' %in% colnames(rating)){
    rating$Interval <- findInterval(rating$Date,dateIntervals) 
  }
  
  rating$userId <- as.character(rating$userId)
  rating$movieId <- as.character(rating$movieId)
  
  factorEst <- t(q) %*% p
  
  compRating <- function(obs){
    mu + bi[obs[2]] + bibin[as.numeric(obs[6]),obs[2]] + factorEst[obs[2], obs[1]]
  }
  apply(rating,1,compRating)
}

#here est_rating is just a vector of ratings correspondign to the first parameter
RMSE2 <- function(rating,est_rating){
  sqrt(mean((rating$rating-est_rating)^2))
}

#################################################################################
