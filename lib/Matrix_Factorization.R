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
train_test_split <- function(data,train_ratio = 0.8){
  rownames(data) <- paste(data$userId,data$movieId,sep = '_')
  test_idx <- sample(1:nrow(data), round(nrow(data)/(1/(1-train_ratio)), 0))
  train_idx <- setdiff(1:nrow(data), test_idx)
  data_train <- data[train_idx,]
  data_test <- data[test_idx,]
  
  allMovies <- unique(data$movieId)
  allUsers <- unique(data$userId)
  
 
  #if there are movies in testing set but not in training set, we need to add them to training
  while (!all(allMovies %in% data_train$movieId) | (!all(allUsers %in% data_train$userId))){
    dupMovies <- data_train %>% group_by(movieId) %>% summarise(Count = n()) %>% filter(Count > 1)
    dupUsers <- data_train %>% group_by(userId) %>% summarise(Count = n()) %>% filter(Count > 1)
    dupMovieUsers <- data_train %>% filter(movieId %in% dupMovies$movieId, userId %in% dupUsers$userId) ##these entries are somewhat safe to remove
    rownames(dupMovieUsers) <- paste(dupMovieUsers$userId,dupMovieUsers$movieId,sep = '_')
    
    if (!all(allMovies %in% data_train$movieId)){#add movies
      print('Movies missing in training after split. Making Changes...')
      addMovies <- allMovies[which(!allMovies %in% data_train$movieId)]
      
      #data to add to train
      add_data <- data_test %>% filter(movieId %in% addMovies) 
      add_data <- do.call(rbind,lapply(split(add_data,add_data$movieId),FUN = function(x){
        x[sample(1:nrow(x),1),]
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
    }else if (!all(allUsers %in% data_train$userId)){#add users
      print('User missing in training after split. Making Changes...')
      addUsers <- allUsers[which(!allUsers %in% data_train$userId)]
      
      #data to add to train
      add_data <- data_test %>% filter(userId %in% addUsers) 
      add_data <- do.call(rbind,lapply(split(add_data,add_data$userId),FUN = function(x){
        x[sample(1:nrow(x),1),]
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
      Ii <- as.character(train %>% filter(userId == i) %>% arrange(movieId) %>% pull(movieId)) #list of movies that this user has rated  
      MIi <- q[,Ii,drop = F]
      RVec <- train %>% filter(userId == i) %>% arrange(movieId) %>% pull(rating) #all ratings by this user
      
      # Ai <- MIi %*% t(MIi) + lambda * length(Ii) * diag(f)
      Ai <- MIi %*% t(MIi)
      Vi <- MIi %*% RVec
      solve(Ai,tol = 1e-30) %*% Vi

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
      Ij <- train %>% filter(movieId == j) %>% arrange (userId) %>% pull(userId) #list of users that this movie was rated by
      UIj <- p[,Ij,drop = F]
      RVec <- train %>% filter(movieId == j) %>% arrange(userId) %>% pull(rating) #all ratings of this movie
      
      Aj <- UIj %*% t(UIj) + lambda * length(Ij) * diag(f)
      # Aj <- UIj %*% t(UIj)
      Vj <- UIj %*% RVec
      solve(Aj,tol = 1e-30) %*% Vj

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


#the model is r_ui(t) = mu + b_i(t) +  b_u(t) + q^T %*% p(t), where
#b_i(t) = b_i + b_(i,Bin(t))
#b_u(t) = b_u + alpha_u * dev_u(t)
#p_u(t) = p_u + alpha_p * dev_u(t)
#For further details see P5.
als.t <- function(f = 10,  lambda = 0.3,max.iter = 10,data, train, test){
  
  #some pre processing work: add Date, Interval, and Deviation to columns
  data$Date <-as.Date(as.POSIXlt(data$timestamp,origin = "1970-01-01",tz = 'UTC'))
  train$Date <- as.Date(as.POSIXlt(train$timestamp,origin = "1970-01-01",tz = 'UTC'))
  test$Date <- as.Date(as.POSIXlt(test$timestamp,origin = "1970-01-01",tz = 'UTC'))
  
  dateIntervals <- as.Date(paste0(sort(unique(as.numeric(format(data$Date,'%Y')))),'-01-01')) #use years as Date Bins for b_i(t)
  train$Interval_i <- findInterval(train$Date,dateIntervals) 
  test$Interval_i <- findInterval(test$Date,dateIntervals)
  
  tu <- as.data.frame(data %>% group_by(userId) %>% summarise(MeanDate = round_date(mean(Date),'day'))) #mean date of rating for each user
  #here we precompute dev_u(t) for each entry. This is computed as (signed) (devation from the mean date of rating ^0.4). 0.4 is the paramter Beta taken from P5
  train$Dev <- as.numeric(sign(train$Date- tu$MeanDate[match(train$userId,tu$userId)]) *(as.numeric(abs((train$Date- tu$MeanDate[match(train$userId,tu$userId)])))^0.4))
  test$Dev <- as.numeric(sign(test$Date- tu$MeanDate[match(test$userId,tu$userId)]) *(as.numeric(abs((test$Date- tu$MeanDate[match(test$userId,tu$userId)])))^0.4))
  
  U <- length(unique(data$userId))
  I <- length(unique(data$movieId))
  
  
  #################initialize paramters################
  #mu,mean of all movie ratings
  mu <- mean(train$rating)
  
  #b_i.Non time-based biase for each movie, initialized to be 0
  bi <- rep(0,I); names(bi) <- levels(as.factor(data$movieId))
  #bibin(t), time-based biase for each movie, initialized to be 0
  bibin <- matrix(0,length(dateIntervals),I); colnames(bibin) <- levels(as.factor(data$movieId))
  
  #b_u. Non time-based biase for each user, initialized to be 0
  bu <-  rep(0,U); names(bu) <- levels(as.factor(data$userId))
  #alpha_u. The coefficient for dev_u(t), initialized to be 0
  alphau <- rep(0,U); names(alphau) <- levels(as.factor(data$userId))
  
  
  #p = user matrix; q = movie matrix
  #assign random small value to matrix p and q
  p <- matrix(runif(f*U, -1, 1), ncol = U) 
  colnames(p) <- levels(as.factor(data$userId))
  ##alpha parametr associated with p(t), initialized to be 0
  alphap <- matrix(0,nrow = f, ncol = U) 
  colnames(alphap) <- levels(as.factor(data$userId))
  q <- matrix(runif(f*I, -1, 1), ncol = I)
  colnames(q) <- levels(as.factor(data$movieId))
  ########################################################
  
  #RMSE at Start 
  RMSEOut <- data.frame(matrix(NA,max.iter+1,15))
  colnames(RMSEOut) <- c("Iteration",'Train.bi.Update','Train.bibin.Update','Train.bu.Update','Train.alphau.Update','Train.P.Update','Train.alphap.Update','Train.Q.Update',
                         'Test.bi.Update','Test.bibin.Update','Test.bu.Update','Test.alphau.Update','Test.P.Update','Test.alphap.Update','Test.Q.Update')
  RMSEOut$Iteration <- c(0:max.iter)
  RMSEOut$Train.Q.Update[1] <- RMSE2(train,predictRating(train,mu,q,p,bi,bibin,bu,alphau,alphap,dateIntervals,tu))
  RMSEOut$Test.Q.Update[1] <- RMSE2(test, predictRating(test,mu,q,p,bi,bibin,bu,alphau,alphap,dateIntervals,tu))
  cat("RMSE at start, training:", RMSEOut$Train.Q.Update[1], "\t testing:",RMSEOut$Test.Q.Update[1], "\n")
  
  #demean the training data for efficiency
  train1 <- train; train1$rating <- train1$rating - mu
  
  #start iterating
  for (l in 1:max.iter){
    cat("Iteration =",l,':\n')
    
    
    #update bi. Runtime on 8 core parallel is 30 seconds
    result <- foreach (movie = 1:length(bi),.combine = c,.packages = c('dplyr','tidyr','foreach')) %dopar%{ 
      i <- names(bi)[movie]
      Ij <- train1 %>% filter(movieId==i) %>% arrange(userId) #all data related to this movie
      
      RVec <- Ij$rating #all ratings of this movie
      bibins <-  bibin[Ij$Interval_i,i]
      bus <- bu[as.character(Ij$userId)]
      alphauDevs <- alphau[as.character(Ij$userId)] * Ij$Dev
      
      PRVec <- c() #all factor-predicted ratings of this movie
      for (k in 1:nrow(Ij)){
        PRVec[k] <- (t(q[,i]) %*% (p[,as.character(Ij$userId[k])] + alphap[,as.character(Ij$userId[k])] *  Ij$Dev[k]))[1]
      }
      
      (sum(RVec) - sum(bibins) - sum(bus) - sum(alphauDevs) - sum(PRVec))/(nrow(Ij)+lambda)
    }
    names(result) <- names(bi)
    bi <- result
    RMSEOut$Train.bi.Update[l+1] <- RMSE2(train,predictRating(train,mu,q,p,bi,bibin,bu,alphau,alphap,dateIntervals,tu))
    RMSEOut$Test.bi.Update[l+1] <- RMSE2(test,predictRating(test,mu,q,p,bi,bibin,bu,alphau,alphap,dateIntervals,tu))
    cat("RMSE after updating bi, training:", RMSEOut$Train.bi.Update[l+1], "\t testing:",RMSEOut$Test.bi.Update[l+1], "\n")
    
    
    
    #update bibin. Runtime on 8 core parallel is 3 minutes
    result <- foreach (movie = 1:ncol(bibin),.combine = cbind,.packages = c('dplyr','tidyr','foreach')) %dopar%{ 
      i <- colnames(bibin)[movie]
      allInts <- sort(unique(train1 %>% filter(movieId==i) %>% pull(Interval_i))) #all intervals where this movie was rated
      
      for (tInt in allInts){ #only need to update entry bibin(t,i) if t is within allInts
        Ij <- train1 %>% filter(movieId==i,Interval_i == tInt) %>% arrange(userId)
        
        RVec <- Ij$rating #all ratings of this movie, at this t
        bis <- bi[as.character(Ij$movieId)]
        bus <- bu[as.character(Ij$userId)]
        alphauDevs <- alphau[as.character(Ij$userId)] * Ij$Dev
        
        PRVec <- c() #all factor-predicted ratings of this movie at this interval
        for (k in 1:nrow(Ij)){
          PRVec[k] <- (t(q[,i]) %*% (p[,as.character(Ij$userId[k])] + alphap[,as.character(Ij$userId[k])] *  Ij$Dev[k]))[1]
        }
        
        
        bibin[tInt,i] <- (sum(RVec) - sum(bis) - sum(bus) - sum(alphauDevs) - sum(PRVec))/(nrow(Ij)+lambda)
      }
      
      bibin[,i]
    }
    colnames(result) <- colnames(bibin)
    bibin <- result
    RMSEOut$Train.bibin.Update[l+1] <- RMSE2(train,predictRating(train,mu,q,p,bi,bibin,bu,alphau,alphap,dateIntervals,tu))
    RMSEOut$Test.bibin.Update[l+1] <- RMSE2(test,predictRating(test,mu,q,p,bi,bibin,bu,alphau,alphap,dateIntervals,tu))
    cat("RMSE after updating bibin, training:", RMSEOut$Train.bibin.Update[l+1], "\t testing:",RMSEOut$Test.bibin.Update[l+1], "\n")
    
    #update bu. Runtime on 8 core parallel is 2.39 seconds
    result <- foreach (user = 1:length(bu),.combine = c,.packages = c('dplyr','tidyr','foreach')) %dopar%{ 
      i <- names(bu)[user]
      Ij <- train1 %>% filter(userId==i) %>% arrange(movieId) #all data related to this user
      
      RVec <- Ij$rating #all ratings from this user
      bis <- bi[as.character(Ij$movieId)]
      bibinInds <- match(as.character(Ij$movieId),colnames(bibin)) * nrow(bibin) - (nrow(bibin)-Ij$Interval_i)#retrive relevant bibin elements
      bibins <- bibin[bibinInds]
      alphauDevs <- alphau[as.character(Ij$userId)] * Ij$Dev
      
      PRVec <- c() #all factor-predicted ratings of this user
      for (k in 1:nrow(Ij)){
        PRVec[k] <- (t(q[,as.character(Ij$movieId[k])]) %*% (p[,as.character(Ij$userId[k])] + alphap[,as.character(Ij$userId[k])] *  Ij$Dev[k]))[1]
      }
      
      (sum(RVec) - sum(bis) - sum(bibins) - sum(alphauDevs) - sum(PRVec))/(nrow(Ij)+lambda)
    }
    names(result) <- names(bu)
    bu <- result
    RMSEOut$Train.bu.Update[l+1] <- RMSE2(train,predictRating(train,mu,q,p,bi,bibin,bu,alphau,alphap,dateIntervals,tu))
    RMSEOut$Test.bu.Update[l+1] <- RMSE2(test,predictRating(test,mu,q,p,bi,bibin,bu,alphau,alphap,dateIntervals,tu))
    cat("RMSE after updating bu, training:", RMSEOut$Train.bu.Update[l+1], "\t testing:",RMSEOut$Test.bu.Update[l+1], "\n")
    
    #update alphau. Runtime on 8 core parallel is 1.72 seconds
    result <- foreach (user = 1:length(alphau),.combine = c,.packages = c('dplyr','tidyr','foreach')) %dopar%{ 
      i <- names(alphau)[user]
      Ij <- train1 %>% filter(userId==i) %>% arrange(movieId) #all data related to this user
      
      #everything here is multiplied by Dev
      RVec <- Ij$rating * Ij$Dev 
      bis <- bi[as.character(Ij$movieId)] * Ij$Dev  
      bibinInds <- match(as.character(Ij$movieId),colnames(bibin)) * nrow(bibin) - (nrow(bibin)-Ij$Interval_i)#retrive relevant bibin elements
      bibins <- bibin[bibinInds]* Ij$Dev 
      bus <- bu[as.character(Ij$userId)]* Ij$Dev 
      
      PRVec <- c() #all factor-predicted ratings of this user
      for (k in 1:nrow(Ij)){
        PRVec[k] <- (t(q[,as.character(Ij$movieId[k])]) %*% (p[,as.character(Ij$userId[k])] + alphap[,as.character(Ij$userId[k])] *  Ij$Dev[k]))[1]
      }
      PRVec <- PRVec * Ij$Dev 
      
      
      (sum(RVec) - sum(bis) - sum(bibins) - sum(bus) - sum(PRVec))/(sum(Ij$Dev^2)+lambda)
    }
    names(result) <- names(alphau)
    alphau <- result
    RMSEOut$Train.alphau.Update[l+1] <- RMSE2(train,predictRating(train,mu,q,p,bi,bibin,bu,alphau,alphap,dateIntervals,tu))
    RMSEOut$Test.alphau.Update[l+1] <- RMSE2(test,predictRating(test,mu,q,p,bi,bibin,bu,alphau,alphap,dateIntervals,tu))
    cat("RMSE after updating alphau, training:", RMSEOut$Train.alphau.Update[l+1], "\t testing:",RMSEOut$Test.alphau.Update[l+1], "\n")
    
    
    #update p. Runtime on 8 core parallel is 1.13 seconds
    result <- foreach (user = 1:ncol(p),.combine = cbind,.packages = c('dplyr','tidyr','foreach')) %dopar%{ 
      i <- colnames(p)[user]
      Ij <- train1 %>% filter(userId==i) %>% arrange(movieId) #all data related to this user
      MI <- q[,as.character(Ij$movieId)]
      
      RVec <- Ij$rating 
      bis <- bi[as.character(Ij$movieId)]
      bibinInds <- match(as.character(Ij$movieId),colnames(bibin)) * nrow(bibin) - (nrow(bibin)-Ij$Interval_i)#retrive relevant bibin elements
      bibins <- bibin[bibinInds]
      bus <- bu[as.character(Ij$userId)]
      alphauDevs <- alphau[as.character(Ij$userId)] * Ij$Dev
      alphapDevs <- alphap[,as.character(Ij$userId)] %*% Ij$Dev 
      
      # V <- MI %*% RVec - MI %*% bis - MI %*% bibins - MI %*% bus - MI %*% alphauDevs - MI %*% t(MI) %*% alphapDevs 
      V <- MI %*% (RVec - bis - bibins -  bus -  alphauDevs -t(MI) %*% alphapDevs) 
      A <-  MI %*% t(MI) + lambda * nrow(Ij) * diag(f)
      
      solve(A,tol = 1e-30) %*% V
      
    }
    colnames(result) <- colnames(p)
    p <- result
    RMSEOut$Train.P.Update[l+1] <- RMSE2(train,predictRating(train,mu,q,p,bi,bibin,bu,alphau,alphap,dateIntervals,tu))
    RMSEOut$Test.P.Update[l+1] <- RMSE2(test,predictRating(test,mu,q,p,bi,bibin,bu,alphau,alphap,dateIntervals,tu))
    cat("RMSE after updating p, training:", RMSEOut$Train.P.Update[l+1], "\t testing:",RMSEOut$Test.P.Update[l+1], "\n")
    
    
    # #update alphap. Runtime on 8 core parallel is 0.88 seconds
    # result <- foreach (user = 1:ncol(alphap),.combine = cbind,.packages = c('dplyr','tidyr','foreach')) %dopar%{ 
    #   i <- colnames(p)[user]
    #   Ij <- train1 %>% filter(userId==i) %>% arrange(movieId) #all data related to this user
    #   MI <- q[,as.character(Ij$movieId),drop = F]
    #   
    #   RVec <- Ij$rating 
    #   bis <- bi[as.character(Ij$movieId)]
    #   bibinInds <- match(as.character(Ij$movieId),colnames(bibin)) * nrow(bibin) - (nrow(bibin)-Ij$Interval_i)#retrive relevant bibin elements
    #   bibins <- bibin[bibinInds]
    #   bus <- bu[as.character(Ij$userId)]
    #   alphauDevs <- alphau[as.character(Ij$userId)] * Ij$Dev
    #   
    #   
    #   # V <- MI %*% (RVec * Ij$Dev) - MI %*% (bis*Ij$Dev) - MI %*% (bibins * Ij$Dev) - MI %*% (bus * Ij$Dev) - MI %*% (alphauDevs * Ij$Dev) - MI %*% (t(MI) %*% p[,i] * Ij$Dev)    
    #   V <- MI %*% ((RVec  - bis -bibins  -bus  - alphauDevs - t(MI) %*% p[,i]) * Ij$Dev)
    #   A <-   (MI %*% diag(Ij$Dev,nrow = nrow(Ij))) %*% t(MI %*% diag(Ij$Dev,nrow = nrow(Ij)))   + lambda * diag(f)
    #   
    #   solve(A,tol = 1e-30) %*% V
    #   
    # }
    # colnames(result) <- colnames(alphap)
    # alphap <- result
    # RMSEOut$Train.alphap.Update[l+1] <- RMSE2(train,predictRating(train,mu,q,p,bi,bibin,bu,alphau,alphap,dateIntervals,tu))
    # RMSEOut$Test.alphap.Update[l+1] <- RMSE2(test,predictRating(test,mu,q,p,bi,bibin,bu,alphau,alphap,dateIntervals,tu))
    # cat("RMSE after updating alphap, training:", RMSEOut$Train.alphap.Update[l+1], "\t testing:",RMSEOut$Test.alphap.Update[l+1], "\n")
    
    # update q. Runtime on 8 core parallel is 30.5 seconds
    result <- foreach (movie = 1:ncol(q),.combine = cbind,.packages = c('dplyr','tidyr','foreach')) %dopar%{
      i <- colnames(q)[movie]
      Ij <- train1 %>% filter(movieId==i) %>% arrange(userId) #all data related to this movie
      MI <- p[,as.character(Ij$userId),drop = F]

      RVec <- Ij$rating
      bis <- bi[as.character(Ij$movieId)]
      bibinInds <- match(as.character(Ij$movieId),colnames(bibin)) * nrow(bibin) - (nrow(bibin)-Ij$Interval_i)#retrive relevant bibin elements
      bibins <- bibin[bibinInds]
      bus <- bu[as.character(Ij$userId)]
      alphauDevs <- alphau[as.character(Ij$userId)] * Ij$Dev
      MIt <- MI + (alphap[,as.character(Ij$userId)] %*% diag(Ij$Dev,nrow = nrow(Ij)))



      # V <- MI %*% RVec - MI %*% bis - MI %*% bibins - MI %*% bus - MI %*% alphauDevs - MI %*% t(MI) %*% alphapDevs
      V <- MIt %*% (RVec - bis - bibins -  bus -  alphauDevs)
      A <-  MIt %*% t(MIt) + lambda * nrow(Ij) * diag(f)

      solve(A,tol = 1e-30) %*% V

    }
    colnames(result) <- colnames(q)
    q <- result
    RMSEOut$Train.Q.Update[l+1] <- RMSE2(train,predictRating(train,mu,q,p,bi,bibin,bu,alphau,alphap,dateIntervals,tu))
    RMSEOut$Test.Q.Update[l+1] <- RMSE2(test,predictRating(test,mu,q,p,bi,bibin,bu,alphau,alphap,dateIntervals,tu))
    cat("RMSE after updating q, training:", RMSEOut$Train.Q.Update[l+1], "\t testing:",RMSEOut$Test.Q.Update[l+1], "\n")

  }
  
  return(list(mu = mu,
              q = q,
              p = p,
              bi = bi,
              bibin = bibin,
              bu = bu,
              alphau = alphau,
              alphap = alphap,
              RMSE = RMSEOut))
  
  
}

#predict ratings given the dataset. Result is in a vector of same length as the number of rows of rating data frame
predictRating <- function(rating,mu,q,p,bi,bibin,bu,alphau,alphap,dateIntervals,tu){
  out <- c()
  if (!'Date' %in% colnames(rating)){
    rating$Date <- as.Date(as.POSIXlt(rating$timestamp,origin = "1970-01-01",tz = 'UTC'))
  }
  
  if (!'Interval_i' %in% colnames(rating)){
    rating$Interval_i <- findInterval(rating$Date,dateIntervals) 
  }
  
  if (!'Dev' %in% colnames(rating)){
    rating$Dev <- as.numeric(sign(rating$Date- tu$MeanDate[match(rating$userId,tu$userId)]) *(as.numeric(abs((rating$Date- tu$MeanDate[match(rating$userId,tu$userId)])))^0.4))
  }
  
  rating$userId <- as.character(rating$userId)
  rating$movieId <- as.character(rating$movieId)
  
  
  compRating <- function(obs){
    mu + bi[obs[2]] + bibin[as.numeric(obs[6]),obs[2]] +  bu[obs[1]] + alphau[obs[1]] * as.numeric(obs[7]) + 
      (t(q[,obs[2]]) %*% (p[,obs[1]] + alphap[,obs[1]] * as.numeric(obs[7])))[1]
  }
  apply(rating,1,compRating)
}

#here est_rating is just a vector of ratings correspondign to the first parameter (e.g. output from predictRating())
RMSE2 <- function(rating,est_rating){
  sqrt(mean((rating$rating-est_rating)^2))
}

#################################################################################
