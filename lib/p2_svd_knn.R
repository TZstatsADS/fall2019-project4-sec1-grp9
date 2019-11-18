norm_vec <- function(x) {return(sqrt(sum(x^2)))}

find_one_n_nei_mov_id<- function(vec) {
  loc = which.max(vec)
  return(names(vec)[loc])
}

pred_rating_svd_knn <- function(data_train, data_test, q)
{
  ## calculate similarity matrix 
  name <- colnames(q)
  norm_q <- apply(q,2,norm_vec)
  sim_q <- t(q) %*% q
  sim_q_reg <- sim_q / norm_q
  sim_q_reg <- t(t(sim_q_reg) / norm_q)
  colnames(sim_q_reg) <- name
  rownames(sim_q_reg) <- name
  ## knn regession
  n1 <- nrow(data_test)
  pred_test <- rep(0,n1)
  for (i in 1:n1){
    user_id <- data_test$userId[i]
    movie_id <- data_test$movieId[i]
    train <- data_train[data_train$userId == user_id & data_train$movieId != movie_id,]
    movie_train <- train$movieId
    sim_vec <- sim_q_reg[rownames(sim_q_reg) == movie_id, colnames(sim_q_reg) %in% movie_train]
    movie <- find_one_n_nei_mov_id(sim_vec)
    pred_test[i] <- train[train$movieId == movie,][3]
  }
  pred_test <- as.numeric(unlist(pred_test))
  rmse_adj <- RMSE2(data_test, pred_test)
  return(list(pred_adj = pred_adj, rmse_adj = rmse_adj))
}







#################################################

# paper link https://www.cs.uic.edu/~liub/KDD-cup-2007/proceedings/Regular-Paterek.pdf
# 
# y <- t(result$q) %*% result$p
library("pracma")
norm_vec <- function(x) {return(x/Norm(x))}
X <- t(apply(result$q,2,norm_vec))

guassian.kernel <- function(v1, v2=v1){
  exp(2*(as.matrix(v1) %*% t(v2) - 1))
}
# myridge.fit <- function(X,y,lambda) { w= solve((t(X) %% X) +(lambdadiag(dim(X)[2])), (t(X) %*% y)) return(w) }
###################################
#define a function to extract the corresponding predictedrating for the test set.
pred_rating <- t(result$q) %*% result$p # compute all the results 
extract_pred_rating <- function(test_set, pred){
  pred_rating <- pred[as.character(test_set[2]), as.character(test_set[1])]
  return(pred_rating)
}
#extract predicted rating
pred_test_rating <- apply(data_test, 1, extract_pred_rating, pred_rating)
pred_train_rating <- apply(data_train, 1, extract_pred_rating, pred_rating)
###################################
# there is a function: https://www.rdocumentation.org/packages/listdtr/versions/1.0/topics/krr
KernelRidgeReg <- function(X,Y.test,lambda){
  kernel <- guassian.kernel(X)
  design.mat <- cbind(1, kernel)
  I <- rbind(0, cbind(0, kernel))
  M <- crossprod(design.mat) + lambda*I
  # M <-design.mat + lambda*I
  # inverse of M
  M.inv <- solve(M)
  k <- as.matrix(diag(guassian.kernel(cbind(X.train,Y.train))))
  # Removing diag still gives the same MSE, but will output a vector of prediction.
  Labels <- rbind(0,as.matrix(Y.train))
  y.hat <- t(Labels) %*% M.inv %*% rbind(0,k) * Y.test
  y.true <- Y.test
  MSE <-mean((y.hat - y.true)^2)
  return(list(MSE=MSE,y.hat=y.hat))
}

results <- KernelRidgeReg(X, pred_test_rating, .5)
