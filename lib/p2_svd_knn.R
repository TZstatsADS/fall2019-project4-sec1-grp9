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
