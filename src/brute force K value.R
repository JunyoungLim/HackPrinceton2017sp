# rmsle analysis
best_rsmle <- 1
best_k<- 0

for (k in c(30:200)) {
  
  cur_close <- post2015$close[1]
  post2015$predicted <- cur_close
  for (i in c(1:(nrow(post2015)-1))) {
    cur_close <- cur_close * (1 + (post2015$pred[i]-0.5)/k);
    post2015$predicted[i+1] <- cur_close;
  }
  
  cur_rmsle <- sqrt(mean((log(post2015$predicted+1)-log(post2015$close+1))^2,na.rm=TRUE))
  if (cur_rmsle < best_rsmle) {
    best_k <- k
    best_rsmle <- cur_rmsle
    print(best_k)
    print(best_rsmle)
  }
}

print(best_k)

#GOOGL
# 112
# 0.04047014

# best difference prediction
best_rsmle <- 100000
best_k<- 0

for (k in c(30:200)) {
  
  cur_close <- post2015$close[1]
  post2015$predicted <- cur_close
  for (i in c(1:(nrow(post2015)-1))) {
    cur_close <- cur_close * (1 + (post2015$pred[i]-0.5)/k);
    post2015$predicted[i+1] <- cur_close;
  }
  
  ###
  sum_diff <- 0
  for (i in c(1:(nrow(post2015)-1))) {
    pred_diff <- abs(post2015$predicted[i+1] - post2015$predicted[i]);
    close_diff <- abs(post2015$close[i+1] - post2015$close[i]);
    sum_diff <- sum_diff + (log(pred_diff+1)-log(close_diff+1))^2;
  }
  
  cur_rmsle <- sqrt(sum_diff / (nrow(post2015)-1))
  ###
  
  if (cur_rmsle < best_rsmle) {
    best_k <- k
    best_rsmle <- cur_rmsle
    print(best_k)
    print(best_rsmle)
  }
}

print(best_k)
# 54
# rsmle = 1.010991